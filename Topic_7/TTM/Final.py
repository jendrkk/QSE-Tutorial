import geopandas as gpd
import pandas as pd
import numpy as np
import networkx as nx
from shapely.geometry import Point
import matplotlib.pyplot as plt
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import dijkstra
import scipy.io as sio
import time
import igraph as ig
from joblib import Parallel, delayed
import os
from pathlib import Path

# ---------------------------------------------------------------------------
# Path resolution: works regardless of the working directory at invocation.
# SCRIPT_DIR  -> .../QSE-Tutorial/Topic_7/
# REPO_ROOT   -> .../QSE-Tutorial/
# ---------------------------------------------------------------------------
SCRIPT_DIR     = Path(__file__).resolve().parent
REPO_ROOT      = SCRIPT_DIR.parent
TRANSPORT_DIR  = REPO_ROOT / "Data" / "Shapefiles-2022" / "Berlin" / "TransportNetworkParts2006"
ARSW_DIR       = REPO_ROOT / "ARSW2015" / "ARSW2015-toolkit" / "shapefile"

# Ensure outputs land next to this script
os.chdir(SCRIPT_DIR)

# Ensure you are using the correct metric CRS for Berlin (UTM Zone 33N is EPSG:25833)
METRIC_CRS = "EPSG:25833"
WALKING_SPEED_M_MIN = 5000 / 60  # 5 km/h converted to meters per minute (83.33 m/min)

# ==========================================
# 1. LOAD AND PROJECT THE STREETS & EXTRACT NODES
# ==========================================
print("Loading and projecting street network...")
streets = gpd.read_file(TRANSPORT_DIR / "Streets.shp").to_crs(epsg=25833).reset_index(drop=True)

# Extract unique street nodes (junctions)
start_points = streets.geometry.apply(lambda line: Point(line.coords[0]))
end_points = streets.geometry.apply(lambda line: Point(line.coords[-1]))
all_points = pd.concat([start_points, end_points])

# We reset index to guarantee unique sequence IDs for the street nodes
street_nodes = gpd.GeoDataFrame(geometry=all_points, crs=streets.crs).drop_duplicates(subset='geometry').reset_index(drop=True)
street_nodes['node_id'] = [f"street_node_{i}" for i in range(len(street_nodes))]

# ==========================================
# 2. COMPUTE AND SNAP BLOCK CENTROIDS TO STREETS
# ==========================================
print("Processing and snapping block centroids...")
# Load raw shapefile in original CRS
blocks_gdf = gpd.read_file(ARSW_DIR / "Berlin4matlab.shp")

# Clean null or empty geometries in original CRS first
blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)

# Helper function to find any geometries with corrupt coordinates (NaN or Inf bounds)
def is_valid_coordinate_geom(geom):
    try:
        b = geom.bounds
        if len(b) != 4:
            return False
        return not (np.any(np.isnan(b)) or np.any(np.isinf(b)))
    except Exception:
        return False

# Drop corrupt coordinate geometries
blocks_gdf = blocks_gdf[blocks_gdf.geometry.apply(is_valid_coordinate_geom)].reset_index(drop=True)

# Repair geometry topologies safely in original CRS
blocks_gdf['geometry'] = blocks_gdf.geometry.make_valid()

# Project to metric CRS
blocks_gdf = blocks_gdf.to_crs(epsg=25833)

# Filter again to drop any nulls, empties, or corrupt geometries introduced by coordinate reprojection
blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)
blocks_gdf = blocks_gdf[blocks_gdf.geometry.apply(is_valid_coordinate_geom)].reset_index(drop=True)

# Assign centroid_id directly to original block polygons so we can map results geographically later
blocks_gdf['centroid_id'] = [f"centroid_{i}" for i in range(len(blocks_gdf))]

centroids = blocks_gdf.geometry.centroid

centroids_gdf = gpd.GeoDataFrame(
    blocks_gdf.drop(columns=['geometry']),
    geometry=centroids,
    crs=blocks_gdf.crs
)

# Snap centroids to the nearest street node (Street-Walk Connector)
snapped_centroids = gpd.sjoin_nearest(
    centroids_gdf,
    street_nodes,
    how="left",
    distance_col="distance_to_node"
)
# In case of equidistant matches, keep only the first connection
snapped_centroids = snapped_centroids.groupby('centroid_id').first().reset_index()
snapped_centroids['travel_time_min'] = snapped_centroids['distance_to_node'] / WALKING_SPEED_M_MIN

# ==========================================
# 3. LOAD, SNAP ENTRANCES & CONNECT TO PLATFORMS
# ==========================================
print("Processing and snapping transit entrances and platform stops...")

# Define filepaths for both Entrances and Stops/Platforms
entrances_files = {
    "Bus":   TRANSPORT_DIR / "BusEntrance.shp",
    "Tram":  TRANSPORT_DIR / "TramEntrance.shp",
    "SBahn": TRANSPORT_DIR / "SBahnEntrance.shp",
    "UBahn": TRANSPORT_DIR / "UBahnEntrance.shp",
}

stops_files = {
    "Bus":   TRANSPORT_DIR / "Bus2006_stops.shp",
    "Tram":  TRANSPORT_DIR / "Tram2006_stops.shp",
    "SBahn": TRANSPORT_DIR / "SBahn2006_stops.shp",
    "UBahn": TRANSPORT_DIR / "UBahn2006_stops.shp",
}

all_snapped_entrances = []
all_entrance_to_platform_edges = []
all_stops_list = []

for mode in entrances_files.keys():
    try:
        # A. Load and project Entrances and Stops for this mode
        ent_gdf = gpd.read_file(entrances_files[mode]).to_crs(epsg=25833).reset_index(drop=True)
        ent_gdf['mode'] = mode
        ent_gdf['entrance_id'] = [f"{mode}_entrance_{i}" for i in range(len(ent_gdf))]
        
        stop_gdf = gpd.read_file(stops_files[mode]).to_crs(epsg=25833).reset_index(drop=True)
        stop_gdf['mode'] = mode
        stop_gdf['stop_id'] = [f"{mode}_stop_{i}" for i in range(len(stop_gdf))]
        all_stops_list.append(stop_gdf)
        
        # B. Connect Entrance to Street Grid (Street -> Entrance Edge)
        snapped_ent = gpd.sjoin_nearest(
            ent_gdf,
            street_nodes,
            how="left",
            distance_col="distance_to_street_node"
        )
        snapped_ent = snapped_ent.groupby('entrance_id').first().reset_index()
        snapped_ent['walk_time_min'] = snapped_ent['distance_to_street_node'] / WALKING_SPEED_M_MIN
        all_snapped_entrances.append(snapped_ent)
        
        # C. Connect Entrance to Platforms/Stops (Entrance -> Platform/Stop Edge)
        # Match each entrance to the nearest platform of the SAME mode
        entrance_to_stop = gpd.sjoin_nearest(
            ent_gdf,
            stop_gdf,
            how="left",
            distance_col="distance_to_platform"
        )
        entrance_to_stop = entrance_to_stop.groupby('entrance_id').first().reset_index()
        
        # Cap distance at 150m to resolve digitization offsets in the GIS layers
        MAX_STATION_WALK_M = 150.0
        entrance_to_stop['distance_to_platform_clean'] = entrance_to_stop['distance_to_platform'].clip(upper=MAX_STATION_WALK_M)
        
        # Pure physical walk time from entrance to platform (capped)
        entrance_to_stop['transition_time_min'] = entrance_to_stop['distance_to_platform_clean'] / WALKING_SPEED_M_MIN
        all_entrance_to_platform_edges.append(entrance_to_stop)
        
        print(f"Successfully processed {mode}: Connected {len(ent_gdf)} entrances to {len(stop_gdf)} platform stops.")
        
    except Exception as e:
        print(f"Error processing {mode} network assets: {e}")

# Combine results into master DataFrames
all_entrances_df = pd.concat(all_snapped_entrances, ignore_index=True) if all_snapped_entrances else pd.DataFrame()
all_entrance_to_platform_df = pd.concat(all_entrance_to_platform_edges, ignore_index=True) if all_entrance_to_platform_edges else pd.DataFrame()
all_stops_gdf = gpd.GeoDataFrame(pd.concat(all_stops_list, ignore_index=True), crs=METRIC_CRS) if all_stops_list else gpd.GeoDataFrame()

# ==========================================
# 4. GENERATE PLATFORM-TO-PLATFORM TRANSFER EDGES
# ==========================================
print("Generating platform-to-platform transfer edges (within 500m)...")

if not all_stops_gdf.empty:
    # Set up left and right representations to calculate spatial proximity transfers
    stops_left = all_stops_gdf[['stop_id', 'mode', 'geometry']].rename(
        columns={'stop_id': 'stop_id_from', 'mode': 'mode_from'}
    )
    stops_right_buffered = all_stops_gdf[['stop_id', 'mode', 'geometry']].copy()
    
    # Buffer right stops by 500m to find all neighbor platforms within walking distance
    stops_right_buffered['geometry'] = stops_right_buffered.geometry.buffer(500)
    stops_right_buffered = stops_right_buffered.rename(
        columns={'stop_id': 'stop_id_to', 'mode': 'mode_to'}
    )
    
    # Perform spatial join to find stops within the 500m buffer
    transfers_df = gpd.sjoin(stops_left, stops_right_buffered, how="inner", predicate="within")
    
    # Filter out self-transfers (connecting a stop to itself)
    transfers_df = transfers_df[transfers_df['stop_id_from'] != transfers_df['stop_id_to']]
    
    # Map the exact coordinate geometry of the destination stop to calculate actual Euclidean distance
    stop_geom_dict = all_stops_gdf.set_index('stop_id')['geometry'].to_dict()
    transfers_df['geom_to'] = transfers_df['stop_id_to'].map(stop_geom_dict)
    
    # Calculate exact distance in meters (vectorized)
    transfers_df['transfer_distance_m'] = gpd.GeoSeries(transfers_df['geometry'], crs=METRIC_CRS).distance(
        gpd.GeoSeries(transfers_df['geom_to'], crs=METRIC_CRS)
    )
    
    # Calculate transfer travel time: 3 min penalty + (distance / walk speed)
    transfers_df['transfer_time_min'] = 3.0 + (transfers_df['transfer_distance_m'] / WALKING_SPEED_M_MIN)
    print(f"Generated {len(transfers_df)} valid platform-to-platform transfer edges.")
else:
    transfers_df = pd.DataFrame()

# ==========================================
# 5. GENERATE STREET-TO-STREET WALKING EDGES
# ==========================================
print("Generating street walk edges...")
# We match start and end coordinates of street segments to coordinate-mapped node IDs
streets['length_m'] = streets.geometry.length

# Create a fast dictionary-based mapping from coordinates to street node IDs
coord_to_street_node = {
    (round(geom.x, 3), round(geom.y, 3)): node_id
    for node_id, geom in zip(street_nodes['node_id'], street_nodes['geometry'])
}

def get_node_from_coord(pt):
    coord = (round(pt.x, 3), round(pt.y, 3))
    return coord_to_street_node.get(coord, None)

# Find unique node endpoints for each street segment
streets['node_id_from'] = streets.geometry.apply(lambda line: get_node_from_coord(Point(line.coords[0])))
streets['node_id_to'] = streets.geometry.apply(lambda line: get_node_from_coord(Point(line.coords[-1])))

# Build clean street walking edges
street_edges = streets.dropna(subset=['node_id_from', 'node_id_to']).copy()
street_edges['travel_time_min'] = street_edges['length_m'] / WALKING_SPEED_M_MIN
print(f"Generated {len(street_edges)} physical street walk edges.")

# ==========================================
# 6. GENERATE TRANSIT VEHICLE LINE EDGES
# ==========================================
print("Generating vehicle-based transit line edges...")

transit_lines_files = {
    "Bus":   TRANSPORT_DIR / "Bus2006_lines.shp",
    "Tram":  TRANSPORT_DIR / "Tram2006_lines.shp",
    "SBahn": TRANSPORT_DIR / "SBahn2006_lines.shp",
    "UBahn": TRANSPORT_DIR / "UBahn2006_lines.shp",
}

# Travel speed parameters from the paper (km/h)
vehicle_speeds_kmh = {
    "Bus": 14.3,
    "Tram": 14.5,
    "SBahn": 25.0,
    "UBahn": 25.0
}

all_transit_edges_list = []

for mode, filepath in transit_lines_files.items():
    try:
        # Load and project transit lines
        lines_gdf = gpd.read_file(filepath).to_crs(epsg=25833)
        # Fix: Reset index to guarantee clean unique row sequence 
        lines_gdf = lines_gdf.reset_index(drop=True)
        lines_gdf['length_m'] = lines_gdf.geometry.length
        
        # Isolate stops specifically for this mode
        stop_gdf = all_stops_gdf[all_stops_gdf['mode'] == mode].copy()
        
        if stop_gdf.empty:
            continue
            
        # Extract starting and ending coordinates of each track line segment
        line_starts = lines_gdf.geometry.apply(lambda line: Point(line.coords[0]))
        line_ends = lines_gdf.geometry.apply(lambda line: Point(line.coords[-1]))
        
        starts_gdf = gpd.GeoDataFrame(geometry=line_starts, crs=lines_gdf.crs)
        ends_gdf = gpd.GeoDataFrame(geometry=line_ends, crs=lines_gdf.crs)
        
        # Snaps track segments to the nearest platforms of the same mode
        matched_starts = gpd.sjoin_nearest(starts_gdf, stop_gdf, how="left", distance_col="start_dist")
        matched_ends = gpd.sjoin_nearest(ends_gdf, stop_gdf, how="left", distance_col="end_dist")
        
        # Fix: Resolve equidistant matching ties (duplicate indices) from sjoin_nearest 
        # by keeping the first match per original line segment index.
        matched_starts = matched_starts.groupby(matched_starts.index).first()
        matched_ends = matched_ends.groupby(matched_ends.index).first()
        
        # Create transit connection structure
        edges_df = pd.DataFrame({
            'node_id_from': matched_starts['stop_id'],
            'node_id_to': matched_ends['stop_id'],
            'length_m': lines_gdf['length_m'],
            'start_dist': matched_starts['start_dist'],
            'end_dist': matched_ends['end_dist']
        })
        
        # Drop snappings that exceed 150m to avoid incorrect snappings over distance gaps
        MAX_SNAP_DIST = 150.0
        edges_df = edges_df[
            (edges_df['start_dist'] <= MAX_SNAP_DIST) & 
            (edges_df['end_dist'] <= MAX_SNAP_DIST)
        ].copy()
        
        # Exclude self-loop segments starting and ending at the exact same platform
        edges_df = edges_df[edges_df['node_id_from'] != edges_df['node_id_to']]
        
        # Calculate vehicle travel time based on distance and mode speed: t = distance / speed
        speed_m_min = (vehicle_speeds_kmh[mode] * 1000) / 60
        edges_df['travel_time_min'] = edges_df['length_m'] / speed_m_min
        edges_df['edge_type'] = f'{mode}_line'
        
        all_transit_edges_list.append(edges_df)
        print(f"Generated {len(edges_df)} active transit track edges for {mode}.")
        
    except Exception as e:
        print(f"Error processing {mode} transit lines: {e}")

all_transit_edges_df = pd.concat(all_transit_edges_list, ignore_index=True) if all_transit_edges_list else pd.DataFrame()

# ==========================================
# 7. COMPILE MASTER NETWORK EDGE LIST
# ==========================================
print("\nCompiling full network Master Edges Dataset...")

master_edges_list = []

# A. Block Centroid to Street Junctions (Walking access connectors)
master_edges_list.append(pd.DataFrame({
    'node_id_from': snapped_centroids['centroid_id'],
    'node_id_to': snapped_centroids['node_id'],
    'travel_time_min': snapped_centroids['travel_time_min'],
    'edge_type': 'centroid_to_street'
}))

# B. Street Junction to Street Junction (Street network geometry walks)
master_edges_list.append(pd.DataFrame({
    'node_id_from': street_edges['node_id_from'],
    'node_id_to': street_edges['node_id_to'],
    'travel_time_min': street_edges['travel_time_min'],
    'edge_type': 'street_walk'
}))

# C. Street Junction to Station Entrance (Entrance walking connectors)
master_edges_list.append(pd.DataFrame({
    'node_id_from': all_entrances_df['node_id'],
    'node_id_to': all_entrances_df['entrance_id'],
    'travel_time_min': all_entrances_df['walk_time_min'],
    'edge_type': 'street_to_entrance'
}))

# D. Station Entrance to Station Platform (Inside station walks)
master_edges_list.append(pd.DataFrame({
    'node_id_from': all_entrance_to_platform_df['entrance_id'],
    'node_id_to': all_entrance_to_platform_df['stop_id'],
    'travel_time_min': all_entrance_to_platform_df['transition_time_min'],
    'edge_type': 'entrance_to_platform'
}))

# E. Station Platform to Station Platform (Walking transfer edges with 3-minute penalties)
if not transfers_df.empty:
    master_edges_list.append(pd.DataFrame({
        'node_id_from': transfers_df['stop_id_from'],
        'node_id_to': transfers_df['stop_id_to'],
        'travel_time_min': transfers_df['transfer_time_min'],
        'edge_type': 'platform_transfer'
    }))

# F. Consecutive Transit Platforms (Track line travel)
if not all_transit_edges_df.empty:
    master_edges_list.append(pd.DataFrame({
        'node_id_from': all_transit_edges_df['node_id_from'],
        'node_id_to': all_transit_edges_df['node_id_to'],
        'travel_time_min': all_transit_edges_df['travel_time_min'],
        'edge_type': all_transit_edges_df['edge_type']
    }))

# Assemble and verify final network structure
master_edges_df = pd.concat(master_edges_list, ignore_index=True)

# OPTIMIZATION & BUG FIX: Handle duplicate edges by keeping the minimum travel time
# Since the graph is undirected, we first sort node IDs to handle (A,B) and (B,A) identically
print(f"Consolidating {len(master_edges_df)} edges (handling duplicates and undirected symmetry)...")
m_df = master_edges_df.copy()
nodes = np.sort(m_df[['node_id_from', 'node_id_to']].values, axis=1)
m_df['node_id_from'] = nodes[:, 0]
m_df['node_id_to'] = nodes[:, 1]

master_edges_df = m_df.groupby(['node_id_from', 'node_id_to']).agg({
    'travel_time_min': 'min',
    'edge_type': 'first'
}).reset_index()

print(f"Successfully compiled complete multi-modal graph with {len(master_edges_df)} unique undirected edges!")

# ==========================================
# 8. CONSTRUCT NETWORKX GRAPHS (FOR SINGLE-PATH TRACING)
# ==========================================
print("\nBuilding Undirected NetworkX Graphs...")

# A. Build the FULL multi-modal transport graph in NetworkX
G_full = nx.Graph()
for idx, row in master_edges_df.iterrows():
    G_full.add_edge(
        row['node_id_from'],
        row['node_id_to'],
        weight=row['travel_time_min'],
        edge_type=row['edge_type']
    )

# B. Build the SIMPLIFIED graph (disregarding Bus and Tram components entirely)
# We filter out any edge referencing Bus or Tram inside the nodes or types.
simple_edges_df = master_edges_df[
    (~master_edges_df['node_id_from'].astype(str).str.contains('Bus|Tram', case=False)) &
    (~master_edges_df['node_id_to'].astype(str).str.contains('Bus|Tram', case=False)) &
    (~master_edges_df['edge_type'].astype(str).str.contains('Bus|Tram', case=False))
].copy()

G_simple = nx.Graph()
for idx, row in simple_edges_df.iterrows():
    G_simple.add_edge(
        row['node_id_from'],
        row['node_id_to'],
        weight=row['travel_time_min'],
        edge_type=row['edge_type']
    )

print(f"Graph Construction Complete:")
print(f"  -> G_full  : {G_full.number_of_nodes()} nodes, {G_full.number_of_edges()} edges")
print(f"  -> G_simple: {G_simple.number_of_nodes()} nodes, {G_simple.number_of_edges()} edges")

# ==========================================
# 9. RUN DIJKSTRA PATH ROUTING EXAMPLES (NETWORKX)
# ==========================================
print("\n--- Running Dijkstra Shortest Path Example (Trace) ---")

# Let's find two block centroids that have a valid connected path
all_centroids = centroids_gdf['centroid_id'].unique()

found_valid_path = False
source_test = all_centroids[0]

# Search for a reachable target to demonstrate a multi-modal journey trace
for target_candidate in all_centroids[1:100]:
    if nx.has_path(G_full, source_test, target_candidate):
        target_test = target_candidate
        found_valid_path = True
        break

if found_valid_path:
    # 1. Compute shortest path sequence and total travel time using Dijkstra
    path_sequence = nx.shortest_path(G_full, source=source_test, target=target_test, weight='weight')
    total_time = nx.shortest_path_length(G_full, source=source_test, target=target_test, weight='weight')
    
    print(f"Fastest Route from {source_test} to {target_test}:")
    print(f"Total Combined Travel Time: {total_time:.2f} minutes\n")
    print("Itinerary Breakdown:")
    
    # 2. Print a step-by-step breakdown explaining each segment type
    for i in range(len(path_sequence) - 1):
        u, v = path_sequence[i], path_sequence[i+1]
        edge_data = G_full[u][v]
        segment_time = edge_data['weight']
        segment_type = edge_data['edge_type']
        print(f"  Step {i+1:02d}: {u} -> {v} | Type: {segment_type:<22} | Cost: {segment_time:5.2f} mins")
else:
    print("Could not locate a connected pair in the sample search window.")

# ==========================================
# 10. HIGH-PERFORMANCE IGRAPH MATRIX SOLVER (PARALLELIZED)
# ==========================================
def compute_travel_time_matrix_igraph(edges_df, centroids, desc=""):
    """
    Computes an N x N bilateral travel time matrix using highly optimized C-level IGraph Dijkstra.
    This version uses multiple CPU cores to solve different origin batches in parallel.
    """
    # 1. Map all nodes present in the edge list to unique sequential integers
    all_nodes = pd.concat([edges_df['node_id_from'], edges_df['node_id_to']]).unique()
    node_to_idx = {node: idx for idx, node in enumerate(all_nodes)}
    
    # 2. Build the IGraph structure
    g = ig.Graph(directed=False)
    g.add_vertices(len(all_nodes))
    g.vs["name"] = all_nodes
    
    # Map edges to integer vertex indices
    edge_list = list(zip(
        edges_df['node_id_from'].map(node_to_idx),
        edges_df['node_id_to'].map(node_to_idx)
    ))
    g.add_edges(edge_list)
    g.es["weight"] = edges_df['travel_time_min'].values
    
    # 3. Identify indices of centroids to solve for
    valid_centroids = [c for c in centroids if c in node_to_idx]
    centroid_indices = np.array([node_to_idx[c] for c in valid_centroids])
    
    total_centroids = len(centroids)
    result_matrix = np.full((total_centroids, total_centroids), np.nan, dtype=np.float32)
    
    # Map valid centroids back to their positions in the master output matrix
    orig_pos_map = {c: i for i, c in enumerate(centroids)}
    valid_positions = np.array([orig_pos_map[c] for c in valid_centroids])
    
    batch_size = 500  # Efficient batch size for parallelization
    num_valid = len(valid_centroids)
    print(f"\nSolving Dijkstra on {desc} using parallelized IGraph for {num_valid} connected centroids...")
    
    start_time = time.time()
    
    def solve_batch(i):
        batch_end = min(i + batch_size, num_valid)
        batch_sources = centroid_indices[i : batch_end]
        # ig.Graph.distances is extremely fast (C-level Dijkstra)
        return g.distances(source=batch_sources, target=centroid_indices, weights="weight")

    # Run batches in parallel using all available CPU cores
    # We use joblib which handles worker pooling and process management
    results = Parallel(n_jobs=-1, batch_size=1)(
        delayed(solve_batch)(i) for i in range(0, num_valid, batch_size)
    )
    
    # Assemble the results back into the master matrix
    curr_idx = 0
    for batch_results in results:
        num_in_batch = len(batch_results)
        batch_orig_positions = valid_positions[curr_idx : curr_idx + num_in_batch]
        
        for b_idx, orig_row_idx in enumerate(batch_orig_positions):
            result_matrix[orig_row_idx, valid_positions] = batch_results[b_idx]
            
        curr_idx += num_in_batch
        
    elapsed = time.time() - start_time
    print(f"  [{desc}] Completed in {elapsed/60:.2f}m | Speed: {num_valid/elapsed:.1f} origins/sec")
            
    return pd.DataFrame(result_matrix, index=centroids, columns=centroids)

# ==========================================
# 10. SEQUENTIAL MATRIX COMPUTATION & PERSISTENCE
# ==========================================
import gc

# Define scenarios to process sequentially
scenarios = [
    {
        "name": "Full Network (All Modes)",
        "edges": master_edges_df,
        "parquet": "sample_travel_time_matrix.parquet",
        "mat_key": "tt_matrix_full"
    },
    {
        "name": "Simplified Network (S-Bahn, U-Bahn, Walk Only)",
        "edges": simple_edges_df,
        "parquet": "simplified_travel_time_matrix.parquet",
        "mat_key": "tt_matrix_simple"
    }
]

print(f"\n--- Generating Bilateral Travel Time Matrices for all {len(all_centroids)} Centroids ---")
final_results_for_mat = {"centroid_ids": list(all_centroids)}

for scenario in scenarios:
    # 1. Compute Matrix
    matrix_df = compute_travel_time_matrix_igraph(
        scenario["edges"], 
        all_centroids, 
        desc=scenario["name"]
    )

    # 2. Save to Parquet immediately (Fast & RAM-efficient)
    print(f"Saving to {scenario['parquet']}...")
    matrix_df.to_parquet(scenario["parquet"], engine='pyarrow')

    # 3. Store in dict for final .mat saving (optional, but requested)
    # To keep RAM truly low, we store the underlying numpy array
    final_results_for_mat[scenario["mat_key"]] = matrix_df.values

    # 4. Cleanup
    del matrix_df
    gc.collect()
    print(f"Completed and cleared RAM for {scenario['name']}.")

# Also save as a highly compact MATLAB .mat file
try:
    print("\nSaving highly optimized MATLAB .mat file...")
    sio.savemat("travel_time_matrices.mat", final_results_for_mat)
    print("Saved 'travel_time_matrices.mat' successfully.")
except Exception as e:
    print(f"Could not save .mat file: {e}")

print("\nMatrix computation and saving completed successfully!")
print("You can now run 'python plots.py' to generate the visualizations.")