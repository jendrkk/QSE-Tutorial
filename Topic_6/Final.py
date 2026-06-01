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

# Ensure you are using the correct metric CRS for Berlin (UTM Zone 33N is EPSG:25833)
METRIC_CRS = "EPSG:25833"
WALKING_SPEED_M_MIN = 5000 / 60  # 5 km/h converted to meters per minute (83.33 m/min)

# ==========================================
# 1. LOAD AND PROJECT THE STREETS & EXTRACT NODES
# ==========================================
print("Loading and projecting street network...")
streets = gpd.read_file("TransportNetworkParts2006/Streets.shp").to_crs(epsg=25833).reset_index(drop=True)

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
blocks_gdf = gpd.read_file("ARSW2015/ARSW2015-toolkit/shapefile/Berlin4matlab.shp")

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
    "Bus": "TransportNetworkParts2006/BusEntrance.shp",
    "Tram": "TransportNetworkParts2006/TramEntrance.shp",
    "SBahn": "TransportNetworkParts2006/SBahnEntrance.shp",
    "UBahn": "TransportNetworkParts2006/UBahnEntrance.shp"
}

stops_files = {
    "Bus": "TransportNetworkParts2006/Bus2006_stops.shp",
    "Tram": "TransportNetworkParts2006/Tram2006_stops.shp",
    "SBahn": "TransportNetworkParts2006/SBahn2006_stops.shp",
    "UBahn": "TransportNetworkParts2006/UBahn2006_stops.shp"
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
    
    # Calculate exact distance in meters
    transfers_df['transfer_distance_m'] = transfers_df.apply(
        lambda row: row['geometry'].distance(row['geom_to']), axis=1
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
    "Bus": "TransportNetworkParts2006/Bus2006_lines.shp",
    "Tram": "TransportNetworkParts2006/Tram2006_lines.shp",
    "SBahn": "TransportNetworkParts2006/SBahn2006_lines.shp",
    "UBahn": "TransportNetworkParts2006/UBahn2006_lines.shp"
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
print(f"Successfully compiled complete multi-modal graph with {len(master_edges_df)} total edges!")

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
# 10. HIGH-PERFORMANCE SCIPY MATRIX SOLVER (WITH ADVANCED PROGRESS PRINTING)
# ==========================================
def compute_travel_time_matrix_scipy(edges_df, centroids, desc=""):
    """
    Computes an N x N bilateral travel time matrix using highly optimized C-level SciPy Dijkstra.
    This reduces pure Python loop times by several orders of magnitude.
    """
    # 1. Map all nodes present in the edge list to unique sequential integers
    all_nodes = pd.concat([edges_df['node_id_from'], edges_df['node_id_to']]).unique()
    node_to_idx = {node: idx for idx, node in enumerate(all_nodes)}
    num_nodes = len(all_nodes)
    
    # 2. Map edge node IDs to these integer indices
    row_idx = edges_df['node_id_from'].map(node_to_idx).values
    col_idx = edges_df['node_id_to'].map(node_to_idx).values
    weights = edges_df['travel_time_min'].values
    
    # Duplicate for undirected symmetric graph representation
    rows = np.concatenate([row_idx, col_idx])
    cols = np.concatenate([col_idx, row_idx])
    data = np.concatenate([weights, weights])
    
    # 3. Create the CSR Sparse Graph matrix
    csr_graph = csr_matrix((data, (rows, cols)), shape=(num_nodes, num_nodes))
    
    # 4. Isolate the subset of centroids present in this graph component
    valid_centroids = [c for c in centroids if c in node_to_idx]
    centroid_indices = np.array([node_to_idx[c] for c in valid_centroids])
    
    total_centroids = len(centroids)
    # Initialize the output matrix with NaNs (float32 saves 50% RAM compared to float64)
    result_matrix = np.full((total_centroids, total_centroids), np.nan, dtype=np.float32)
    
    # Map valid centroids to their positional row/column indices in the master output matrix
    orig_pos_map = {c: i for i, c in enumerate(centroids)}
    valid_positions = np.array([orig_pos_map[c] for c in valid_centroids])
    
    # Run Dijkstra in batches of origins to maintain a small, fast memory footprint
    batch_size = 100  # Smaller batch size to provide frequent and highly informative updates
    num_valid = len(valid_centroids)
    print(f"\nSolving Dijkstra on {desc} for {num_valid} connected centroids (out of {total_centroids})...")
    
    start_time = time.time()
    for i in range(0, num_valid, batch_size):
        batch_sources = centroid_indices[i : i + batch_size]
        batch_orig_positions = valid_positions[i : i + len(batch_sources)]
        
        # SciPy Dijkstra runs optimized Fibonacci heap sweeps in pre-compiled C loops
        dist_matrix = dijkstra(csr_graph, directed=False, indices=batch_sources)
        
        # Isolate distances back to our valid centroids
        batch_dists = dist_matrix[:, centroid_indices]
        
        # Distribute batch values back to the original matrix coordinates
        for batch_row_idx, orig_row_idx in enumerate(batch_orig_positions):
            result_matrix[orig_row_idx, valid_positions] = batch_dists[batch_row_idx]
            
        # Calculate progress statistics
        processed = min(i + batch_size, num_valid)
        percent = (processed / num_valid) * 100
        elapsed = time.time() - start_time
        speed = processed / elapsed if elapsed > 0 else 0
        eta = (num_valid - processed) / speed if speed > 0 else 0
        
        # Format beautiful console update
        print(f"  [{desc}] Progress: {processed:5d}/{num_valid:5d} ({percent:5.1f}%) | "
              f"Elapsed: {elapsed/60:6.2f}m | ETA: {eta/60:6.2f}m | Speed: {speed:5.1f} origins/sec")
            
    return pd.DataFrame(result_matrix, index=centroids, columns=centroids)

# Compute the matrices for both the full and simplified network topologies
print(f"\n--- Generating Bilateral Travel Time Matrices for all {len(all_centroids)} Centroids ---")

# A. Solve the Full Network Graph
sample_tt_matrix_full = compute_travel_time_matrix_scipy(
    master_edges_df, 
    all_centroids, 
    desc="Full Network (All Modes)"
)

# B. Solve the Simplified Network Graph
sample_tt_matrix_simple = compute_travel_time_matrix_scipy(
    simple_edges_df, 
    all_centroids, 
    desc="Simplified Network (S-Bahn, U-Bahn, Walk Only)"
)

# Save both calculated matrices to disk as CSV files (optimized with precision limits to prevent freezing)
print("\nSaving matrices to CSV files (this may take a few minutes due to the 151M elements)...")
sample_tt_matrix_full.to_csv("sample_travel_time_matrix.csv", float_format="%.2f")
print("Saved 'sample_travel_time_matrix.csv'.")
sample_tt_matrix_simple.to_csv("simplified_travel_time_matrix.csv", float_format="%.2f")
print("Saved 'simplified_travel_time_matrix.csv'.")

# Also save as a highly compact MATLAB .mat file (takes only seconds and is directly loadable in MATLAB!)
try:
    print("Saving highly optimized MATLAB .mat file...")
    sio.savemat("travel_time_matrices.mat", {
        "tt_matrix_full": sample_tt_matrix_full.values,
        "tt_matrix_simple": sample_tt_matrix_simple.values,
        "centroid_ids": list(all_centroids)
    })
    print("Saved 'travel_time_matrices.mat' successfully.")
except Exception as e:
    print(f"Could not save .mat file: {e}")

# ==========================================
# 11. SUBTRACT MATRICES & GENERATE HEATMAP
# ==========================================
print("\n--- Calculating Commute Cost Disadvantage and Plotting ---")

# Difference = Simplified Network Commute Time - Full Network Commute Time
# Positive values represent the exact walk/commute time penalty (minutes) caused by losing buses/trams.
tt_difference = sample_tt_matrix_simple - sample_tt_matrix_full

# Print summary metrics
mean_diff = np.nanmean(tt_difference.values)
max_diff = np.nanmax(tt_difference.values)
print(f"Summary of Disadvantaged Travel Times:")
print(f"  -> Mean Travel Time Loss : {mean_diff:.2f} minutes")
print(f"  -> Maximum Travel Time Loss : {max_diff:.2f} minutes")

# Generate premium, high-resolution heat map visualization
plt.figure(figsize=(10, 8), dpi=300)

# We use YlOrRd colormap: darker red/orange colors represent severe commute disruptions
im = plt.imshow(tt_difference.values, cmap='YlOrRd', interpolation='nearest')

# Configure clean visual styling
cbar = plt.colorbar(im)
cbar.set_label('Travel Time Increase without Bus & Tram (Minutes)', fontsize=12, fontweight='bold', labelpad=10)

plt.title('Commute Impact of Disregarding Bus & Tram Networks\n(Bilateral Travel Time Difference: Simplified - Full)', fontsize=14, fontweight='bold', pad=15)
plt.xlabel('Destination Block Index', fontsize=11, labelpad=8)
plt.ylabel('Origin Block Index', fontsize=11, labelpad=8)

# Disable unnecessary panel borders (spines) for an elegant look
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.gca().spines['left'].set_color('#cccccc')
plt.gca().spines['bottom'].set_color('#cccccc')

plt.tight_layout()
plt.savefig('travel_time_difference_heatmap.png', dpi=300)
print("Successfully generated and saved 'travel_time_difference_heatmap.png'.")

# ==========================================
# 12. GENERATE BERLIN GEOGRAPHIC ACCESSIBILITY ISOCHRONE & DIFFERENCE MAPS
# ==========================================
print("\n--- Generating Geographic Accessibility Isochrone & Difference Maps of Berlin ---")

# 1. Mathematically calculate closeness centrality for each centroid in the full network
# Closeness Centrality is inversely proportional to the mean travel time to all other centroids
mean_times_full = sample_tt_matrix_full.mean(axis=1)

# Find the absolute most central node (minimum mean travel time)
most_central_id = mean_times_full.idxmin()
min_mean_time = mean_times_full.min()

# For a powerful geographical contrast, find the absolute most isolated node (maximum mean travel time)
most_isolated_id = mean_times_full.idxmax()
max_mean_time = mean_times_full.max()

# Keep the first centroid as our third baseline
baseline_id = all_centroids[0]

selected_origins = [baseline_id, most_central_id, most_isolated_id]
origin_labels = [
    f"Baseline Node ({baseline_id})",
    f"Most Central Node ({most_central_id})",
    f"Most Isolated Node ({most_isolated_id})"
]

print(f"Selected Origin Nodes for mapping:")
print(f"  -> Baseline : {baseline_id} (Mean travel time: {mean_times_full.loc[baseline_id]:.2f} mins)")
print(f"  -> Most Central: {most_central_id} (Mean travel time: {min_mean_time:.2f} mins)")
print(f"  -> Most Isolated: {most_isolated_id} (Mean travel time: {max_mean_time:.2f} mins)")

for origin_id, label in zip(selected_origins, origin_labels):
    if origin_id in sample_tt_matrix_full.index and origin_id in sample_tt_matrix_simple.index:
        # -------------------------------------------------------------
        # MAP A: ISOCHRONE ACCESSIBILITY ON THE FULL NETWORK (BLUE TO RED COLORMAP)
        # -------------------------------------------------------------
        print(f"  -> Plotting Full Network Isochrone for {origin_id}...")
        
        # Extract travel times from this origin to all other destinations
        travel_times_full = sample_tt_matrix_full.loc[origin_id].to_frame(name='travel_time_min')
        
        # Join travel times back to the original block polygon geometries
        berlin_map_gdf = blocks_gdf.merge(travel_times_full, left_on='centroid_id', right_index=True)
        
        # Locate the coordinates of the origin centroid
        origin_geom = centroids_gdf[centroids_gdf['centroid_id'] == origin_id].geometry.values[0]

        fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
        
        # Draw base map background (gray) to handle missing values
        blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')

        # Draw accessibility isochrones (from Blue [low travel time] to Red [high travel time])
        # 'coolwarm' colormap maps low times to blue, middle times to gray/white, and high times to red.
        berlin_map_gdf.plot(
            column='travel_time_min',
            cmap='coolwarm',
            legend=True,
            legend_kwds={
                'label': 'Travel Time from Origin (Minutes)', 
                'orientation': 'horizontal', 
                'pad': 0.05, 
                'shrink': 0.7
            },
            ax=ax,
            edgecolor='none',
            missing_kwds={'color': '#eaeaea'}
        )

        # Plot origin centroid as a prominent red star
        ax.scatter(origin_geom.x, origin_geom.y, color='red', edgecolor='white', s=180, marker='*', zorder=5, label='Origin Point')
        
        # Overlay streets (semitransparent white grid)
        streets.plot(ax=ax, color='#ffffff', linewidth=0.1, alpha=0.3, zorder=2)

        ax.set_title(f'Berlin Isochrone Map: Travel Times originating from:\n{label} (Full Network)', fontsize=14, fontweight='bold', pad=15)
        ax.set_axis_off()
        ax.legend(loc='upper left', frameon=True, facecolor='white', edgecolor='none')
        plt.tight_layout()
        
        output_isochrone = f'berlin_isochrone_{origin_id}.png'
        plt.savefig(output_isochrone, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"     Saved: '{output_isochrone}'")

        # -------------------------------------------------------------
        # MAP B: ACCESSIBILITY DIFFERENCE MAP (SIMPLIFIED - FULL) FROM THE SAME ORIGIN
        # -------------------------------------------------------------
        print(f"  -> Plotting Accessibility Difference Map for {origin_id}...")
        
        # Extract difference times from this origin: Simplified Network - Full Network
        travel_times_diff = (sample_tt_matrix_simple.loc[origin_id] - sample_tt_matrix_full.loc[origin_id]).to_frame(name='diff_min')
        
        # Join difference values back to the original block polygon geometries
        berlin_diff_gdf = blocks_gdf.merge(travel_times_diff, left_on='centroid_id', right_index=True)

        fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
        
        # Draw base map background (gray)
        blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')

        # Draw difference isochrones. We want to show where the travel times increased.
        # Blue to red color scheme:
        # Blue/Cool: Little to no commute time increase (e.g. 0 minutes)
        # Red/Warm: Significant travel time penalties (e.g. 20+ minutes) due to losing buses and trams.
        # 'coolwarm' colormap works perfectly here as well!
        berlin_diff_gdf.plot(
            column='diff_min',
            cmap='coolwarm',
            legend=True,
            legend_kwds={
                'label': 'Increase in Commute Time without Bus & Tram (Minutes)', 
                'orientation': 'horizontal', 
                'pad': 0.05, 
                'shrink': 0.7
            },
            ax=ax,
            edgecolor='none',
            missing_kwds={'color': '#eaeaea'}
        )

        # Plot origin centroid
        ax.scatter(origin_geom.x, origin_geom.y, color='red', edgecolor='white', s=180, marker='*', zorder=5, label='Origin Point')
        
        # Overlay streets
        streets.plot(ax=ax, color='#ffffff', linewidth=0.1, alpha=0.3, zorder=2)

        ax.set_title(f'Bilateral Accessibility Difference from:\n{label}\n(Simplified Network [S-Bahn/U-Bahn Only] - Full Network)', fontsize=14, fontweight='bold', pad=15)
        ax.set_axis_off()
        ax.legend(loc='upper left', frameon=True, facecolor='white', edgecolor='none')
        plt.tight_layout()
        
        output_diff = f'berlin_diff_{origin_id}.png'
        plt.savefig(output_diff, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"     Saved: '{output_diff}'")

print("\nAll geographic visualizations completed successfully!")