import geopandas as gpd
import pandas as pd
import numpy as np
from shapely.geometry import Point

WALKING_SPEED_M_MIN = 5000 / 60  # 83.33 m/min

def load_street_network(streets_shp, crs="EPSG:25833"):
    """Loads streets, extracts nodes, and generates street-to-street walking edges."""
    streets = gpd.read_file(streets_shp).to_crs(crs).reset_index(drop=True)
    streets['length_m'] = streets.geometry.length
    
    start_points = streets.geometry.apply(lambda line: Point(line.coords[0]))
    end_points = streets.geometry.apply(lambda line: Point(line.coords[-1]))
    all_points = pd.concat([start_points, end_points])
    
    street_nodes = gpd.GeoDataFrame(geometry=all_points, crs=crs).drop_duplicates(subset='geometry').reset_index(drop=True)
    street_nodes['node_id'] = [f"street_node_{i}" for i in range(len(street_nodes))]
    
    # Map coordinates to node IDs to build edges
    coord_to_street_node = {
        (round(geom.x, 3), round(geom.y, 3)): node_id
        for node_id, geom in zip(street_nodes['node_id'], street_nodes['geometry'])
    }
    
    def get_node(pt):
        return coord_to_street_node.get((round(pt.x, 3), round(pt.y, 3)), None)

    streets['node_id_from'] = streets.geometry.apply(lambda line: get_node(Point(line.coords[0])))
    streets['node_id_to'] = streets.geometry.apply(lambda line: get_node(Point(line.coords[-1])))
    
    street_edges = streets.dropna(subset=['node_id_from', 'node_id_to']).copy()
    street_edges['travel_time_min'] = street_edges['length_m'] / WALKING_SPEED_M_MIN
    
    return street_nodes, street_edges

def snap_centroids_to_streets(blocks_shp, street_nodes, crs="EPSG:25833"):
    """Loads blocks, cleans geometries, and snaps centroids to the street grid."""
    blocks_gdf = gpd.read_file(blocks_shp)
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)
    blocks_gdf['geometry'] = blocks_gdf.geometry.make_valid()
    blocks_gdf = blocks_gdf.to_crs(crs)
    blocks_gdf['centroid_id'] = [f"centroid_{i}" for i in range(len(blocks_gdf))]
    
    centroids_gdf = gpd.GeoDataFrame(
        blocks_gdf.drop(columns=['geometry']), 
        geometry=blocks_gdf.geometry.centroid, 
        crs=crs
    )
    
    snapped = gpd.sjoin_nearest(centroids_gdf, street_nodes, how="left", distance_col="distance_to_node")
    snapped = snapped.groupby('centroid_id').first().reset_index()
    snapped['travel_time_min'] = snapped['distance_to_node'] / WALKING_SPEED_M_MIN
    
    return centroids_gdf, snapped

def process_transit_nodes(entrances_files, stops_files, street_nodes, crs="EPSG:25833"):
    """Processes entrances/stops for all modes and generates walk/transition edges."""
    all_entrances, all_ent_to_plat, all_stops = [], [], []
    
    for mode in entrances_files.keys():
        ent_gdf = gpd.read_file(entrances_files[mode]).to_crs(crs).reset_index(drop=True)
        ent_gdf['mode'] = mode
        ent_gdf['entrance_id'] = [f"{mode}_entrance_{i}" for i in range(len(ent_gdf))]
        
        stop_gdf = gpd.read_file(stops_files[mode]).to_crs(crs).reset_index(drop=True)
        stop_gdf['mode'] = mode
        stop_gdf['stop_id'] = [f"{mode}_stop_{i}" for i in range(len(stop_gdf))]
        all_stops.append(stop_gdf)
        
        # Snap entrance to street
        snapped_ent = gpd.sjoin_nearest(ent_gdf, street_nodes, how="left", distance_col="distance_to_street")
        snapped_ent = snapped_ent.groupby('entrance_id').first().reset_index()
        snapped_ent['walk_time_min'] = snapped_ent['distance_to_street'] / WALKING_SPEED_M_MIN
        all_entrances.append(snapped_ent)
        
        # Snap entrance to platform
        ent_to_stop = gpd.sjoin_nearest(ent_gdf, stop_gdf, how="left", distance_col="distance_to_platform")
        ent_to_stop = ent_to_stop.groupby('entrance_id').first().reset_index()
        ent_to_stop['transition_time_min'] = ent_to_stop['distance_to_platform'].clip(upper=150.0) / WALKING_SPEED_M_MIN
        all_ent_to_plat.append(ent_to_stop)

    return pd.concat(all_entrances, ignore_index=True), \
           pd.concat(all_ent_to_plat, ignore_index=True), \
           gpd.GeoDataFrame(pd.concat(all_stops, ignore_index=True), crs=crs)

def generate_platform_transfers(stops_gdf, crs="EPSG:25833", buffer_m=500):
    """Generates walk transfer edges between platforms within a certain distance."""
    stops_left = stops_gdf[['stop_id', 'mode', 'geometry']].rename(columns={'stop_id': 'stop_id_from', 'mode': 'mode_from'})
    stops_right = stops_gdf[['stop_id', 'mode', 'geometry']].copy()
    stops_right['geometry'] = stops_right.geometry.buffer(buffer_m)
    stops_right = stops_right.rename(columns={'stop_id': 'stop_id_to', 'mode': 'mode_to'})
    
    transfers = gpd.sjoin(stops_left, stops_right, how="inner", predicate="within")
    transfers = transfers[transfers['stop_id_from'] != transfers['stop_id_to']].copy()
    
    stop_geom_dict = stops_gdf.set_index('stop_id')['geometry'].to_dict()
    transfers['geom_to'] = transfers['stop_id_to'].map(stop_geom_dict)
    
    transfers['transfer_dist'] = gpd.GeoSeries(transfers['geometry'], crs=crs).distance(
        gpd.GeoSeries(transfers['geom_to'], crs=crs)
    )
    transfers['transfer_time_min'] = 3.0 + (transfers['transfer_dist'] / WALKING_SPEED_M_MIN)
    return transfers

def generate_transit_lines(lines_files, stops_gdf, vehicle_speeds_kmh, crs="EPSG:25833"):
    """Snaps transit track lines to platforms to create travel edges."""
    all_transit_edges = []
    
    for mode, filepath in lines_files.items():
        lines_gdf = gpd.read_file(filepath).to_crs(crs).reset_index(drop=True)
        lines_gdf['length_m'] = lines_gdf.geometry.length
        
        mode_stops = stops_gdf[stops_gdf['mode'] == mode].copy()
        if mode_stops.empty: continue
            
        starts = gpd.GeoDataFrame(geometry=lines_gdf.geometry.apply(lambda l: Point(l.coords[0])), crs=crs)
        ends = gpd.GeoDataFrame(geometry=lines_gdf.geometry.apply(lambda l: Point(l.coords[-1])), crs=crs)
        
        matched_starts = gpd.sjoin_nearest(starts, mode_stops, how="left", distance_col="start_dist").groupby(level=0).first()
        matched_ends = gpd.sjoin_nearest(ends, mode_stops, how="left", distance_col="end_dist").groupby(level=0).first()
        
        edges = pd.DataFrame({
            'node_id_from': matched_starts['stop_id'],
            'node_id_to': matched_ends['stop_id'],
            'length_m': lines_gdf['length_m'],
            'start_dist': matched_starts['start_dist'],
            'end_dist': matched_ends['end_dist']
        })
        
        edges = edges[(edges['start_dist'] <= 150) & (edges['end_dist'] <= 150) & (edges['node_id_from'] != edges['node_id_to'])].copy()
        speed_m_min = (vehicle_speeds_kmh[mode] * 1000) / 60
        edges['travel_time_min'] = edges['length_m'] / speed_m_min
        edges['edge_type'] = f'{mode}_line'
        
        all_transit_edges.append(edges)
        
    return pd.concat(all_transit_edges, ignore_index=True)

def compile_master_graph(snapped_centroids, street_edges, entrances_df, ent_to_plat_df, transfers_df, transit_edges_df):
    """Assembles all dataframes into a unified undirected edge list."""
    edges = [
        pd.DataFrame({'node_id_from': snapped_centroids['centroid_id'], 'node_id_to': snapped_centroids['node_id'], 'travel_time_min': snapped_centroids['travel_time_min'], 'edge_type': 'centroid_to_street'}),
        pd.DataFrame({'node_id_from': street_edges['node_id_from'], 'node_id_to': street_edges['node_id_to'], 'travel_time_min': street_edges['travel_time_min'], 'edge_type': 'street_walk'}),
        pd.DataFrame({'node_id_from': entrances_df['node_id'], 'node_id_to': entrances_df['entrance_id'], 'travel_time_min': entrances_df['walk_time_min'], 'edge_type': 'street_to_entrance'}),
        pd.DataFrame({'node_id_from': ent_to_plat_df['entrance_id'], 'node_id_to': ent_to_plat_df['stop_id'], 'travel_time_min': ent_to_plat_df['transition_time_min'], 'edge_type': 'entrance_to_platform'})
    ]
    
    if not transfers_df.empty:
        edges.append(pd.DataFrame({'node_id_from': transfers_df['stop_id_from'], 'node_id_to': transfers_df['stop_id_to'], 'travel_time_min': transfers_df['transfer_time_min'], 'edge_type': 'platform_transfer'}))
    if not transit_edges_df.empty:
        edges.append(pd.DataFrame({'node_id_from': transit_edges_df['node_id_from'], 'node_id_to': transit_edges_df['node_id_to'], 'travel_time_min': transit_edges_df['travel_time_min'], 'edge_type': transit_edges_df['edge_type']}))

    master_df = pd.concat(edges, ignore_index=True)
    
    # --- BUG FIX START ---
    # 1. Drop any orphan edges where snapping failed (leaving NaN/None)
    master_df = master_df.dropna(subset=['node_id_from', 'node_id_to']).copy()
    
    # 2. Force all node IDs to standard strings so NumPy can sort them safely
    master_df['node_id_from'] = master_df['node_id_from'].astype(str)
    master_df['node_id_to'] = master_df['node_id_to'].astype(str)
    # --- BUG FIX END ---

    # Handle undirected duplicates by sorting nodes alphabetically
    nodes = np.sort(master_df[['node_id_from', 'node_id_to']].values, axis=1)
    master_df['node_id_from'] = nodes[:, 0]
    master_df['node_id_to'] = nodes[:, 1]
    
    return master_df.groupby(['node_id_from', 'node_id_to']).agg({
        'travel_time_min': 'min',
        'edge_type': 'first'
    }).reset_index()