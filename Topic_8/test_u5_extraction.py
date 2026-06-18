import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, LineString

def extract_u5_network(PATH):
    print("Loading GTFS text files...")
    routes = pd.read_csv(f"{PATH}routes.txt", dtype=str)
    trips = pd.read_csv(f"{PATH}trips.txt", dtype=str)
    shapes = pd.read_csv(f"{PATH}shapes.txt", dtype={'shape_id': str})
    stops = pd.read_csv(f"{PATH}stops.txt", dtype=str)
    stop_times = pd.read_csv(f"{PATH}stop_times.txt", dtype=str)

    # --- 1. FIND THE U5 LINE ---
    print("Extracting U5 Line...")
    u5_route_ids = routes[(routes['route_short_name'].str.strip() == 'U5') & (routes['route_type'] == '400')]['route_id'].tolist()
    print("U5 Route IDs (Subway only):", u5_route_ids)
    u5_trips = trips[trips['route_id'].isin(u5_route_ids)]
    print("Number of U5 trips:", len(u5_trips))
    
    u5_shape_ids = u5_trips['shape_id'].dropna().unique()
    print("U5 Shape IDs:", u5_shape_ids)
    u5_shapes = shapes[shapes['shape_id'].isin(u5_shape_ids)].sort_values(by=['shape_id', 'shape_pt_sequence'])
    print("Number of shape points:", len(u5_shapes))
    
    if len(u5_shapes) > 0:
        u5_shapes['geometry'] = u5_shapes.apply(lambda row: Point(row['shape_pt_lon'], row['shape_pt_lat']), axis=1)
        lines = u5_shapes.groupby('shape_id')['geometry'].apply(lambda x: LineString(x.tolist()) if len(x) >= 2 else Point(x.iloc[0])).reset_index()
        lines_gdf = gpd.GeoDataFrame(lines, geometry='geometry', crs="EPSG:4326")
        print("Lines GDF:")
        print(lines_gdf.head())
        # Try to save to file
        lines_gdf.to_file(PATH + "U5_Line_test.shp")
    else:
        print("No shape points found for U5!")

    # --- 2. FIND THE U5 PLATFORMS ---
    print("\nExtracting U5 Platforms...")
    u5_trip_ids = u5_trips['trip_id'].tolist()
    u5_platform_ids = stop_times[stop_times['trip_id'].isin(u5_trip_ids)]['stop_id'].unique()
    print("Number of U5 platform IDs:", len(u5_platform_ids))
    
    u5_platforms = stops[stops['stop_id'].isin(u5_platform_ids)].copy()
    print("Number of U5 platforms:", len(u5_platforms))
    
    if len(u5_platforms) > 0:
        u5_platforms['geometry'] = u5_platforms.apply(
            lambda row: Point(float(row['stop_lon']), float(row['stop_lat'])), axis=1
        )
        platforms_gdf = gpd.GeoDataFrame(u5_platforms, geometry='geometry', crs="EPSG:4326")
        print("Platforms GDF:")
        print(platforms_gdf.head())
        platforms_gdf.to_file(PATH + "U5_Platforms_test.shp")

    # --- 3. FIND THE U5 ENTRANCES ---
    print("\nExtracting U5 Station Entrances...")
    u5_parent_stations = u5_platforms['parent_station'].dropna().unique()
    print("Number of U5 parent stations:", len(u5_parent_stations))
    
    u5_entrances = stops[
        (stops['parent_station'].isin(u5_parent_stations)) & 
        (stops['location_type'].astype(str).str.strip() == '2')
    ].copy()
    print("Number of U5 entrances found:", len(u5_entrances))
    
    if not u5_entrances.empty:
        u5_entrances['geometry'] = u5_entrances.apply(
            lambda row: Point(float(row['stop_lon']), float(row['stop_lat'])), axis=1
        )
        entrances_gdf = gpd.GeoDataFrame(u5_entrances, geometry='geometry', crs="EPSG:4326")
        entrances_gdf.to_file(PATH + "U5_Entrances_test.shp")
        print(f"Found {len(u5_entrances)} entrances.")
    else:
        print("No entrances found.")

extract_u5_network('VBB_2026/')
