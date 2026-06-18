import network_builder as nb
import TT_calculator as re

# 1. DEFINE YOUR PATHS
# Notice how the UBahn paths point to your newly merged files!
OLD_DIR = "TransportNetworkParts2006/"
NEW_DIR = "ExtendedUBahnNetwork/"

streets_file = OLD_DIR + "Streets.shp"
blocks_file  = "Blocks/Berlin4matlab.shp"

entrances = {
    "Bus":   OLD_DIR + "BusEntrance.shp",
    "Tram":  OLD_DIR + "TramEntrance.shp",
    "SBahn": OLD_DIR + "SBahnEntrance.shp",
    "UBahn": NEW_DIR + "UBahn_Entrances_test.shp"  # Updated U5 Network!
}

stops = {
    "Bus":   OLD_DIR + "Bus2006_stops.shp",
    "Tram":  OLD_DIR + "Tram2006_stops.shp",
    "SBahn": OLD_DIR + "SBahn2006_stops.shp",
    "UBahn": NEW_DIR + "UBahn_stops_test.shp"      # Updated U5 Network!
}

lines = {
    "Bus":   OLD_DIR + "Bus2006_lines.shp",
    "Tram":  OLD_DIR + "Tram2006_lines.shp",
    "SBahn": OLD_DIR + "SBahn2006_lines.shp",
    "UBahn": NEW_DIR + "UBahn_lines_test.shp"      # Updated U5 Network!
}

speeds = {"Bus": 14.3, "Tram": 14.5, "SBahn": 25.0, "UBahn": 25.0}

# 2. BUILD THE NETWORK
print("Building street network...")
street_nodes, street_edges = nb.load_street_network(streets_file)

print("Snapping centroids...")
centroids_gdf, snapped_centroids = nb.snap_centroids_to_streets(blocks_file, street_nodes)

print("Processing transit nodes and platform transfers...")
ent_df, ent_to_plat_df, stops_gdf = nb.process_transit_nodes(entrances, stops, street_nodes)
transfers_df = nb.generate_platform_transfers(stops_gdf)

print("Processing transit track lines...")
transit_edges_df = nb.generate_transit_lines(lines, stops_gdf, speeds)

print("Compiling Master Edge list...")
master_edges = nb.compile_master_graph(
    snapped_centroids, street_edges, ent_df, ent_to_plat_df, transfers_df, transit_edges_df
)
# 3. ROUTE AND SOLVE

print("\nBuilding NetworkX representation...")
G_full = re.build_networkx_graph(master_edges)

all_centroids = centroids_gdf['centroid_id'].tolist()

# Trace a quick test path
print("Tracing a test route...")
re.trace_dijkstra_path(G_full, all_centroids[0], all_centroids[50])

# Run Matrix Solver and save to Parquet
print("\nSolving Full TTM Matrix...")
ttm_df = re.compute_travel_time_matrix(master_edges, all_centroids, n_jobs=2, batch_size=500)
ttm_df.to_parquet("updated_u5_travel_time_matrix_test.parquet")
print("Done! Matrix saved.")
