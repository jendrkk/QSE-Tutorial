import geopandas as gpd

OLD_PATH = 'TransportNetworkParts2006/'

for name, filename in [
    ("Lines", 'UBahn2006_lines.shp'),
    ("Entrance", 'UBahnEntrance.shp'),
    ("Stops", 'UBahn2006_stops.shp')
]:
    print(f"\n--- {name} ({filename}) ---")
    gdf = gpd.read_file(OLD_PATH + filename)
    print("CRS:", gdf.crs)
    print("Columns:", gdf.columns.tolist())
    print("Head:\n", gdf.head(2))
