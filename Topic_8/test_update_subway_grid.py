import geopandas as gpd
import pandas as pd

def update_subway_grid(new_line_path, old_grid_path, new_grid_path):
    print(f'Loading: {new_line_path} and {old_grid_path}')
    new_u5 = gpd.read_file(new_line_path)
    new_u5 = new_u5.rename(columns={'shape_id': 'Id'})
    
    old_grid = gpd.read_file(old_grid_path)

    print("Merging the new U5 directly into the legacy network...")
    
    max_id = old_grid['Id'].max() if 'Id' in old_grid.columns else 0
    if pd.isna(max_id): max_id = 0
    new_u5['Id'] = range(int(max_id) + 1, int(max_id) + 1 + len(new_u5))
    
    if new_u5.crs != old_grid.crs:
        print(f"Aligning CRS: Converting new data from {new_u5.crs} to {old_grid.crs}")
        new_u5 = new_u5.to_crs(old_grid.crs)
    
    # Concatenate
    merged_gdf = pd.concat([old_grid, new_u5], ignore_index=True)

    print(f"Saving updated network to {new_grid_path}...")
    merged_gdf.to_file(new_grid_path)
    print("Done!\n")

OLD_PATH = 'TransportNetworkParts2006/' 
VBB_PATH = 'VBB_2026/'
NEW_PATH = 'ExtendedUBahnNetwork/'

update_subway_grid(VBB_PATH + 'U5_Line_test.shp', OLD_PATH + 'UBahn2006_lines.shp', NEW_PATH + 'UBahn_lines_test.shp')
update_subway_grid(VBB_PATH + 'U5_Entrances_test.shp', OLD_PATH + 'UBahnEntrance.shp', NEW_PATH + 'UBahn_Entrances_test.shp')
update_subway_grid(VBB_PATH + 'U5_Platforms_test.shp', OLD_PATH + 'UBahn2006_stops.shp', NEW_PATH + 'UBahn_stops_test.shp')
