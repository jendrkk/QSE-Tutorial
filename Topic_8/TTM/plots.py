import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
from pathlib import Path
from scipy.spatial.distance import cdist

# ---------------------------------------------------------------------------
# Path resolution: relative to the script location
# SCRIPT_DIR  -> .../Topic_8/TTM/
# REPO_ROOT   -> .../Topic_8/
# ---------------------------------------------------------------------------
SCRIPT_DIR     = Path(__file__).resolve().parent
REPO_ROOT      = SCRIPT_DIR.parent

# Ensure outputs are written to the TTM folder
os.chdir(SCRIPT_DIR)

# Configuration
METRIC_CRS     = "EPSG:25833"
BLOCKS_SHP     = REPO_ROOT / "Blocks" / "Berlin4matlab.shp"
STREETS_SHP    = REPO_ROOT / "TransportNetworkParts2006" / "Streets.shp"
GREEN_SHP      = REPO_ROOT / "Blocks" / "BerlinGreen.shp"
WATER_SHP      = REPO_ROOT / "Blocks" / "BerlinWater.shp"
DISTRICTS_SHP  = REPO_ROOT / "Blocks" / "Bezirke23.shp"
UBAHN_LINES_SHP = REPO_ROOT / "ExtendedUBahnNetwork" / "UBahn_lines.shp"
UBAHN_STOPS_SHP = REPO_ROOT / "ExtendedUBahnNetwork" / "UBahn_stops.shp"
SBAHN_LINES_SHP = REPO_ROOT / "TransportNetworkParts2006" / "SBahn2006_lines.shp"
PARQUET_FILE   = REPO_ROOT / "updated_u5_travel_time_matrix.parquet"
LEGACY_PARQUET_FILE = REPO_ROOT / "legacy_travel_time_matrix.parquet"

def load_geometries():
    print("Loading geometries...")
    # Load blocks
    blocks_gdf = gpd.read_file(BLOCKS_SHP)
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)
    blocks_gdf['geometry'] = blocks_gdf.geometry.make_valid()
    blocks_gdf = blocks_gdf.to_crs(METRIC_CRS)
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)
    blocks_gdf['centroid_id'] = [f"centroid_{i}" for i in range(len(blocks_gdf))]
    
    # Load streets for overlay
    print("Loading street network...")
    streets = gpd.read_file(STREETS_SHP).to_crs(METRIC_CRS)
    
    # Load greenery and water
    print("Loading greenery and water...")
    green = gpd.read_file(GREEN_SHP).to_crs(METRIC_CRS)
    water = gpd.read_file(WATER_SHP).to_crs(METRIC_CRS)
    
    # Load districts
    print("Loading district boundaries...")
    districts = gpd.read_file(DISTRICTS_SHP).to_crs(METRIC_CRS)
    
    # Load U-Bahn network
    print("Loading U-Bahn network...")
    ubahn_lines = gpd.read_file(UBAHN_LINES_SHP).to_crs(METRIC_CRS)
    ubahn_stops = gpd.read_file(UBAHN_STOPS_SHP).to_crs(METRIC_CRS)
    
    # Load S-Bahn network
    print("Loading S-Bahn network...")
    sbahn_lines = gpd.read_file(SBAHN_LINES_SHP).to_crs(METRIC_CRS)
    
    return blocks_gdf, streets, green, water, districts, ubahn_lines, ubahn_stops, sbahn_lines

def load_matrix(filepath):
    print(f"Reading travel time matrix: {filepath}...")
    if not filepath.exists():
        raise FileNotFoundError(f"Travel time matrix parquet file not found at {filepath}.")
    df = pd.read_parquet(filepath)
    return {
        'full': df.values,
        'ids': df.index.tolist()
    }

def get_robust_limits(data, p_low=2, p_high=98):
    clean_data = data[~np.isnan(data) & ~np.isinf(data)]
    if len(clean_data) == 0:
        return 0.0, 100.0
    return np.percentile(clean_data, p_low), np.percentile(clean_data, p_high)

def plot_average_travel_times(matrix_data, blocks_gdf, green, water, districts, cmap='coolwarm', suffix='coolwarm'):
    print(f"Plotting average travel times ({cmap})...")
    tt_matrix = matrix_data['full']
    centroid_ids = matrix_data['ids']
    
    avg_times = np.nanmean(tt_matrix, axis=1)
    avg_times_df = pd.DataFrame({
        'centroid_id': centroid_ids,
        'avg_travel_time': avg_times
    })
    
    map_gdf = blocks_gdf.merge(avg_times_df, on='centroid_id')
    
    # Robust limits
    vmin, vmax = get_robust_limits(avg_times, 2, 98)
    print(f"  -> Times range: {np.nanmin(avg_times):.1f} to {np.nanmax(avg_times):.1f} min. Scaling colorbar to: {vmin:.1f} - {vmax:.1f} min.")
    
    fig, ax = plt.subplots(figsize=(10, 5.625), dpi=300)
    
    # Base background blocks
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    
    # Greenery & Water layers
    green.plot(ax=ax, color='#d2f1d2', edgecolor='none', alpha=0.8, zorder=1)
    water.plot(ax=ax, color='#aed6f1', edgecolor='none', alpha=0.8, zorder=1)
    
    # Districts layer
    districts.plot(ax=ax, color='none', edgecolor='#b0bec5', linewidth=0.5, linestyle='--', zorder=2)
    
    # Average travel time blocks
    map_gdf.plot(
        column='avg_travel_time',
        cmap=cmap,
        legend=True,
        legend_kwds={
            'label': 'Average Travel Time to All Other Centroids (Minutes)',
            'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7
        },
        ax=ax,
        edgecolor='none',
        vmin=vmin,
        vmax=vmax,
        zorder=3
    )
    
    ax.set_title('Average Travel Time\n(Updated Network - Robust Scale)', fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    
    plt.tight_layout()
    filename = f'berlin_average_travel_times_{suffix}.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"Saved '{filename}'")

def plot_ubahn_network(blocks_gdf, green, water, districts, ubahn_lines, ubahn_stops):
    print("Plotting U-Bahn network map (highlighting U5)...")
    
    try:
        old_lines = gpd.read_file(REPO_ROOT / "TransportNetworkParts2006" / "UBahn2006_lines.shp")
        max_line_id = old_lines['Id'].max()
        if pd.isna(max_line_id): max_line_id = 0
    except Exception:
        max_line_id = 0
        
    try:
        old_stops = gpd.read_file(REPO_ROOT / "TransportNetworkParts2006" / "UBahn2006_stops.shp")
        max_stop_id = old_stops['Id'].max()
        if pd.isna(max_stop_id): max_stop_id = 0
    except Exception:
        max_stop_id = 0

    fig, ax = plt.subplots(figsize=(10, 5.625), dpi=300)
    
    blocks_gdf.plot(ax=ax, color='#f7f7f7', edgecolor='none')
    green.plot(ax=ax, color='#d2f1d2', edgecolor='none', alpha=0.8, zorder=1)
    water.plot(ax=ax, color='#aed6f1', edgecolor='none', alpha=0.8, zorder=1)
    districts.plot(ax=ax, color='none', edgecolor='#b0bec5', linewidth=0.8, linestyle='--', zorder=2)
    
    legacy_lines = ubahn_lines[ubahn_lines['Id'] <= max_line_id]
    new_lines_candidate = ubahn_lines[ubahn_lines['Id'] > max_line_id]
    
    legacy_stops = ubahn_stops[ubahn_stops['Id'] <= max_stop_id]
    new_stops_candidate = ubahn_stops[ubahn_stops['Id'] > max_stop_id]
    
    # Filter new_lines and new_stops to only include the actual extension (west of Alexanderplatz)
    new_lines_4326 = new_lines_candidate.to_crs("EPSG:4326")
    new_lines = new_lines_candidate[new_lines_4326.geometry.centroid.x < 13.411]
    
    new_stops_4326 = new_stops_candidate.to_crs("EPSG:4326")
    new_stops = new_stops_candidate[new_stops_4326.geometry.x < 13.411]

    
    if not legacy_lines.empty:
        legacy_lines.plot(ax=ax, color='#1565c0', linewidth=1.8, alpha=0.8, zorder=3, label='Legacy U-Bahn Network')
    if not legacy_stops.empty:
        legacy_stops.plot(ax=ax, color='#0d47a1', markersize=5, alpha=0.8, zorder=4, label='Legacy Stations')
        
    if not new_lines.empty:
        new_lines.plot(ax=ax, color='#ff6d00', linewidth=2.8, alpha=1.0, zorder=5, label='new U5')
    if not new_stops.empty:
        new_stops.plot(ax=ax, color='#d84315', markersize=12, marker='o', edgecolor='white', linewidth=0.5, alpha=1.0, zorder=6, label='new U5 stations')
        
    ax.set_title('Berlin U-Bahn Network\n(Highlighting the U5 Subway Line)', fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    plt.legend(loc='lower left', frameon=True, facecolor='white', edgecolor='#cfd8dc')
    
    plt.tight_layout()
    plt.savefig('berlin_ubahn_network_u5.png', dpi=300, bbox_inches='tight')
    plt.close()
    print("Saved 'berlin_ubahn_network_u5.png'")

def plot_travel_time_savings(new_matrix_data, legacy_matrix_data, blocks_gdf, green, water, districts, ubahn_lines, sbahn_lines):
    print("Plotting travel time savings...")
    new_t = np.nanmean(new_matrix_data['full'], axis=1)
    legacy_t = np.nanmean(legacy_matrix_data['full'], axis=1)
    
    # Clip travel time savings to strictly non-negative values to remove numerical/snapping noise
    time_savings = np.maximum(legacy_t - new_t, 0.0)
    
    print(f"  -> Travel time savings range: {np.nanmin(time_savings):.2f} to {np.nanmax(time_savings):.2f} minutes")
    
    df = pd.DataFrame({
        'centroid_id': new_matrix_data['ids'],
        'time_savings': time_savings
    })
    
    map_gdf = blocks_gdf.merge(df, on='centroid_id')
    
    fig, ax = plt.subplots(figsize=(10, 5.625), dpi=300)
    
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    green.plot(ax=ax, color='#d2f1d2', edgecolor='none', alpha=0.8, zorder=1)
    water.plot(ax=ax, color='#aed6f1', edgecolor='none', alpha=0.8, zorder=1)
    districts.plot(ax=ax, color='none', edgecolor='#b0bec5', linewidth=0.5, linestyle='--', zorder=2)
    
    # Filter to only show positive savings (> 0.5 minutes) to highlight impact zones
    impact_gdf = map_gdf[map_gdf['time_savings'] > 0.5]
    
    if not impact_gdf.empty:
        # Scale color scale based on the 98th percentile of positive savings for clarity
        vmax = np.percentile(impact_gdf['time_savings'], 98)
        impact_gdf.plot(
            column='time_savings',
            cmap='YlOrRd',
            legend=True,
            legend_kwds={
                'label': 'Average Travel Time Saved\n(In average minutes need to reach all other nodes)',
                'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7
            },
            ax=ax,
            edgecolor='none',
            vmin=0.0,
            vmax=max(vmax, 1.0),
            zorder=3
        )
    else:
        print("  -> Warning: No significant travel time savings found to plot.")
        
    # Overlay S-Bahn network (forest green) & U-Bahn network (blue)
    sbahn_lines.plot(ax=ax, color='#1b5e20', linewidth=1.2, alpha=0.8, zorder=4, label='S-Bahn Network')
    ubahn_lines.plot(ax=ax, color='#1565c0', linewidth=1.2, alpha=0.8, zorder=5, label='U-Bahn Network')
    
    ax.set_title("Travel Time Saved for U5 Extension\n (Average time needed to reach all other Centriods in minutes)", fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    plt.legend(loc='lower left', frameon=True, facecolor='white', edgecolor='#cfd8dc')
    
    plt.tight_layout()
    plt.savefig('berlin_travel_time_savings.png', dpi=300, bbox_inches='tight')
    plt.close()
    print("Saved 'berlin_travel_time_savings.png'")
def plot_travel_speeds(new_matrix_data, legacy_matrix_data, blocks_gdf, green, water, districts, ubahn_lines, sbahn_lines):
    print("Calculating travel speeds...")
    centroid_ids = new_matrix_data['ids']
    blocks_ordered = blocks_gdf.set_index('centroid_id').loc[centroid_ids].reset_index()
    
    # Coordinates in projected meters (EPSG:25833)
    coords = np.array([[g.x, g.y] for g in blocks_ordered.geometry.centroid])
    
    print("Computing Euclidean distance matrix...")
    dist_matrix = cdist(coords, coords, metric='euclidean').astype(np.float32)
    
    # Calculate speeds: speed (km/h) = (dist_m / time_min) * 0.06
    # Clip between 0 and 100 km/h to handle self-travel or snaps
    with np.errstate(divide='ignore', invalid='ignore'):
        v_new_mat = np.clip((dist_matrix / new_matrix_data['full']) * 0.06, 0.0, 100.0)
        v_legacy_mat = np.clip((dist_matrix / legacy_matrix_data['full']) * 0.06, 0.0, 100.0)
        
    avg_v_new = np.nanmean(v_new_mat, axis=1)
    avg_v_legacy = np.nanmean(v_legacy_mat, axis=1)
    speed_gains = avg_v_new - avg_v_legacy
    
    print(f"  -> Average speed range (Updated): {np.nanmin(avg_v_new):.2f} to {np.nanmax(avg_v_new):.2f} km/h")
    print(f"  -> Speed gains range: {np.nanmin(speed_gains):.2f} to {np.nanmax(speed_gains):.2f} km/h")
    
    # Plot 1: Average Travel Speeds (Updated)
    df_speed = pd.DataFrame({'centroid_id': centroid_ids, 'speed': avg_v_new})
    map_gdf = blocks_gdf.merge(df_speed, on='centroid_id')
    
    # Robust limits for absolute speed
    vmin_v, vmax_v = get_robust_limits(avg_v_new, 2, 98)
    
    fig, ax = plt.subplots(figsize=(10, 5.625), dpi=300)
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    green.plot(ax=ax, color='#d2f1d2', edgecolor='none', alpha=0.8, zorder=1)
    water.plot(ax=ax, color='#aed6f1', edgecolor='none', alpha=0.8, zorder=1)
    districts.plot(ax=ax, color='none', edgecolor='#b0bec5', linewidth=0.5, linestyle='--', zorder=2)
    
    map_gdf.plot(
        column='speed',
        cmap='magma',
        legend=True,
        legend_kwds={
            'label': 'Average Journey Speed (km/h)',
            'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7
        },
        ax=ax,
        edgecolor='none',
        vmin=vmin_v,
        vmax=vmax_v,
        zorder=3
    )
    # Overlay S-Bahn network (forest green) & U-Bahn network (blue)
    sbahn_lines.plot(ax=ax, color='#1b5e20', linewidth=1.2, alpha=0.8, zorder=4, label='S-Bahn Network')
    ubahn_lines.plot(ax=ax, color='#1565c0', linewidth=1.2, alpha=0.8, zorder=5, label='U-Bahn Network')
    
    ax.set_title('Average Journey Speed\n(Updated Network - Robust Scale)', fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    plt.legend(loc='lower left', frameon=True, facecolor='white', edgecolor='#cfd8dc')
    plt.tight_layout()
    plt.savefig('berlin_average_travel_speeds.png', dpi=300, bbox_inches='tight')
    plt.close()
    print("Saved 'berlin_average_travel_speeds.png'")
    
    # Plot 2: Journey Speed Gains (Difference Map)
    df_gains = pd.DataFrame({'centroid_id': centroid_ids, 'gains': speed_gains})
    map_gdf_gains = blocks_gdf.merge(df_gains, on='centroid_id')
    
    # Filter to only show positive gains (> 0.02 km/h) to highlight impact zones
    impact_gdf = map_gdf_gains[map_gdf_gains['gains'] > 0.02]
    
    fig, ax = plt.subplots(figsize=(10, 5.625), dpi=300)
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    green.plot(ax=ax, color='#d2f1d2', edgecolor='none', alpha=0.8, zorder=1)
    water.plot(ax=ax, color='#aed6f1', edgecolor='none', alpha=0.8, zorder=1)
    districts.plot(ax=ax, color='none', edgecolor='#b0bec5', linewidth=0.5, linestyle='--', zorder=2)
    
    if not impact_gdf.empty:
        # Scale color based on 98th percentile of positive gains
        vmax_g = np.percentile(impact_gdf['gains'], 98)
        impact_gdf.plot(
            column='gains',
            cmap='Oranges',
            legend=True,
            legend_kwds={
                'label': 'Journey Speed Gain (km/h)',
                'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7
            },
            ax=ax,
            edgecolor='none',
            vmin=0.0,
            vmax=max(vmax_g, 0.5),
            zorder=3
        )
    else:
        print("  -> Warning: No significant speed gains found to plot.")
        
    # Overlay S-Bahn network (forest green) & U-Bahn network (blue)
    sbahn_lines.plot(ax=ax, color='#1b5e20', linewidth=1.2, alpha=0.8, zorder=4, label='S-Bahn Network')
    ubahn_lines.plot(ax=ax, color='#1565c0', linewidth=1.2, alpha=0.8, zorder=5, label='U-Bahn Network')
    
    ax.set_title('Journey Speed Gains from U5 extension (km/h)', fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    plt.legend(loc='lower left', frameon=True, facecolor='white', edgecolor='#cfd8dc')
    plt.tight_layout()
    plt.savefig('berlin_journey_speed_gains.png', dpi=300, bbox_inches='tight')
    plt.close()
    print("Saved 'berlin_journey_speed_gains.png'")

if __name__ == "__main__":
    try:
        print("Starting plotting routine...")
        blocks_gdf, streets, green, water, districts, ubahn_lines, ubahn_stops, sbahn_lines = load_geometries()
        
        # Load updated (new U5) matrix
        print("\nLoading updated TTM (with U5)...")
        new_matrix_data = load_matrix(PARQUET_FILE)
        
        # Plot absolute travel times (coolwarm version)
        plot_average_travel_times(new_matrix_data, blocks_gdf, green, water, districts, cmap='coolwarm', suffix='coolwarm')
        
        # Plot absolute travel times (viridis version)
        plot_average_travel_times(new_matrix_data, blocks_gdf, green, water, districts, cmap='viridis_r', suffix='viridis')
        
        # Plot U-Bahn network highlighting the U5 line (with scaled nodes and 'new U5' labels)
        plot_ubahn_network(blocks_gdf, green, water, districts, ubahn_lines, ubahn_stops)
        
        # Load legacy matrix and plot differences
        print(f"\nChecking for legacy TTM at {LEGACY_PARQUET_FILE}...")
        if LEGACY_PARQUET_FILE.exists():
            legacy_matrix_data = load_matrix(LEGACY_PARQUET_FILE)
            print("\nComputing difference maps...")
            # Savings map (with S-Bahn and U-Bahn overlay, and legend)
            plot_travel_time_savings(new_matrix_data, legacy_matrix_data, blocks_gdf, green, water, districts, ubahn_lines, sbahn_lines)
            # Speed and Speed Gains maps
            plot_travel_speeds(new_matrix_data, legacy_matrix_data, blocks_gdf, green, water, districts, ubahn_lines, sbahn_lines)
        else:
            print("Legacy travel time matrix not found on disk. Skipping difference plots.")
            
        print("\nAll maps generated successfully!")
    except Exception as e:
        import traceback
        print(f"Error generating maps: {e}")
        traceback.print_exc()

