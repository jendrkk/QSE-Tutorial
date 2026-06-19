import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.io as sio
import os
from pathlib import Path
from scipy.spatial.distance import cdist
import seaborn as sns

# ---------------------------------------------------------------------------
# Path resolution: works regardless of the working directory at invocation.
# SCRIPT_DIR  -> .../QSE-Tutorial/Topic_7/
# REPO_ROOT   -> .../QSE-Tutorial/
# ---------------------------------------------------------------------------
SCRIPT_DIR    = Path(__file__).resolve().parent
REPO_ROOT     = SCRIPT_DIR.parent
TRANSPORT_DIR = REPO_ROOT.parent / "Data" / "Shapefiles-2022" / "Berlin" / "TransportNetworkParts2006"
ARSW_DIR      = REPO_ROOT.parent / "ARSW2015" / "ARSW2015-toolkit" / "shapefile"

# Ensure outputs are read from / written to the Topic_7 folder
os.chdir(SCRIPT_DIR)

# Configuration
METRIC_CRS   = "EPSG:25833"
BLOCKS_SHP   = ARSW_DIR      / "Berlin4matlab.shp"
STREETS_SHP  = TRANSPORT_DIR / "Streets.shp"
MAT_FILE     = SCRIPT_DIR    / "travel_time_matrices.mat"

sns.set_style("whitegrid")
plt.rcParams.update({
    #'figure.figsize': (6*2, 2*4.5),
    'font.size': 16.0,
    'font.family': 'serif',
    'font.serif': 'Palatino',
    'axes.titlesize': 'medium',
    'figure.titlesize': 'large',
    'legend.fontsize': 'medium',
    # dpi for high-res output
    'figure.dpi': 100,
    'savefig.dpi': 300,
    # Tight layout by default
    'figure.autolayout': True,
    'text.usetex': True,
    'text.latex.preamble': r"\usepackage{amsmath}\usepackage{amssymb}\usepackage{siunitx}[=v2]",
})


def load_geometries():
    print("Loading geometries...")
    # Load blocks
    blocks_gdf = gpd.read_file(BLOCKS_SHP)
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)
    
    def is_valid_coordinate_geom(geom):
        try:
            b = geom.bounds
            return len(b) == 4 and not (np.any(np.isnan(b)) or np.any(np.isinf(b)))
        except: return False

    blocks_gdf = blocks_gdf[blocks_gdf.geometry.apply(is_valid_coordinate_geom)].reset_index(drop=True)
    blocks_gdf['geometry'] = blocks_gdf.geometry.make_valid()
    blocks_gdf = blocks_gdf.to_crs(METRIC_CRS)
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty].reset_index(drop=True)
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.apply(is_valid_coordinate_geom)].reset_index(drop=True)
    blocks_gdf['centroid_id'] = [f"centroid_{i}" for i in range(len(blocks_gdf))]
    
    # Load streets for overlay
    print("Loading street network...")
    streets = gpd.read_file(STREETS_SHP).to_crs(METRIC_CRS)
    
    return blocks_gdf, streets

PARQUET_FULL   = SCRIPT_DIR / "sample_travel_time_matrix.parquet"
PARQUET_SIMPLE = SCRIPT_DIR / "simplified_travel_time_matrix.parquet"

def load_matrices():
    print("Loading matrices...")
    try:
        # Prefer Parquet for speed and precision
        print(f"Reading {PARQUET_FULL}...")
        df_full = pd.read_parquet(PARQUET_FULL)
        print(f"Reading {PARQUET_SIMPLE}...")
        df_simple = pd.read_parquet(PARQUET_SIMPLE)

        return {
            'full': df_full.values,
            'simple': df_simple.values,
            'ids': df_full.index.tolist()
        }
    except Exception as e:
        print(f"Parquet loading failed or files missing ({e}). Falling back to .mat...")
        try:
            data = sio.loadmat(MAT_FILE)
            c_ids = data['centroid_ids']
            centroid_ids = [str(c).strip() for c in c_ids.flatten()] if isinstance(c_ids, np.ndarray) else [str(c).strip() for c in c_ids]
            return {
                'full': data['tt_matrix_full'],
                'simple': data['tt_matrix_simple'],
                'ids': centroid_ids
            }
        except Exception as e2:
            print(f"Error loading fallback .mat file: {e2}")
            return None


def plot_average_travel_times(matrices, blocks_gdf, streets):
    print("Plotting average travel times...")
    tt_matrix_full = matrices['full']
    centroid_ids = matrices['ids']
    
    avg_times = np.nanmean(tt_matrix_full, axis=1)
    avg_times_df = pd.DataFrame({
        'centroid_id': centroid_ids,
        'avg_travel_time': avg_times
    })
    
    map_gdf = blocks_gdf.merge(avg_times_df, on='centroid_id')
    
    fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    
    map_gdf.plot(
        column='avg_travel_time',
        cmap='viridis_r',
        legend=True,
        legend_kwds={
            'label': 'Average Travel Time to All Other Centroids (Minutes)',
            'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7
        },
        ax=ax,
        edgecolor='none'
    )
    green_gdf = gpd.read_file(ARSW_DIR / "BerlinGreen.shp").to_crs(epsg=25833).reset_index(drop=True)
    water_gdf = gpd.read_file(ARSW_DIR / "BerlinWater.shp").to_crs(epsg=25833).reset_index(drop=True)
    green_gdf.plot(ax=ax, color='forestgreen', alpha=0.35, linewidth=0.1, zorder=0)
    water_gdf.plot(ax=ax, color='royalblue', alpha=0.35, linewidth=0.1, zorder=0)
    
    streets.plot(ax=ax, color='white', linewidth=0.1, alpha=0.35, zorder=0)
    ax.set_title('Berlin Geographic Accessibility: Average Travel Time\n(Full Multi-modal Network 2006)', fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    
    plt.tight_layout()
    plt.savefig('berlin_average_travel_times.png', dpi=300, bbox_inches='tight', transparent=True)
    plt.close()
    print("Saved 'berlin_average_travel_times.png'")

def plot_average_speed_map(matrices, blocks_gdf, streets):
    print("Plotting average travel speed map...")
    centroid_ids = matrices['ids']
    blocks_ordered = blocks_gdf.set_index('centroid_id').loc[centroid_ids].reset_index()
    coords = np.array([[g.x, g.y] for g in blocks_ordered.geometry.centroid])
    
    # Compute Euclidean distance matrix and calculate speeds
    dist_matrix = cdist(coords, coords, metric='euclidean').astype(np.float32)
    with np.errstate(divide='ignore', invalid='ignore'):
        v_full_mat = np.clip((dist_matrix / matrices['full']) * 0.06, 0, 100)
    
    avg_v_full = np.nanmean(v_full_mat, axis=1)
    speed_df = pd.DataFrame({'centroid_id': centroid_ids, 'avg_speed': avg_v_full})
    
    map_gdf = blocks_gdf.merge(speed_df, on='centroid_id')
    
    fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    
    map_gdf.plot(
        column='avg_speed',
        cmap='magma', # High speed = brighter colors
        legend=True,
        legend_kwds={
            'label': 'Average Journey Speed (Euclidean Dist / Time) [km/h]',
            'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7
        },
        ax=ax,
        edgecolor='none'
    )
    
    green_gdf = gpd.read_file(ARSW_DIR / "BerlinGreen.shp").to_crs(epsg=25833).reset_index(drop=True)
    water_gdf = gpd.read_file(ARSW_DIR / "BerlinWater.shp").to_crs(epsg=25833).reset_index(drop=True)
    green_gdf.plot(ax=ax, color='forestgreen', alpha=0.35, linewidth=0.1, zorder=0)
    water_gdf.plot(ax=ax, color='royalblue', alpha=0.35, linewidth=0.1, zorder=0)
    
    streets.plot(ax=ax, color='white', linewidth=0.1, alpha=0.3, zorder=2)
    ax.set_title('Berlin Geographic Performance: Average Journey Speed\n(Full Multi-modal Network 2006)', fontsize=14, fontweight='bold', pad=15)
    ax.set_axis_off()
    
    plt.tight_layout()
    plt.savefig('berlin_average_travel_speeds.png', dpi=300, bbox_inches='tight', transparent=True)
    plt.close()
    print("Saved 'berlin_average_travel_speeds.png'")

def plot_speed_distribution(matrices, blocks_gdf):
    print("Generating speed distribution histogram...")
    centroid_ids = matrices['ids']
    blocks_ordered = blocks_gdf.set_index('centroid_id').loc[centroid_ids].reset_index()
    coords = np.array([[g.x, g.y] for g in blocks_ordered.geometry.centroid])
    dist_matrix = cdist(coords, coords, metric='euclidean').astype(np.float32)
    
    with np.errstate(divide='ignore', invalid='ignore'):
        avg_v_full = np.nanmean(np.clip((dist_matrix / matrices['full']) * 0.06, 0, 100), axis=1)
        avg_v_simple = np.nanmean(np.clip((dist_matrix / matrices['simple']) * 0.06, 0, 100), axis=1)

    plt.figure(figsize=(10, 6), dpi=300)
    plt.hist(avg_v_full, bins=50, alpha=0.5, label=r'With Buses \& Trams', color='tab:blue', density=True)
    plt.hist(avg_v_simple, bins=50, alpha=0.5, label=r'Without Buses \& Trams', color='tab:orange', density=True)
    
    plt.axvline(np.nanmean(avg_v_full), color='blue', linestyle='--', label=rf'Mean (With): {np.nanmean(avg_v_full):.1f} km/h')
    plt.axvline(np.nanmean(avg_v_simple), color='orange', linestyle='--', label=rf'Mean (Without): {np.nanmean(avg_v_simple):.1f} km/h')
    
    plt.xlabel('Average Journey Speed per Block (km/h)', fontsize=12)
    plt.ylabel('Density', fontsize=12)
    plt.title(r'Shift in Berlin Travel Speed Distribution' '\n' r'(Impact of Bus \& Tram Network)', fontsize=14, fontweight='bold')
    plt.legend(); plt.grid(axis='y', alpha=0.3); plt.tight_layout()
    plt.savefig('speed_distribution_histogram.png', dpi=300, transparent=True); plt.close()
    print("Saved 'speed_distribution_histogram.png'")

def plot_accessibility_scatter(matrices):
    print("Generating accessibility comparison scatter plot...")
    avg_full = np.nanmean(matrices['full'], axis=1)
    avg_simple = np.nanmean(matrices['simple'], axis=1)
    
    # Clean NaNs for plotting and fitting
    mask = ~np.isnan(avg_full) & ~np.isnan(avg_simple)
    x, y = avg_simple[mask], avg_full[mask]
    
    plt.figure(figsize=(10, 10), dpi=300)
    plt.scatter(x, y, alpha=0.2, s=5, color='tab:blue', label='Berlin Blocks')
    
    # Unified Square Axis
    lims = [min(x.min(), y.min()), max(x.max(), y.max())]
    plt.plot(lims, lims, color='red', linestyle='--', linewidth=1.5, label='1:1 Parity Line')
    
    # Linear Fit
    m, b = np.polyfit(x, y, 1)
    plt.plot(x, m*x + b, color='black', linewidth=2, label=f'Linear Fit (slope={m:.2f})')
    
    plt.xlim(lims); plt.ylim(lims)
    plt.xlabel(r'Avg Travel Time: WITHOUT Buses \& Trams (Minutes)', fontsize=12, fontweight='bold')
    plt.ylabel(r'Avg Travel Time: WITH Buses \& Trams (Minutes)', fontsize=12, fontweight='bold')
    plt.title('Accessibility Comparison: Time to Reach All Other Blocks\n(Points below line = Faster with Buses/Trams)', fontsize=14, fontweight='bold', pad=20)
    plt.grid(True, linestyle='--', alpha=0.5); plt.legend(); plt.tight_layout()
    plt.savefig('accessibility_comparison_scatter.png', dpi=300, transparent=True); plt.close()
    print("Saved 'accessibility_comparison_scatter.png'")

def plot_speed_scatter(matrices, blocks_gdf):
    print("Calculating travel speeds for comparison plot...")
    centroid_ids = matrices['ids']
    blocks_ordered = blocks_gdf.set_index('centroid_id').loc[centroid_ids].reset_index()
    coords = np.array([[g.x, g.y] for g in blocks_ordered.geometry.centroid])
    
    print("Computing Euclidean distance matrix...")
    dist_matrix = cdist(coords, coords, metric='euclidean').astype(np.float32)
    
    # Speed (km/h) = (dist_m / time_min) * 0.06
    with np.errstate(divide='ignore', invalid='ignore'):
        v_full_mat = np.clip((dist_matrix / matrices['full']) * 0.06, 0, 100)
        v_simple_mat = np.clip((dist_matrix / matrices['simple']) * 0.06, 0, 100)
    
    avg_v_full = np.nanmean(v_full_mat, axis=1)
    avg_v_simple = np.nanmean(v_simple_mat, axis=1)
    
    mask = ~np.isnan(avg_v_full) & ~np.isnan(avg_v_simple)
    x, y = avg_v_simple[mask], avg_v_full[mask]
    
    plt.figure(figsize=(10, 10), dpi=300)
    plt.scatter(x, y, alpha=0.2, s=5, color='tab:orange', label='Berlin Blocks')
    
    # Unified Square Axis
    lims = [0, max(x.max(), y.max())]
    plt.plot(lims, lims, color='red', linestyle='--', linewidth=1.5, label='1:1 Parity Line')
    
    # Linear Fit
    m, b = np.polyfit(x, y, 1)
    plt.plot(x, m*x + b, color='black', linewidth=2, label=f'Linear Fit (slope={m:.2f})')
    
    plt.xlim(lims); plt.ylim(lims)
    plt.xlabel(r'Avg Travel Speed: WITHOUT Buses \& Trams (km/h)', fontsize=12, fontweight='bold')
    plt.ylabel(r'Avg Travel Speed: WITH Buses \& Trams (km/h)', fontsize=12, fontweight='bold')
    plt.title('Performance Comparison: Journey Speed (Euclidean Dist / Time)\n(Points above line = Faster Journeys with Buses/Trams)', fontsize=14, fontweight='bold', pad=20)
    plt.grid(True, linestyle='--', alpha=0.5); plt.legend(); plt.tight_layout()
    plt.savefig('speed_comparison_scatter.png', dpi=300, transparent=True); plt.close()
    print("Saved 'speed_comparison_scatter.png'")

def plot_geographic_comparisons(matrices, blocks_gdf, streets):
    print("Generating comprehensive geographic comparison maps...")
    centroid_ids = matrices['ids']
    blocks_ordered = blocks_gdf.set_index('centroid_id').loc[centroid_ids].reset_index()
    coords = np.array([[g.x, g.y] for g in blocks_ordered.geometry.centroid])
    
    # 1. TIME MAPS
    print("  -> Processing Time Maps...")
    avg_t_full = np.nanmean(matrices['full'], axis=1)
    avg_t_simple = np.nanmean(matrices['simple'], axis=1)
    t_diff = avg_t_simple - avg_t_full
    
    # Common Scale for absolute time maps
    t_min, t_max = min(avg_t_full.min(), avg_t_simple.min()), max(avg_t_full.max(), avg_t_simple.max())
    
    scenarios_t = [
        (avg_t_simple, 'viridis_r', 'Average Travel Time (Without Bus/Tram) [min]', 'berlin_avg_time_simple.png', (t_min, t_max)),
        (t_diff, 'YlOrRd', 'Travel Time Increase without Bus/Tram [min]', 'berlin_avg_time_diff.png', None)
    ]
    
    for data, cmap, label, fname, vlims in scenarios_t:
        df = pd.DataFrame({'centroid_id': centroid_ids, 'val': data})
        map_gdf = blocks_gdf.merge(df, on='centroid_id')
        fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
        blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
        map_gdf.plot(column='val', cmap=cmap, legend=True, ax=ax, edgecolor='none',
                     vmin=vlims[0] if vlims else None, vmax=vlims[1] if vlims else None,
                     legend_kwds={'label': label, 'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7})
        green_gdf = gpd.read_file(ARSW_DIR / "BerlinGreen.shp").to_crs(epsg=25833).reset_index(drop=True)
        water_gdf = gpd.read_file(ARSW_DIR / "BerlinWater.shp").to_crs(epsg=25833).reset_index(drop=True)
        green_gdf.plot(ax=ax, color='forestgreen', alpha=0.35, linewidth=0.1, zorder=0)
        water_gdf.plot(ax=ax, color='royalblue', alpha=0.35, linewidth=0.1, zorder=0)
        streets.plot(ax=ax, color='white', linewidth=0.1, alpha=0.3, zorder=0)
        ax.set_axis_off(); plt.tight_layout(); plt.savefig(fname, dpi=300, transparent=True); plt.close()
        print(f"     Saved {fname}")

    # 2. SPEED MAPS
    print("  -> Processing Speed Maps...")
    dist_matrix = cdist(coords, coords, metric='euclidean').astype(np.float32)
    with np.errstate(divide='ignore', invalid='ignore'):
        v_full_mat = np.clip((dist_matrix / matrices['full']) * 0.06, 0, 100)
        v_simple_mat = np.clip((dist_matrix / matrices['simple']) * 0.06, 0, 100)
    
    avg_v_full = np.nanmean(v_full_mat, axis=1)
    avg_v_simple = np.nanmean(v_simple_mat, axis=1)
    v_diff = avg_v_full - avg_v_simple # How much speed we gain by adding Bus/Tram
    
    # Common Scale for absolute speed maps
    v_min, v_max = min(avg_v_full.min(), avg_v_simple.min()), max(avg_v_full.max(), avg_v_simple.max())
    
    scenarios_v = [
        (avg_v_simple, 'magma', 'Average Journey Speed (Without Bus/Tram) [km/h]', 'berlin_avg_speed_simple.png', (v_min, v_max)),
        (v_diff, 'coolwarm', 'Journey Speed Gain with Bus/Tram [km/h]', 'berlin_avg_speed_diff.png', None)
    ]
    
    for data, cmap, label, fname, vlims in scenarios_v:
        df = pd.DataFrame({'centroid_id': centroid_ids, 'val': data})
        map_gdf = blocks_gdf.merge(df, on='centroid_id')
        fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
        blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
        map_gdf.plot(column='val', cmap=cmap, legend=True, ax=ax, edgecolor='none',
                     vmin=vlims[0] if vlims else None, vmax=vlims[1] if vlims else None,
                     legend_kwds={'label': label, 'orientation': 'horizontal', 'pad': 0.05, 'shrink': 0.7})
        
        green_gdf = gpd.read_file(ARSW_DIR / "BerlinGreen.shp").to_crs(epsg=25833).reset_index(drop=True)
        water_gdf = gpd.read_file(ARSW_DIR / "BerlinWater.shp").to_crs(epsg=25833).reset_index(drop=True)
        green_gdf.plot(ax=ax, color='forestgreen', alpha=0.35, linewidth=0.1, zorder=0)
        water_gdf.plot(ax=ax, color='royalblue', alpha=0.35, linewidth=0.1, zorder=0)
        streets.plot(ax=ax, color='white', linewidth=0.1, alpha=0.3, zorder=0)
        ax.set_axis_off(); plt.tight_layout(); plt.savefig(fname, dpi=300, transparent=True); plt.close()
        print(f"     Saved {fname}")

if __name__ == "__main__":
    matrices = load_matrices()
    if matrices:
        blocks_gdf, streets = load_geometries()
        # Existing Maps
        plot_average_travel_times(matrices, blocks_gdf, streets)
        plot_average_speed_map(matrices, blocks_gdf, streets)
        
        # New Comparative Maps
        plot_geographic_comparisons(matrices, blocks_gdf, streets)
        
        # Scatters and Distributions
        plot_accessibility_scatter(matrices)
        plot_speed_scatter(matrices, blocks_gdf)
        plot_speed_distribution(matrices, blocks_gdf)
