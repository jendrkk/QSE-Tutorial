"""
plots_delta.py
--------------
Plots the per-block difference in average travel times between two travel-time
matrices.  The visual design (colours, typography, overlays, layout) is
intentionally identical to plots.py.

Usage
-----
    python plots_delta.py <path_matrix_a> <path_matrix_b> \
        [--label-a "Matrix A"] [--label-b "Matrix B"] \
        [--output-dir .]

Each matrix path can point to a .parquet file (preferred) or a .mat file that
follows the same schema used in Final.py / plots.py.
"""

import argparse
import os
import sys
from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import scipy.io as sio
import seaborn as sns

# ---------------------------------------------------------------------------
# Path resolution (mirrors plots.py)
# ---------------------------------------------------------------------------
SCRIPT_DIR    = Path(__file__).resolve().parent
REPO_ROOT     = SCRIPT_DIR.parent
TRANSPORT_DIR = REPO_ROOT.parent / "Data" / "Shapefiles-2022" / "Berlin" / "TransportNetworkParts2006"
ARSW_DIR      = REPO_ROOT.parent / "ARSW2015" / "ARSW2015-toolkit" / "shapefile"

os.chdir(SCRIPT_DIR)

METRIC_CRS  = "EPSG:25833"
BLOCKS_SHP  = ARSW_DIR      / "Berlin4matlab.shp"
STREETS_SHP = TRANSPORT_DIR / "Streets.shp"

# ---------------------------------------------------------------------------
# Matplotlib / Seaborn style (identical to plots.py)
# ---------------------------------------------------------------------------
sns.set_style("whitegrid")
plt.rcParams.update({
    'font.size': 16.0,
    'font.family': 'serif',
    'font.serif': 'Palatino',
    'axes.titlesize': 'medium',
    'figure.titlesize': 'large',
    'legend.fontsize': 'medium',
    'figure.dpi': 100,
    'savefig.dpi': 300,
    'figure.autolayout': True,
    'text.usetex': True,
    'text.latex.preamble': r"\usepackage{amsmath}\usepackage{amssymb}\usepackage{siunitx}[=v2]",
})


# ---------------------------------------------------------------------------
# Geometry helpers (identical to plots.py)
# ---------------------------------------------------------------------------
def _is_valid_coord_geom(geom):
    try:
        b = geom.bounds
        return len(b) == 4 and not (np.any(np.isnan(b)) or np.any(np.isinf(b)))
    except Exception:
        return False


def load_geometries():
    print("Loading geometries...")
    blocks_gdf = gpd.read_file(BLOCKS_SHP)
    blocks_gdf = (
        blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty]
        .reset_index(drop=True)
    )
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.apply(_is_valid_coord_geom)].reset_index(drop=True)
    blocks_gdf['geometry'] = blocks_gdf.geometry.make_valid()
    blocks_gdf = blocks_gdf.to_crs(METRIC_CRS)
    blocks_gdf = (
        blocks_gdf[blocks_gdf.geometry.notnull() & ~blocks_gdf.geometry.is_empty]
        .reset_index(drop=True)
    )
    blocks_gdf = blocks_gdf[blocks_gdf.geometry.apply(_is_valid_coord_geom)].reset_index(drop=True)
    blocks_gdf['centroid_id'] = [f"centroid_{i}" for i in range(len(blocks_gdf))]

    print("Loading street network...")
    streets = gpd.read_file(STREETS_SHP).to_crs(METRIC_CRS)

    return blocks_gdf, streets


def _load_green_water():
    green_gdf = gpd.read_file(ARSW_DIR / "BerlinGreen.shp").to_crs(epsg=25833).reset_index(drop=True)
    water_gdf = gpd.read_file(ARSW_DIR / "BerlinWater.shp").to_crs(epsg=25833).reset_index(drop=True)
    return green_gdf, water_gdf


# ---------------------------------------------------------------------------
# Matrix loader: parquet (preferred) or .mat
# ---------------------------------------------------------------------------
def load_matrix(path: Path) -> dict:
    """
    Returns a dict with keys:
        'matrix' : np.ndarray  (N x N, float)
        'ids'    : list[str]   (centroid IDs, length N)
    """
    path = Path(path)
    if not path.exists():
        sys.exit(f"ERROR: Matrix file not found: {path}")

    suffix = path.suffix.lower()

    if suffix == ".parquet":
        print(f"  Reading parquet: {path}")
        df = pd.read_parquet(path)
        return {'matrix': df.values, 'ids': df.index.tolist()}

    if suffix == ".mat":
        print(f"  Reading .mat: {path}")
        data = sio.loadmat(path)
        # Try common key names used in Final.py
        for key in ('tt_matrix_full', 'tt_matrix_simple', 'tt_matrix'):
            if key in data:
                mat = data[key]
                c_ids = data.get('centroid_ids', None)
                if c_ids is not None:
                    ids = [str(c).strip() for c in np.array(c_ids).flatten()]
                else:
                    ids = [f"centroid_{i}" for i in range(mat.shape[0])]
                return {'matrix': mat, 'ids': ids}
        sys.exit(f"ERROR: Could not find a travel-time matrix inside {path}. "
                 f"Available keys: {list(data.keys())}")

    sys.exit(f"ERROR: Unsupported file format '{suffix}'. Use .parquet or .mat.")


# ---------------------------------------------------------------------------
# Core map-drawing helper (mirrors the loop in plot_geographic_comparisons)
# ---------------------------------------------------------------------------
def _draw_map(ax, blocks_gdf, map_gdf, column, cmap, legend_label,
              streets, green_gdf, water_gdf, vmin=None, vmax=None):
    blocks_gdf.plot(ax=ax, color='#eaeaea', edgecolor='none')
    map_gdf.plot(
        column=column,
        cmap=cmap,
        legend=True,
        legend_kwds={
            'label': legend_label,
            'orientation': 'vertical',
            'fraction': 0.03,
            'pad': 0.02,
            'shrink': 0.8,
        },
        ax=ax,
        edgecolor='none',
        vmin=vmin,
        vmax=vmax,
    )
    green_gdf.plot(ax=ax, color='forestgreen', alpha=0.35, linewidth=0.1, zorder=0)
    water_gdf.plot(ax=ax, color='royalblue',   alpha=0.35, linewidth=0.1, zorder=0)
    streets.plot(  ax=ax, color='white',        linewidth=0.1, alpha=0.35, zorder=0)
    ax.set_axis_off()


# ---------------------------------------------------------------------------
# Plot 1 – Delta map  (avg_A − avg_B)
# ---------------------------------------------------------------------------
def plot_delta_map(delta, centroid_ids, blocks_gdf, streets, label_a, label_b, output_dir):
    print("Plotting delta map (avg travel time A − B)...")
    green_gdf, water_gdf = _load_green_water()

    df = pd.DataFrame({'centroid_id': centroid_ids, 'delta': delta})
    map_gdf = blocks_gdf.merge(df, on='centroid_id')

    abs_max = np.nanmax(np.abs(delta))
    vmin, vmax = -abs_max, abs_max

    fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
    _draw_map(
        ax, blocks_gdf, map_gdf,
        column='delta',
        cmap='RdBu_r',
        legend_label=rf'$\Delta$ Avg. Travel Time: {label_a} $-$ {label_b} (minutes)',
        streets=streets,
        green_gdf=green_gdf,
        water_gdf=water_gdf,
        vmin=vmin,
        vmax=vmax,
    )
    ax.set_title(
        rf'Berlin Accessibility: Difference in Average Travel Time'
        '\n'
        rf'(\textit{{{label_a}}} $-$ \textit{{{label_b}}})',
        fontsize=14, fontweight='bold', pad=15,
    )
    plt.tight_layout()

    fname = output_dir / "berlin_avg_travel_time_delta.png"
    plt.savefig(fname, dpi=300, bbox_inches='tight', transparent=True)
    plt.close()
    print(f"  Saved '{fname}'")


# ---------------------------------------------------------------------------
# Plot 2 – Side-by-side absolute avg-time maps on a shared colour scale
# ---------------------------------------------------------------------------
def plot_absolute_maps(avg_a, avg_b, centroid_ids, blocks_gdf, streets,
                       label_a, label_b, output_dir):
    print("Plotting absolute average travel time maps (shared scale)...")
    green_gdf, water_gdf = _load_green_water()

    t_min = min(np.nanmin(avg_a), np.nanmin(avg_b))
    t_max = max(np.nanmax(avg_a), np.nanmax(avg_b))

    for avg, label, fname_stem in [
        (avg_a, label_a, "berlin_avg_time_matrix_a"),
        (avg_b, label_b, "berlin_avg_time_matrix_b"),
    ]:
        df = pd.DataFrame({'centroid_id': centroid_ids, 'avg_travel_time': avg})
        map_gdf = blocks_gdf.merge(df, on='centroid_id')

        fig, ax = plt.subplots(figsize=(12, 10), dpi=300)
        _draw_map(
            ax, blocks_gdf, map_gdf,
            column='avg_travel_time',
            cmap='viridis_r',
            legend_label='Average Travel Time to All Other Centroids (minutes)',
            streets=streets,
            green_gdf=green_gdf,
            water_gdf=water_gdf,
            vmin=t_min,
            vmax=t_max,
        )
        ax.set_title(
            rf'Berlin Geographic Accessibility: Average Travel Time'
            '\n'
            rf'(\textit{{{label}}})',
            fontsize=14, fontweight='bold', pad=15,
        )
        plt.tight_layout()

        fname = output_dir / f"{fname_stem}.png"
        plt.savefig(fname, dpi=300, bbox_inches='tight', transparent=True)
        plt.close()
        print(f"  Saved '{fname}'")


# ---------------------------------------------------------------------------
# Plot 3 – Histogram of per-block deltas
# ---------------------------------------------------------------------------
def plot_delta_histogram(delta, label_a, label_b, output_dir):
    print("Plotting delta histogram...")
    mean_d = np.nanmean(delta)

    plt.figure(figsize=(10, 6), dpi=300)
    plt.hist(delta, bins=50, color='tab:blue', alpha=0.7, density=True,
             label=rf'{label_a} $-$ {label_b}')
    plt.axvline(0,      color='black',   linestyle='--', linewidth=1.5, label='No change')
    plt.axvline(mean_d, color='tab:red', linestyle='--', linewidth=1.5,
                label=rf'Mean $\Delta$ = {mean_d:.1f} min')

    plt.xlabel(rf'$\Delta$ Avg. Travel Time: {label_a} $-$ {label_b} (minutes)', fontsize=12)
    plt.ylabel('Density', fontsize=12)
    plt.title(
        rf'Distribution of Per-Block Travel-Time Differences'
        '\n'
        rf'(\textit{{{label_a}}} $-$ \textit{{{label_b}}})',
        fontsize=14, fontweight='bold',
    )
    plt.legend()
    plt.grid(axis='y', alpha=0.3)
    plt.tight_layout()

    fname = output_dir / "berlin_avg_travel_time_delta_histogram.png"
    plt.savefig(fname, dpi=300, transparent=True)
    plt.close()
    print(f"  Saved '{fname}'")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
def parse_args():
    parser = argparse.ArgumentParser(
        description="Plot per-block differences in average travel times between two matrices.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("path_a", type=Path,
                        help="Path to the first travel-time matrix (.parquet or .mat)")
    parser.add_argument("path_b", type=Path,
                        help="Path to the second travel-time matrix (.parquet or .mat)")
    parser.add_argument("--label-a", default="Matrix A",
                        help="Short label for the first matrix (default: 'Matrix A')")
    parser.add_argument("--label-b", default="Matrix B",
                        help="Short label for the second matrix (default: 'Matrix B')")
    parser.add_argument("--output-dir", type=Path, default=SCRIPT_DIR,
                        help="Directory in which to save output PNGs (default: TTM folder)")
    return parser.parse_args()


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------
def main():
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # -- Load matrices
    print(f"\nLoading matrix A: {args.path_a}")
    mat_a = load_matrix(args.path_a)
    print(f"Loading matrix B: {args.path_b}")
    mat_b = load_matrix(args.path_b)

    # -- Validate that both matrices share the same centroid IDs
    ids_a = mat_a['ids']
    ids_b = mat_b['ids']

    if ids_a != ids_b:
        # Try to find a common ordered intersection
        common = [c for c in ids_a if c in set(ids_b)]
        if len(common) == 0:
            sys.exit("ERROR: The two matrices share no common centroid IDs.")
        print(f"WARNING: Centroid ID sets differ; using {len(common)} common IDs.")
        idx_a = [ids_a.index(c) for c in common]
        idx_b = [ids_b.index(c) for c in common]
        matrix_a = mat_a['matrix'][np.ix_(idx_a, idx_a)]
        matrix_b = mat_b['matrix'][np.ix_(idx_b, idx_b)]
        centroid_ids = common
    else:
        matrix_a    = mat_a['matrix']
        matrix_b    = mat_b['matrix']
        centroid_ids = ids_a

    if matrix_a.shape != matrix_b.shape:
        sys.exit(
            f"ERROR: Matrix shape mismatch after alignment: "
            f"{matrix_a.shape} vs {matrix_b.shape}"
        )

    # -- Compute per-block average travel times
    print("\nComputing average travel times per block...")
    # Replace 0s on the diagonal (self-travel) and inf (unreachable) with NaN
    matrix_a = matrix_a.astype(np.float64)
    matrix_b = matrix_b.astype(np.float64)
    np.fill_diagonal(matrix_a, np.nan)
    np.fill_diagonal(matrix_b, np.nan)
    matrix_a[~np.isfinite(matrix_a)] = np.nan
    matrix_b[~np.isfinite(matrix_b)] = np.nan

    avg_a = np.nanmean(matrix_a, axis=1)
    avg_b = np.nanmean(matrix_b, axis=1)
    delta = avg_a - avg_b  # positive → A is slower, negative → A is faster

    n_pos = np.sum(delta > 0)
    n_neg = np.sum(delta < 0)
    print(f"  Mean delta : {np.nanmean(delta):+.2f} min")
    print(f"  Blocks where A is slower (delta > 0) : {n_pos}")
    print(f"  Blocks where A is faster  (delta < 0) : {n_neg}")

    # -- Load spatial data
    blocks_gdf, streets = load_geometries()

    # -- Generate plots
    label_a = args.label_a
    label_b = args.label_b

    plot_delta_map(delta, centroid_ids, blocks_gdf, streets, label_a, label_b, args.output_dir)
    plot_absolute_maps(avg_a, avg_b, centroid_ids, blocks_gdf, streets, label_a, label_b, args.output_dir)
    plot_delta_histogram(delta, label_a, label_b, args.output_dir)

    print("\nAll plots saved successfully.")


if __name__ == "__main__":
    main()
