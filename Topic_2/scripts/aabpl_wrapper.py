# =============================================================================
# TASK 3a — CBD Identification from Price Index using AABPL Algorithm
# =============================================================================
# Theory: Monocentric city model predicts prices peak at the CBD.
# We use the purchase price index as the "importance weight" in the AABPL
# clustering algorithm to identify the dominant price cluster = theory-
# consistent CBD. Its centroid is used as the city center for Task 3b.
#
# Input:  Output/2_index_values/index_wk_long.csv  (purchase prices)
#         Output/1_postcode_city_shapefiles/postcodes_<city>.shp
# Output: Output/3_cbd/all_cities_cbd_centroids.csv
#
# CRS note: shapefiles are ETRS89/UTM32N (EPSG:25832).
#           AABPL requires WGS84 decimal degrees (EPSG:4326).
# =============================================================================

from pathlib import Path
import pandas as pd
import numpy as np
import geopandas as gpd
from pyproj import Transformer
import warnings
warnings.filterwarnings("ignore")

# --- AABPL: correct import path (not a top-level run_delineation function) ---
from aabpl.main import detect_cluster_cells

# =============================================================================
# PATHS
# =============================================================================
REPO_ROOT = Path(__file__).resolve().parents[2]   # QSE-Tutorial/
OUT_ROOT  = REPO_ROOT / "Topic_2" / "Output"
SHP_DIR   = OUT_ROOT / "1_postcode_city_shapefiles"
IDX_DIR   = OUT_ROOT / "2_index_values"
CBD_DIR   = OUT_ROOT / "3_cbd"
CBD_DIR.mkdir(exist_ok=True)

# =============================================================================
# CONFIG
# =============================================================================
CITIES = {
    "berlin":            "postcodes_berlin.shp",
    "hamburg":           "postcodes_hamburg.shp",
    "muenchen":          "postcodes_muenchen.shp",
    "koeln":             "postcodes_koeln.shp",
    "frankfurt_am_main": "postcodes_frankfurt_am_main.shp",
}

INDEX_FILE     = IDX_DIR / "index_wk_long.csv"   # purchase prices
REFERENCE_YEAR = 2022                              # None = average all years

# CRS conversion: ETRS89/UTM32N → WGS84
SRC_CRS  = "EPSG:25832"
DEST_CRS = "EPSG:4326"

# AABPL parameters
# r: search radius in metres. Postcodes are coarse (~1-3km diameter),
#    so 2000m captures meaningful neighbourhood price clusters.
RADIUS_M       = 2000
# k_th_percentile: a point must exceed this percentile of a random
#    distribution to be labelled as a cluster cell. 99.5 is the default
#    (very selective — only the true price peak qualifies).
K_PERCENTILE   = 99.5
# Weight column name
WEIGHT_COL     = "price_weight"
# Suffix added by AABPL to create the cluster membership column
CLUSTER_SUFFIX = "_cluster"

# =============================================================================
# STEP 1 — Load price index
# =============================================================================
print("Loading price index...")
idx = pd.read_csv(INDEX_FILE)

if REFERENCE_YEAR is not None:
    idx_yr = idx[idx["year"] == REFERENCE_YEAR].copy()
    if len(idx_yr) == 0:
        raise ValueError(
            f"No data for year {REFERENCE_YEAR}. "
            f"Available years: {sorted(idx['year'].unique())}"
        )
    print(f"  Year {REFERENCE_YEAR}: {len(idx_yr)} observations")
else:
    idx_yr = (
        idx.groupby("target_id")
           .agg(lprice=("lprice", "mean"),
                target_X=("target_X", "first"),
                target_Y=("target_Y", "first"))
           .reset_index()
    )
    print(f"  All-year average: {len(idx_yr)} observations")

# Exponentiate log-price to get positive weight for AABPL
idx_yr[WEIGHT_COL] = np.exp(idx_yr["lprice"])
idx_yr["target_id"] = pd.to_numeric(idx_yr["target_id"], errors="coerce").astype("Int64")

# =============================================================================
# STEP 2 — CRS transformer (ETRS89/UTM32N → WGS84)
# =============================================================================
transformer = Transformer.from_crs(SRC_CRS, DEST_CRS, always_xy=True)

# =============================================================================
# STEP 3 — Process each city
# =============================================================================
results = {}

for city, shp_file in CITIES.items():
    print(f"\n{'='*60}")
    print(f"  {city.upper()}")
    print(f"{'='*60}")

    # ── 3a. Load postcode shapefile ───────────────────────────────────────────
    shp_path = SHP_DIR / shp_file
    if not shp_path.exists():
        print(f"  ⚠  Shapefile missing: {shp_path}")
        continue

    gdf = gpd.read_file(shp_path)
    print(f"  Columns: {gdf.columns.tolist()}")

    # Detect postcode column.
    # Shapefiles truncate column names to 10 chars: ZIP_CODE → ZIP_COD
    plz_col = next(
        (c for c in gdf.columns
         if c.lower() in ["zip_cod", "zip_code", "postal5", "plz",
                          "postal_num", "target_id", "zip"]),
        None
    )
    if plz_col is None:
        print(f"  ⚠  Cannot identify postcode column. Skipping.")
        continue
    print(f"  Postcode column: '{plz_col}'")

    gdf["target_id"] = pd.to_numeric(gdf[plz_col], errors="coerce").astype("Int64")

    # ── 3b. Merge price index ─────────────────────────────────────────────────
    merged = gdf.merge(
        idx_yr[["target_id", WEIGHT_COL, "target_X", "target_Y"]],
        on="target_id", how="inner"
    ).dropna(subset=["target_X", "target_Y", WEIGHT_COL])
    merged = merged[merged[WEIGHT_COL] > 0]

    n = len(merged)
    print(f"  Matched {n} postcodes with price data")
    if n < 5:
        print(f"  ⚠  Too few observations ({n}), skipping.")
        continue

    # ── 3c. Convert ETRS89/UTM32N → WGS84 ────────────────────────────────────
    lon_arr, lat_arr = transformer.transform(
        merged["target_X"].values,
        merged["target_Y"].values
    )
    pts = pd.DataFrame({
        "lon":       lon_arr,
        "lat":       lat_arr,
        WEIGHT_COL:  merged[WEIGHT_COL].values,
        "target_id": merged["target_id"].values,
    })

    # ── 3d. Run AABPL algorithm ───────────────────────────────────────────────
    # The algorithm:
    #   1. For each postcode, sums price_weight of neighbours within RADIUS_M
    #   2. Compares each sum against a random-point null distribution
    #   3. Cells above K_PERCENTILE are labelled as cluster members
    #   4. Adjacent cluster cells are merged into named cluster polygons
    print(f"  Running AABPL (r={RADIUS_M}m, k={K_PERCENTILE}th pct)...")

    try:
        grid = detect_cluster_cells(
            pts              = pts,
            crs              = DEST_CRS,       # WGS84 input
            r                = RADIUS_M,
            c                = [WEIGHT_COL],   # column to aggregate
            k_th_percentile  = K_PERCENTILE,
            exclude_pt_itself= False,          # include own postcode in sum
            sum_suffix       = f"_{RADIUS_M}m",
            cluster_suffix   = CLUSTER_SUFFIX,
            x                = "lon",
            y                = "lat",
            silent           = False,
        )

        # ── 3e. Extract dominant cluster centroid ─────────────────────────────
        # grid.clustering.by_column is keyed by the BASE column name, not by
        # the full "<col>_<suffix>_cluster" string.  AABPL's create_clusters
        # stores results as self.by_column[column] where column = "price_weight"
        # (see clusters.py Clustering.create_clusters line ~152).
        cluster_col  = WEIGHT_COL
        clusters_df  = grid.create_clusters_df_for_column(
            cluster_column = cluster_col,
            target_crs     = "EPSG:4326",
        )

        if len(clusters_df) == 0:
            raise ValueError(
                "No clusters found. Try lowering K_PERCENTILE (e.g. 99.0) "
                "or increasing RADIUS_M."
            )

        print(f"  ✓ {len(clusters_df)} cluster(s) found:")
        print(clusters_df[["cluster_id", "sum", "n_cells",
                            "centroid_x", "centroid_y"]].to_string(index=False))

        # Dominant cluster = highest total price weight (= CBD by theory)
        dominant     = clusters_df.loc[clusters_df["sum"].idxmax()]
        cbd_lon      = dominant["centroid_x"]
        cbd_lat      = dominant["centroid_y"]
        weight_share = dominant["sum"] / clusters_df["sum"].sum()
        method       = (f"AABPL dominant cluster centroid "
                        f"(r={RADIUS_M}m, k={K_PERCENTILE}, "
                        f"share={weight_share:.1%})")

        print(f"\n  ★ CBD → lon={cbd_lon:.5f}, lat={cbd_lat:.5f}")
        if len(clusters_df) > 1:
            print(f"  ⚠  Polycentric: {len(clusters_df)} clusters detected.")
            print(f"     Using dominant cluster only (monocentric assumption).")
            print(f"     → Flag this as a limitation in Task 4!")

    except Exception as e:
        # ── Fallback: price-weighted centroid of top-10% postcodes ───────────
        print(f"  ⚠  AABPL error: {e}")
        print(f"  Fallback: price-weighted centroid (top 10% postcodes)...")
        threshold = pts[WEIGHT_COL].quantile(0.90)
        top_pts   = pts[pts[WEIGHT_COL] >= threshold]
        w         = top_pts[WEIGHT_COL].values
        cbd_lon   = np.average(top_pts["lon"].values, weights=w)
        cbd_lat   = np.average(top_pts["lat"].values, weights=w)
        method    = "Price-weighted centroid (top 10% — AABPL fallback)"
        print(f"  ★ CBD (fallback) → lon={cbd_lon:.5f}, lat={cbd_lat:.5f}")

    # ── 3f. Save ──────────────────────────────────────────────────────────────
    city_dir = CBD_DIR / city
    city_dir.mkdir(exist_ok=True)
    pd.DataFrame([{
        "city": city, "cbd_lon": cbd_lon, "cbd_lat": cbd_lat,
        "method": method, "n_postcodes": n,
    }]).to_csv(city_dir / f"{city}_cbd_centroid.csv", index=False)

    results[city] = {"cbd_lon": cbd_lon, "cbd_lat": cbd_lat, "method": method}

# =============================================================================
# STEP 4 — Summary
# =============================================================================
print(f"\n{'='*60}")
print("SUMMARY — CBD Coordinates")
print(f"{'='*60}")

if not results:
    print("⚠  No cities processed successfully.")
    print("   Check shapefile paths and column names printed above.")
else:
    summary = pd.DataFrame([
        {"city": city, **vals} for city, vals in results.items()
    ])
    summary_path = CBD_DIR / "all_cities_cbd_centroids.csv"
    summary.to_csv(summary_path, index=False)
    print(summary[["city", "cbd_lon", "cbd_lat"]].to_string(index=False))
    print(f"\n→ Saved to: {summary_path}")
    print("\nNext → Task 3b: load 'all_cities_cbd_centroids.csv',")
    print("  compute Haversine distance from each postcode to its city CBD,")
    print("  then regress:  log(price) ~ dist_to_cbd  (log-level model)")