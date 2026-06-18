"""
geo.py — geometry alignment and mapping (fixes the positional-join bug).

The ARSW MAPIT.m maps a block-level vector to the shapefile *by position*
(SHAPE(i).SHOW = variable(i)), and Berlin4matlab.shp is sorted identically to the
.mat / caldata block order. The original Final.py / helpers.py broke this by dropping
"invalid" geometries and calling reset_index, which shifts every block after the dropped
row. Berlin4matlab.shp has 12309 features of which exactly 1 (canonical index 3100,
STAT_BLOCK 53632, a real West block with employment) is dropped on reprojection.

This module:
  * load_geometry      -> repairs invalid geometry IN PLACE, never drops a row,
                          asserts len == N (so the positional join is exact);
  * surviving_block_order -> reproduces Final.py's cleaning to learn which canonical
                          block each user-TTM centroid_i corresponds to;
  * realign_user_ttm   -> scatters an (M×M) user TTM back into a full (N×N) matrix in
                          canonical order, fills dropped/disconnected entries
                          (nearest-block), zeroes the diagonal, returns NaN-free output;
  * plot_block_map / plot_comparison -> Jenks-style choropleths matching MAPIT's look.
"""
from __future__ import annotations
from pathlib import Path
import numpy as np
import geopandas as gpd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import mapclassify


def _valid_coords(geom):
    try:
        b = geom.bounds
        return len(b) == 4 and not (np.any(np.isnan(b)) or np.any(np.isinf(b)))
    except Exception:
        return False


def load_geometry(shp_path, n_canon, work_epsg=25833):
    """Load the block shapefile in CANONICAL order. Invalid geometries are repaired
    in place (make_valid; reprojection-corrupted ones replaced by a placeholder that is
    coloured as 0/NaN). Returns a GeoDataFrame of length exactly n_canon: row i is model
    block i."""
    gdf = gpd.read_file(shp_path).reset_index(drop=True)
    if len(gdf) != n_canon:
        raise ValueError(f"{Path(shp_path).name} has {len(gdf)} features, expected {n_canon}")
    gdf["geometry"] = gdf.geometry.make_valid()
    gdf = gdf.to_crs(epsg=work_epsg)
    bad = (~gdf.geometry.apply(_valid_coords)) | gdf.geometry.isna() | gdf.geometry.is_empty
    if bad.any():
        good_geom = gdf.geometry[~bad].iloc[0]
        for i in np.where(bad.values)[0]:
            gdf.at[i, "geometry"] = good_geom           # placeholder only; value masked in maps
    gdf["_bad_geom"] = bad.values
    assert len(gdf) == n_canon
    return gdf


def surviving_block_order(shp_path, work_epsg=25833):
    """Reproduce Final.py's clean-and-reset_index. Returns (surviving_orig_pos, n_orig):
    user-TTM centroid_i corresponds to canonical block surviving_orig_pos[i]."""
    gdf = gpd.read_file(shp_path).reset_index(drop=True)
    gdf["__pos"] = np.arange(len(gdf))
    n_orig = len(gdf)
    g = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty].copy().reset_index(drop=True)
    g = g[g.geometry.apply(_valid_coords)].reset_index(drop=True)
    g["geometry"] = g.geometry.make_valid()
    g = g.to_crs(epsg=work_epsg)
    keep = g.geometry.notnull() & ~g.geometry.is_empty & g.geometry.apply(_valid_coords)
    g = g[keep].reset_index(drop=True)
    return g["__pos"].values.astype(int), n_orig


def realign_user_ttm(user_M, shp_path, n_canon, fill="nearest", verbose=True):
    """Return a full (n_canon × n_canon) float64 travel-time matrix in canonical order.

    Handles three cases:
      * user_M is already n_canon×n_canon  -> used as-is (Final.py was fixed);
      * user_M is (#surviving)×(#surviving) -> scattered to canonical positions, the
        dropped block(s) filled from their nearest surviving neighbour;
      * otherwise -> error with guidance.
    Remaining NaN (graph-disconnected pairs) are filled per-row with that row's worst-case
    finite time; the diagonal is forced to 0. Output contains no NaN."""
    user_M = np.asarray(user_M, float)
    M = user_M.shape[0]
    if M == n_canon:
        full = user_M.copy()
    else:
        pos, n_orig = surviving_block_order(shp_path)
        if n_orig != n_canon:
            raise ValueError(f"shapefile {n_orig} != canonical {n_canon}")
        if M != len(pos):
            raise ValueError(
                f"user TTM is {M}×{M}; cleaning Berlin4matlab.shp implies {len(pos)} "
                f"surviving blocks and {n_canon} canonical blocks. Rebuild the TTM from "
                f"the SAME Berlin4matlab.shp (and do not drop rows).")
        full = np.full((n_canon, n_canon), np.nan)
        full[np.ix_(pos, pos)] = user_M
        missing = np.setdiff1d(np.arange(n_canon), pos)
        if len(missing) and fill == "nearest":
            gdf = load_geometry(shp_path, n_canon)
            cen = gdf.geometry.centroid
            cx, cy = cen.x.values, cen.y.values
            for m in missing:
                d = np.hypot(cx[pos] - cx[m], cy[pos] - cy[m])
                nb = pos[int(np.argmin(d))]
                full[m, :] = full[nb, :]
                full[:, m] = full[:, nb]
            if verbose:
                print(f"  realign: filled {len(missing)} dropped block(s) "
                      f"{missing.tolist()} from nearest neighbour")
    # disconnected pairs
    if np.isnan(full).any():
        finite = np.where(np.isfinite(full), full, np.nan)
        rmax = np.nanmax(finite, axis=1)
        gmax = np.nanmax(rmax[np.isfinite(rmax)])
        nrep = int(np.isnan(full).sum())
        for i in range(n_canon):
            nanm = np.isnan(full[i])
            if nanm.any():
                full[i, nanm] = rmax[i] if np.isfinite(rmax[i]) else gmax
        if verbose:
            print(f"  realign: filled {nrep} disconnected pair(s) with worst-case time")
    np.fill_diagonal(full, 0.0)
    return full


# ---------------------------------------------------------------------------
# Mapping (MAPIT.m look: yellow->red Jenks classes, zero/NaN white)
# ---------------------------------------------------------------------------
_YL_RD = LinearSegmentedColormap.from_list("ylrd", [(1, 1, 0), (1, 0, 0)], N=256)


def _classify(values, mask, k=10):
    v = values[mask]
    v = v[np.isfinite(v) & (v != 0)]
    if len(np.unique(v)) <= k:
        return mapclassify.UserDefined(values, bins=np.unique(v)).bins
    return mapclassify.NaturalBreaks(v, k=k).bins


def plot_block_map(gdf, values, title, out_path, k=10, bezirke_path=None,
                   log1p=False, dpi=150):
    """Positional choropleth of a block vector. gdf row i ↔ values[i]. Blocks with
    value 0 / NaN / bad geometry are white (as in MAPIT)."""
    values = np.asarray(values, float).copy()
    if log1p:
        values = np.log(np.clip(values, 0, None) + 1)
    g = gdf.copy()
    g["v"] = values
    show = np.isfinite(values) & (values != 0) & (~g["_bad_geom"].values)
    fig, ax = plt.subplots(figsize=(8, 8))
    if (~show).any():
        g.loc[~show].plot(color="white", linewidth=0, ax=ax)
    if show.any():
        gv = g.loc[show]
        gv.plot(column="v", scheme="natural_breaks", k=min(k, gv["v"].nunique()),
                cmap=_YL_RD, linewidth=0, ax=ax, legend=True,
                legend_kwds={"loc": "lower right", "fontsize": 7, "title": "Jenks"})
    if bezirke_path and Path(bezirke_path).exists():
        try:
            bz = gpd.read_file(bezirke_path).to_crs(gdf.crs)
            bz.boundary.plot(ax=ax, color="black", linewidth=0.25)
        except Exception:
            pass
    ax.set_axis_off(); ax.set_title(title, fontsize=12)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=dpi, bbox_inches="tight")
    plt.close(fig)
    return out_path


def plot_comparison(gdf, values_a, values_b, label_a, label_b, suptitle, out_path,
                    k=10, bezirke_path=None, log1p=False, dpi=150):
    """Two block choropleths side by side on a COMMON colour scale (Jenks breaks from the
    pooled positive values) for an honest visual comparison (Task 1d)."""
    va = np.asarray(values_a, float).copy()
    vb = np.asarray(values_b, float).copy()
    if log1p:
        va = np.log(np.clip(va, 0, None) + 1)
        vb = np.log(np.clip(vb, 0, None) + 1)
    pooled = np.concatenate([va, vb])
    pooled = pooled[np.isfinite(pooled) & (pooled != 0)]
    kk = min(k, len(np.unique(pooled)))
    breaks = mapclassify.NaturalBreaks(pooled, k=kk).bins
    fig, axes = plt.subplots(1, 2, figsize=(15, 8))
    for ax, vals, lab in [(axes[0], va, label_a), (axes[1], vb, label_b)]:
        g = gdf.copy(); g["v"] = vals
        show = np.isfinite(vals) & (vals != 0) & (~g["_bad_geom"].values)
        if (~show).any():
            g.loc[~show].plot(color="white", linewidth=0, ax=ax)
        if show.any():
            g.loc[show].plot(column="v", cmap=_YL_RD, linewidth=0, ax=ax,
                             classification_kwds={"bins": breaks},
                             scheme="user_defined", legend=False)
        if bezirke_path and Path(bezirke_path).exists():
            try:
                bz = gpd.read_file(bezirke_path).to_crs(gdf.crs)
                bz.boundary.plot(ax=ax, color="black", linewidth=0.25)
            except Exception:
                pass
        ax.set_axis_off(); ax.set_title(lab, fontsize=12)
    fig.suptitle(suptitle, fontsize=14)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=dpi, bbox_inches="tight")
    plt.close(fig)
    return out_path
