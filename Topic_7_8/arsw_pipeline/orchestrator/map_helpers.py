"""
map_helpers.py — orchestrator-specific plotting routines.

All routines:
  * accept a length-N block-indexed NumPy vector positionally joined to a
    length-N `gdf` (== `geo.load_geometry(SHP_BERLIN, NOBS_2006)`);
  * support an `overlays: Overlays` instance for greens/water/streets/bezirke;
  * save tight-bbox PNGs with transparent background (the rcParams set by
    `plot_style.set_rcparams` make this the default; the explicit
    `transparent=True` on `fig.savefig` is a belt-and-braces guarantee).

The internal `_draw_choropleth` ALWAYS lays down a uniform light-grey block
base at zorder 0 before any colored geometry — the fix for the "sparse-looking
map" bug where NaN/0 blocks render invisibly on a transparent figure.
"""
from __future__ import annotations
from pathlib import Path
from typing import Optional

import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.colors import Normalize, BoundaryNorm
import mapclassify

from .overlays import Overlays, add_overlays_to_ax
from .plot_style import (
    FIGSIZE_SINGLE_MAP, FIGSIZE_PAIRED_MAPS, FIGSIZE_GRID_2x2,
    FIGSIZE_HISTOGRAM, FIGSIZE_BAR, FIGSIZE_CONVERGENCE,
    CMAP_DIVERGING, CMAP_SEQ_PRODUCTIVITY, CMAP_SEQ_AMENITY,
    DEFAULT_ALPHA_GREEN, DEFAULT_ALPHA_WATER, DEFAULT_ALPHA_STREETS,
)


# ===========================================================================
# Internal: positional-join data renderer
# ===========================================================================
def _draw_choropleth(ax, gdf: gpd.GeoDataFrame, values: np.ndarray, *,
                     cmap, norm=None, classifier=None,
                     invalid_mask: Optional[np.ndarray] = None,
                     invalid_color: str = "#d0d0d0",
                     base_color:    str = "#e8e8e8"):
    """Render one panel: grey base → invalid greys → colored choropleth.
    Returns a ScalarMappable suitable for colorbar attachment."""
    if len(values) != len(gdf):
        raise ValueError(f"length mismatch: values={len(values)}, gdf={len(gdf)}")
    # zorder 0 — uniform base
    gdf.plot(ax=ax, color=base_color, linewidth=0, zorder=0)
    bad_geom = gdf.get("_bad_geom", None)
    bad_geom_mask = bad_geom.values.astype(bool) if bad_geom is not None \
        else np.zeros(len(gdf), dtype=bool)
    if invalid_mask is None:
        invalid_mask = ~np.isfinite(values)
    invalid_mask = np.asarray(invalid_mask, dtype=bool) | bad_geom_mask
    if invalid_mask.any():
        gdf.iloc[invalid_mask].plot(
            ax=ax, color=invalid_color, linewidth=0, zorder=1)
    show = ~invalid_mask
    sm = None
    if show.any():
        g = gdf.iloc[show].copy()
        v = np.asarray(values, float)[show]
        g["_v"] = v
        if classifier is not None:
            g.plot(column="_v", cmap=cmap, scheme="user_defined",
                   classification_kwds={"bins": list(classifier.bins)},
                   linewidth=0, ax=ax, zorder=1, legend=False)
            cmap_obj = plt.get_cmap(cmap)
            boundaries = np.concatenate([[v.min()], np.asarray(classifier.bins)])
            sm = ScalarMappable(cmap=cmap_obj,
                                norm=BoundaryNorm(boundaries, ncolors=cmap_obj.N))
            sm.set_array([])
        else:
            if norm is None:
                norm = Normalize(vmin=float(np.nanmin(v)),
                                 vmax=float(np.nanmax(v)))
            g.plot(column="_v", cmap=cmap, norm=norm,
                   linewidth=0, ax=ax, zorder=1, legend=False)
            sm = ScalarMappable(cmap=cmap, norm=norm)
            sm.set_array([])
    return sm


# ===========================================================================
# 1. Diverging %-change (single, paired, 2x2 grid)
# ===========================================================================
def plot_pcc_single(out_path, gdf, pcc, *, title, overlays,
                    truncate=(-50.0, 100.0), cmap=CMAP_DIVERGING,
                    cbar_label=r"\% change", figsize=FIGSIZE_SINGLE_MAP,
                    alpha_green=DEFAULT_ALPHA_GREEN,
                    alpha_water=DEFAULT_ALPHA_WATER,
                    alpha_streets=DEFAULT_ALPHA_STREETS) -> Path:
    pcc = np.asarray(pcc, float)
    lo, hi = float(truncate[0]), float(truncate[1])
    bad = ~np.isfinite(pcc)
    pcc_clip = np.where(bad, 0.0, np.clip(pcc, lo, hi))
    vmax_abs = max(abs(lo), abs(hi))
    norm = Normalize(vmin=-vmax_abs, vmax=+vmax_abs)
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    sm = _draw_choropleth(ax, gdf, pcc_clip, cmap=cmap, norm=norm,
                          invalid_mask=bad)
    add_overlays_to_ax(ax, overlays, alpha_green=alpha_green,
                       alpha_water=alpha_water, alpha_streets=alpha_streets)
    ax.set_axis_off()
    ax.set_title(title)
    if sm is not None:
        cb = fig.colorbar(sm, ax=ax, orientation="vertical",
                          shrink=0.7, pad=0.01)
        cb.set_label(cbar_label)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_pcc_paired(out_path, gdf, pcc_a, pcc_b, *,
                    title_a, title_b, suptitle=None, overlays,
                    truncate=(-50.0, 100.0), cmap=CMAP_DIVERGING,
                    cbar_label=r"\% change", figsize=FIGSIZE_PAIRED_MAPS,
                    alpha_green=DEFAULT_ALPHA_GREEN,
                    alpha_water=DEFAULT_ALPHA_WATER,
                    alpha_streets=DEFAULT_ALPHA_STREETS) -> Path:
    pcc_a = np.asarray(pcc_a, float); pcc_b = np.asarray(pcc_b, float)
    lo, hi = float(truncate[0]), float(truncate[1])
    vmax_abs = max(abs(lo), abs(hi))
    norm = Normalize(vmin=-vmax_abs, vmax=+vmax_abs)
    bad_a = ~np.isfinite(pcc_a); bad_b = ~np.isfinite(pcc_b)
    clip_a = np.where(bad_a, 0.0, np.clip(pcc_a, lo, hi))
    clip_b = np.where(bad_b, 0.0, np.clip(pcc_b, lo, hi))
    fig, axes = plt.subplots(1, 2, figsize=figsize, constrained_layout=True)
    sm = None
    for ax, vals, bad, sub in [(axes[0], clip_a, bad_a, title_a),
                                (axes[1], clip_b, bad_b, title_b)]:
        sm = _draw_choropleth(ax, gdf, vals, cmap=cmap, norm=norm,
                              invalid_mask=bad)
        add_overlays_to_ax(ax, overlays, alpha_green=alpha_green,
                           alpha_water=alpha_water, alpha_streets=alpha_streets)
        ax.set_axis_off()
        ax.set_title(sub)
    if suptitle:
        fig.suptitle(suptitle)
    if sm is not None:
        cb = fig.colorbar(sm, ax=axes.ravel().tolist(),
                          orientation="vertical", shrink=0.65, pad=0.01)
        cb.set_label(cbar_label)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_pcc_grid_2x2(out_path, gdf, pccs, *, titles, suptitle=None,
                      overlays, truncate=(-50.0, 100.0), cmap=CMAP_DIVERGING,
                      cbar_label=r"\% change", figsize=FIGSIZE_GRID_2x2,
                      alpha_green=DEFAULT_ALPHA_GREEN,
                      alpha_water=DEFAULT_ALPHA_WATER,
                      alpha_streets=DEFAULT_ALPHA_STREETS) -> Path:
    """`pccs` and `titles` are dicts with exactly 4 same-key entries; row-major panel order."""
    keys = list(pccs.keys())
    if len(keys) != 4:
        raise ValueError(f"plot_pcc_grid_2x2 needs 4 panels, got {len(keys)}")
    lo, hi = float(truncate[0]), float(truncate[1])
    vmax_abs = max(abs(lo), abs(hi))
    norm = Normalize(vmin=-vmax_abs, vmax=+vmax_abs)
    fig, axes = plt.subplots(2, 2, figsize=figsize, constrained_layout=True)
    sm = None
    for ax, key in zip(axes.ravel(), keys):
        v = np.asarray(pccs[key], float)
        bad = ~np.isfinite(v)
        clip = np.where(bad, 0.0, np.clip(v, lo, hi))
        sm = _draw_choropleth(ax, gdf, clip, cmap=cmap, norm=norm,
                              invalid_mask=bad)
        add_overlays_to_ax(ax, overlays, alpha_green=alpha_green,
                           alpha_water=alpha_water, alpha_streets=alpha_streets)
        ax.set_axis_off()
        ax.set_title(titles.get(key, key))
    if suptitle:
        fig.suptitle(suptitle)
    if sm is not None:
        cb = fig.colorbar(sm, ax=axes.ravel().tolist(),
                          orientation="vertical", shrink=0.6, pad=0.01)
        cb.set_label(cbar_label)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


# ===========================================================================
# 2. Sequential Jenks (single, paired with pooled breaks)
# ===========================================================================
def _jenks_classifier(values, *, k=10, positive_only=True):
    v = np.asarray(values, float)
    v = v[np.isfinite(v)]
    if positive_only:
        v = v[v != 0]
    if v.size == 0:
        return mapclassify.UserDefined(
            np.array([0.0, 1.0]), bins=np.array([0.5, 1.0]))
    n_unique = int(np.unique(v).size)
    kk = min(k, max(2, n_unique))
    if n_unique <= kk:
        return mapclassify.UserDefined(v, bins=np.unique(v))
    return mapclassify.NaturalBreaks(v, k=kk)


def plot_jenks_single(out_path, gdf, values, *, title, overlays,
                      cmap=CMAP_SEQ_PRODUCTIVITY, cbar_label="", k=10,
                      log1p=False, positive_only=True,
                      figsize=FIGSIZE_SINGLE_MAP,
                      alpha_green=DEFAULT_ALPHA_GREEN,
                      alpha_water=DEFAULT_ALPHA_WATER,
                      alpha_streets=DEFAULT_ALPHA_STREETS) -> Path:
    v = np.asarray(values, float).copy()
    if log1p:
        v = np.log(np.clip(v, 0, None) + 1)
    classifier = _jenks_classifier(v, k=k, positive_only=positive_only)
    bad = ((~np.isfinite(v)) | (v == 0)) if positive_only else (~np.isfinite(v))
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    sm = _draw_choropleth(ax, gdf, v, cmap=cmap, classifier=classifier,
                          invalid_mask=bad)
    add_overlays_to_ax(ax, overlays, alpha_green=alpha_green,
                       alpha_water=alpha_water, alpha_streets=alpha_streets)
    ax.set_axis_off()
    ax.set_title(title)
    if sm is not None:
        cb = fig.colorbar(sm, ax=ax, orientation="vertical",
                          shrink=0.7, pad=0.01,
                          ticks=list(classifier.bins))
        cb.set_label(cbar_label)
        cb.ax.set_yticklabels([f"{b:.3g}" for b in classifier.bins])
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_jenks_paired(out_path, gdf, values_a, values_b, *,
                      title_a, title_b, suptitle=None, overlays,
                      cmap=CMAP_SEQ_PRODUCTIVITY, cbar_label="", k=10,
                      log1p=False, positive_only=True,
                      figsize=FIGSIZE_PAIRED_MAPS,
                      alpha_green=DEFAULT_ALPHA_GREEN,
                      alpha_water=DEFAULT_ALPHA_WATER,
                      alpha_streets=DEFAULT_ALPHA_STREETS) -> Path:
    """Pooled Jenks breaks across both arrays → panels are directly comparable."""
    va = np.asarray(values_a, float).copy()
    vb = np.asarray(values_b, float).copy()
    if log1p:
        va = np.log(np.clip(va, 0, None) + 1)
        vb = np.log(np.clip(vb, 0, None) + 1)
    pooled = np.concatenate([va, vb])
    classifier = _jenks_classifier(pooled, k=k, positive_only=positive_only)
    bad_a = ((~np.isfinite(va)) | (va == 0)) if positive_only else (~np.isfinite(va))
    bad_b = ((~np.isfinite(vb)) | (vb == 0)) if positive_only else (~np.isfinite(vb))
    fig, axes = plt.subplots(1, 2, figsize=figsize, constrained_layout=True)
    sm = None
    for ax, vals, bad, sub in [(axes[0], va, bad_a, title_a),
                                (axes[1], vb, bad_b, title_b)]:
        sm = _draw_choropleth(ax, gdf, vals, cmap=cmap, classifier=classifier,
                              invalid_mask=bad)
        add_overlays_to_ax(ax, overlays, alpha_green=alpha_green,
                           alpha_water=alpha_water, alpha_streets=alpha_streets)
        ax.set_axis_off()
        ax.set_title(sub)
    if suptitle:
        fig.suptitle(suptitle)
    if sm is not None:
        cb = fig.colorbar(sm, ax=axes.ravel().tolist(),
                          orientation="vertical", shrink=0.65, pad=0.01,
                          ticks=list(classifier.bins))
        cb.set_label(cbar_label)
        cb.ax.set_yticklabels([f"{b:.3g}" for b in classifier.bins])
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


# ===========================================================================
# 3. Auxiliary plots (convergence, TTM diff distribution, welfare bar)
# ===========================================================================
def plot_convergence(out_path, cpath, *, title,
                     figsize=FIGSIZE_CONVERGENCE) -> Path:
    """Convergence path (log-scale max |Δlog| per target vs iteration).

    `cpath` is the solver's `result["cpath"]` ndarray with columns
    [maxLD_wage, maxLD_q, maxLD_Q, maxLD_theta, maxLD, MLSE, iter].
    """
    cp = np.asarray(cpath, float)
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    if cp.size > 0:
        ax.semilogy(cp[:, 6], np.maximum(cp[:, 0], 1e-12), label=r"max $|\Delta\log w|$")
        ax.semilogy(cp[:, 6], np.maximum(cp[:, 1], 1e-12), label=r"max $|\Delta\log q|$", linestyle="--")
        ax.semilogy(cp[:, 6], np.maximum(cp[:, 2], 1e-12), label=r"max $|\Delta\log Q|$", linestyle=":")
        ax.semilogy(cp[:, 6], np.maximum(cp[:, 3], 1e-12), label=r"max $|\Delta\log\theta|$", linestyle="-.")
    ax.set_xlabel("iteration"); ax.set_ylabel(r"max $|\Delta\log|$ (rounded 2dp)")
    ax.set_title(title)
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(loc="upper right")
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_ttm_diff_histogram(out_path, tt_ref, tt_alt, *,
                            title, ref_label, alt_label,
                            sample_size: int = 2_000_000,
                            figsize=FIGSIZE_HISTOGRAM,
                            seed: int = 1) -> Path:
    """Histogram of (tt_ref - tt_alt) on a random sample of OD pairs.

    Positive values = ALT is faster than REF for that OD pair (= time saved by
    switching from REF to ALT). Histogram excludes the diagonal (always 0).
    """
    N = tt_ref.shape[0]
    rng = np.random.default_rng(seed)
    flat_size = N * N - N
    sample_size = min(sample_size, flat_size)
    idx = rng.integers(0, flat_size, size=sample_size)
    i = idx // (N - 1)
    j = idx % (N - 1)
    j = j + (j >= i)   # skip diagonal
    diffs = (tt_ref[i, j] - tt_alt[i, j]).astype(np.float64)

    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    ax.hist(diffs, bins=100, color="tab:blue", alpha=0.7, density=True,
            edgecolor="none")
    ax.axvline(0.0, color="black", linewidth=0.8, linestyle="--",
               label=r"no change")
    ax.axvline(float(np.mean(diffs)), color="tab:red", linewidth=1.0,
               label=rf"mean $= {np.mean(diffs):+.3f}$ min")
    ax.axvline(float(np.median(diffs)), color="tab:green", linewidth=1.0,
               label=rf"median $= {np.median(diffs):+.3f}$ min")
    ax.set_xlabel(rf"$\tau_{{ij}}^{{\mathrm{{{ref_label}}}}} - \tau_{{ij}}^{{\mathrm{{{alt_label}}}}}$ (minutes)")
    ax.set_ylabel("density")
    ax.set_title(title)
    ax.legend(loc="upper right")
    ax.grid(True, alpha=0.3)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_aggregate_bar(out_path, *, labels, values, ylabel, title,
                       value_fmt: str = "{:+.3f}",
                       colors=None, figsize=FIGSIZE_BAR) -> Path:
    """Simple bar plot (e.g. ΔGDP%, ΔŪ%, ΔWATT%)."""
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    x = np.arange(len(labels))
    cols = colors or [("tab:blue" if v >= 0 else "tab:red") for v in values]
    bars = ax.bar(x, values, color=cols, alpha=0.85, edgecolor="black", linewidth=0.6)
    ax.axhline(0.0, color="black", linewidth=0.6)
    ax.set_xticks(x); ax.set_xticklabels(labels, rotation=0)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.grid(True, axis="y", alpha=0.3)
    ymax = max(abs(v) for v in values) if values else 1.0
    for b, v in zip(bars, values):
        ax.text(b.get_x() + b.get_width() / 2,
                v + (0.04 * ymax * (1 if v >= 0 else -1)),
                value_fmt.format(v),
                ha="center",
                va=("bottom" if v >= 0 else "top"),
                fontsize=12)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_welfare_comparison(out_path, *, model_delta_gdp_eur,
                            planner_monetised: dict,
                            title, figsize=FIGSIZE_BAR) -> Path:
    """Side-by-side bars: model ΔGDP vs transport-planner monetised time savings
    under several VoT scenarios."""
    labels = [r"Model $\Delta$GDP"] + \
             [rf"Planner ({k.replace('vot_','').replace('pct','\\%')} of wage)"
              for k in planner_monetised]
    values = [model_delta_gdp_eur] + list(planner_monetised.values())
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    x = np.arange(len(labels))
    cols = ["tab:blue"] + ["tab:orange"] * len(planner_monetised)
    bars = ax.bar(x, values, color=cols, alpha=0.85, edgecolor="black", linewidth=0.6)
    ax.axhline(0.0, color="black", linewidth=0.6)
    ax.set_xticks(x); ax.set_xticklabels(labels, rotation=15, ha="right")
    ax.set_ylabel(r"\EUR{} per year")
    ax.set_title(title)
    ax.grid(True, axis="y", alpha=0.3)
    ymax = max(abs(v) for v in values) if values else 1.0
    for b, v in zip(bars, values):
        ax.text(b.get_x() + b.get_width() / 2,
                v + (0.04 * ymax * (1 if v >= 0 else -1)),
                f"{v:+.2e}", ha="center",
                va=("bottom" if v >= 0 else "top"), fontsize=11)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)


def plot_watt_bar(out_path, *, watt_ref_min, watt_alt_min,
                  ref_label, alt_label, title,
                  figsize=FIGSIZE_BAR) -> Path:
    """Two-bar plot: WATT before/after (minutes), with the % change in the title."""
    delta_pct = (watt_alt_min / watt_ref_min - 1.0) * 100.0
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=True)
    x = np.arange(2)
    bars = ax.bar(x, [watt_ref_min, watt_alt_min],
                  color=["tab:blue", "tab:orange"], alpha=0.85,
                  edgecolor="black", linewidth=0.6)
    ax.set_xticks(x); ax.set_xticklabels([ref_label, alt_label])
    ax.set_ylabel("WATT (minutes)")
    ax.set_title(rf"{title}  ($\Delta = {delta_pct:+.2f}\%$)")
    ax.grid(True, axis="y", alpha=0.3)
    for b, v in zip(bars, [watt_ref_min, watt_alt_min]):
        ax.text(b.get_x() + b.get_width() / 2, v,
                f"{v:.2f}", ha="center", va="bottom", fontsize=12)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, transparent=True)
    plt.close(fig)
    return Path(out_path)
