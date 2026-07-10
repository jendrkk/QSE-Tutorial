"""
mapping.py — county choropleths in the project's standard style.

Layering:  grey background (zorder 1) -> data (zorder 2) -> state overlay (zorder 3).
Sequential Jenks natural breaks for level maps (log A_n, log b_n); a symmetric
diverging scale centred at 0 for counterfactual log-changes. Serif typography,
16:9 Beamer aspect, high DPI, tight bounding box, transparent background.
"""
from __future__ import annotations
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
import geopandas as gpd

from . import config as C

# --- typography: prefer a Palatino-like serif, fall back gracefully --------- #
_SERIF = ["Palatino", "TeX Gyre Pagella", "URW Palladio L", "DejaVu Serif", "serif"]
_avail = {f.name for f in font_manager.fontManager.ttflist}
plt.rcParams.update({
    "font.family": "serif",
    "font.serif": [f for f in _SERIF if f in _avail] or ["serif"],
    "mathtext.fontset": "dejavuserif",
    "axes.edgecolor": "#333333",
    "savefig.transparent": True,
    "figure.dpi": 200,
})

_county_gdf = None
_states_gdf = None


def _load_shapes():
    global _county_gdf, _states_gdf
    if _county_gdf is None:
        _county_gdf = gpd.read_file(C.SHAPE_DIR / f"{C.SHP_COUNTY}.shp")
        _county_gdf["county_id"] = _county_gdf["county_id"].astype(int)
    if _states_gdf is None:
        try:
            _states_gdf = gpd.read_file(C.SHAPE_DIR / f"{C.SHP_STATES}.shp")
        except Exception:
            _states_gdf = None
    return _county_gdf, _states_gdf


def _frame(gdf, values, keep):
    s = dict(zip(keep, values))
    g = gdf.copy()
    g["val"] = g["county_id"].map(s)
    return g


def choropleth(values, keep, title, fname,
               diverging=False, k=7, cmap=None, label=None):
    """Render one choropleth to FIGS_DIR/fname.png. Returns the full path."""
    county, states = _load_shapes()
    g = _frame(county, np.asarray(values, float), keep)
    plotted = g[g["val"].notna()]

    fig, ax = plt.subplots(figsize=(7.5, 4.2))          # ~16:9
    # background
    county.plot(ax=ax, color="#d9d9d9", edgecolor="none", zorder=1)

    if diverging:
        vmax = np.nanmax(np.abs(plotted["val"].values))
        norm = matplotlib.colors.TwoSlopeNorm(vmin=-vmax, vcenter=0.0, vmax=vmax)
        plotted.plot(ax=ax, column="val", cmap=cmap or "RdBu_r", norm=norm,
                     linewidth=0.05, edgecolor="#555555", zorder=2,
                     legend=True,
                     legend_kwds={"shrink": 0.6, "label": label or "",
                                  "orientation": "vertical"})
    else:
        plotted.plot(ax=ax, column="val", cmap=cmap or "viridis",
                     scheme="NaturalBreaks", k=k,
                     linewidth=0.05, edgecolor="#555555", zorder=2,
                     legend=True,
                     legend_kwds={"loc": "lower left", "fontsize": 6,
                                  "frameon": False, "title": label or ""})
    # state overlay
    if states is not None:
        states.boundary.plot(ax=ax, color="#ffffff", linewidth=0.6, zorder=3)

    ax.set_title(title, fontsize=11)
    ax.set_axis_off()
    ax.margins(0)
    out = C.FIGS_DIR / f"{fname}.png"
    fig.savefig(out, bbox_inches="tight", pad_inches=0.02, dpi=300)
    plt.close(fig)
    return out