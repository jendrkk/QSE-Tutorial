"""
plots.py — non-map summary figures.

border_scatter: log-change of an outcome against signed distance to the former
inner-German border (West negative, East positive), with a vertical line at 0.
Mirrors the border-discontinuity scatters in scripts/Counterfactuals.m.

effect_bars: West vs East mean log-change bars for a set of outcomes, one grouped
panel per counterfactual.
"""
from __future__ import annotations
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from . import config as C

plt.rcParams.update({"font.family": "serif", "savefig.transparent": True})

_W = "#1f5c99"   # West
_E = "#c1440e"   # East


def border_scatter(borderdist, series: dict, title, fname, ylabel="Log change"):
    """series: {label: values}. Returns output path."""
    fig, ax = plt.subplots(figsize=(7.0, 4.0))
    markers = ["o", "s", "^", "D", "v"]
    for (lab, y), m in zip(series.items(), markers):
        ax.scatter(borderdist, np.log(y), s=10, marker=m, alpha=0.7, label=lab,
                   edgecolors="none")
    ax.axvline(0, color="k", lw=1.2)
    ax.axhline(0, color="k", lw=0.8, alpha=0.5)
    ax.set_xlabel("Distance to inner-German border (km); West $<0<$ East")
    ax.set_ylabel(ylabel)
    ax.set_title(title, fontsize=11)
    ax.legend(fontsize=8, frameon=False, loc="best")
    ax.grid(True, alpha=0.25)
    out = C.FIGS_DIR / f"{fname}.png"
    fig.savefig(out, bbox_inches="tight", dpi=300)
    plt.close(fig)
    return out


def effect_bars(labels, west_vals, east_vals, title, fname, ylabel="Mean log change"):
    """Grouped West/East bars over outcomes. Returns output path."""
    x = np.arange(len(labels))
    w = 0.38
    fig, ax = plt.subplots(figsize=(7.0, 4.0))
    ax.bar(x - w / 2, west_vals, w, color=_W, label="West")
    ax.bar(x + w / 2, east_vals, w, color=_E, label="East")
    ax.axhline(0, color="k", lw=0.8)
    ax.set_xticks(x)
    ax.set_xticklabels(labels, rotation=20, ha="right", fontsize=8)
    ax.set_ylabel(ylabel)
    ax.set_title(title, fontsize=11)
    ax.legend(fontsize=9, frameon=False)
    ax.grid(True, axis="y", alpha=0.25)
    out = C.FIGS_DIR / f"{fname}.png"
    fig.savefig(out, bbox_inches="tight", dpi=300)
    plt.close(fig)
    return out