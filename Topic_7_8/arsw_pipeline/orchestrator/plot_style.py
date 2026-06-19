"""
plot_style.py — beamer-friendly LaTeX/Palatino matplotlib styling for the
orchestrator's figures. Call `set_rcparams()` once at script startup before
any figure is created. Figure-size constants give 16:9-ish aspect ratios sized
for a beamer slide with a thin frame-title bar (effective canvas roughly 18:9).
"""
from __future__ import annotations
import matplotlib.pyplot as plt
import seaborn as sns


# ---------------------------------------------------------------------------
# Figure sizes (inches). All chosen so the saved PNG fills a 16:9 beamer slide
# with room for a frame title; effective aspect ratio of the canvas is ~18:9.
# ---------------------------------------------------------------------------
FIGSIZE_SINGLE_MAP    = (12.0, 6.0)   # one Berlin map + vertical cbar on right
FIGSIZE_PAIRED_MAPS   = (16.0, 6.0)   # two Berlin maps + one shared cbar
FIGSIZE_TRIPLE_MAPS   = (18.0, 6.0)   # three maps + shared cbar
FIGSIZE_GRID_2x2      = (16.0, 9.0)   # 2x2 panel (HM, HR, wage, Crent)
FIGSIZE_HISTOGRAM     = (10.0, 5.0)
FIGSIZE_BAR           = (10.0, 5.0)
FIGSIZE_CONVERGENCE   = (10.0, 5.0)
FIGSIZE_SCATTER       = (8.0,  8.0)

# Colour conventions
CMAP_DIVERGING        = "RdBu_r"      # diverging, blue=neg, red=pos, white=0
CMAP_SEQ_PRODUCTIVITY = "YlOrRd"      # MAPIT-style sequential
CMAP_SEQ_AMENITY      = "YlGnBu"      # alternative; less collision with Greens overlay
CMAP_TIME             = "viridis_r"   # for raw travel-time maps
CMAP_SPEED            = "magma"

# Overlay alphas (the user noted that if the data cmap is in greens or blues
# the overlay alpha for that layer should be lowered; map_helpers exposes
# alpha_green / alpha_water kwargs to allow per-plot override).
DEFAULT_ALPHA_GREEN   = 0.35
DEFAULT_ALPHA_WATER   = 0.35
DEFAULT_ALPHA_STREETS = 0.35


def set_rcparams() -> None:
    """Apply LaTeX/Palatino + transparent-background styling globally.

    Idempotent. Safe to call multiple times.
    """
    sns.set_style("whitegrid")
    plt.rcParams.update({
        "font.size":          16.0,
        "font.family":        "serif",
        "font.serif":         ["Palatino", "Palatino Linotype", "URW Palladio L",
                               "TeX Gyre Pagella", "DejaVu Serif"],
        "axes.titlesize":     "medium",
        "figure.titlesize":   "large",
        "legend.fontsize":    "medium",
        "axes.labelsize":     "medium",
        "xtick.labelsize":    "small",
        "ytick.labelsize":    "small",
        "figure.dpi":         100,
        "savefig.dpi":        300,
        "figure.autolayout":  True,           # tight_layout by default
        "savefig.bbox":       "tight",        # crop whitespace aggressively
        "savefig.pad_inches": 0.05,           # minimal padding
        "text.usetex":        True,
        "text.latex.preamble":
            r"\usepackage{amsmath}\usepackage{amssymb}\usepackage{siunitx}[=v2]",
        # transparent background everywhere
        "savefig.transparent": True,
        "figure.facecolor":    "none",
        "axes.facecolor":      "none",
    })
