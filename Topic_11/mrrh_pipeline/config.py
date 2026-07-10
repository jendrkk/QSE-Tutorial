"""
config.py — MRRH2018 / SW2020 pipeline for Topic 11, Task 1.

Central store for structural parameters, file paths, and analysis knobs.
Paths default to the local repository layout but can be overridden with the
environment variables MRRH_DATA_DIR / MRRH_TUT9_DIR / MRRH_SHAPE_DIR
(used for testing outside the repo). Nothing else in the pipeline hard-codes a
path.
"""
from __future__ import annotations
import os
from pathlib import Path

# --------------------------------------------------------------------------- #
# Structural parameters  (verbatim from scripts/MRRH2018_toolkit.m)
# --------------------------------------------------------------------------- #
ALP   = 0.70   # expenditure share on TRADABLES (Codebook Table-1 "land" label is a typo;
               # 1-ALP = 0.30 is the land/housing share, consistent with eqs 3 & 5)
EPSI  = 4.60   # Frechet shape (commuting/location dispersion)
MU    = 0.47   # travel-time elasticity of commuting cost   (phi = EPSI*MU = 2.162)
DELTA = 0.38   # housing supply elasticity
SIGG  = 4.00   # CES elasticity of substitution across varieties
FIXC  = 1.00   # fixed cost / normalisation in the price index
NU    = 0.05   # agglomeration (productivity spillover) elasticity
PSI   = 0.42   # trade-cost distance elasticity (only used if trade cost is rebuilt
               # from distance; here we use the Tutorial-9 trade-cost matrix directly)

PHI = EPSI * MU            # commuting decay in the cost matrix, tau^(-PHI)

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #
# Repo root for Topic 11 (…/QSE-Tutorial/Topic_11)
_TOPIC11 = Path(
    "/Users/jedrek/Documents/Studium Volkswirschaftslehre/4. Semester/"
    "Quantitive Spatial Economics/QSE-Tutorial/Topic_11"
)
_TOOLKIT = _TOPIC11 / "MRRH-toolkit" / "MRRH2018-toolkit"

# Toolkit input data (commuting, wages, house prices, area, border distance)
DATA_DIR  = Path(os.environ.get("MRRH_DATA_DIR",  _TOOLKIT / "data" / "input"))
# Tutorial-9 time-cost matrices (trade cost dni, travel time tau)
TUT9_DIR  = Path(os.environ.get("MRRH_TUT9_DIR",  _TOPIC11 / "data"))
# Shapefiles (county choropleth base + state boundaries overlay)
SHAPE_DIR = Path(os.environ.get("MRRH_SHAPE_DIR", _TOOLKIT / "shape"))

# Package-local outputs (created if missing)
PKG_DIR    = Path(__file__).resolve().parent
OUTPUT_DIR = Path(os.environ.get("MRRH_OUT_DIR", PKG_DIR / "output"))
FIGS_DIR   = Path(os.environ.get("MRRH_FIG_DIR", PKG_DIR / "figs"))

# Individual files
F_COMMUTING = "commuting_wide.csv"        # [work x res] flows, header+index = AGS
F_LABOR     = "labor_tidy.csv"            # region_id, median_income_workplace, ...
F_HOUSE     = "house_prices.csv"          # county_id, rentindex  (Q_n)
F_AREA      = "CountyArea.csv"            # county_id, Area (km^2)
F_BORDER    = "CountyBorderDist.csv"      # county_id, BorderDist (km to inner-German border)
F_ROUNDTRIP = "roundtrip_time_base.csv"   # toolkit own-time reference (unused by default)
F_TCMATR    = "tcmatr_de.csv"             # Tutorial-9 bilateral trade cost dni (diag = 1)
F_TTMATR    = "ttmatr_de.csv"             # Tutorial-9 bilateral travel time, minutes (diag = 0)

SHP_COUNTY  = "VG250_KRS_clean_final"     # 401 features, EPSG:25832, join key = county_id
SHP_STATES  = "states"

# --------------------------------------------------------------------------- #
# Analysis knobs
# --------------------------------------------------------------------------- #
# East definition: AGS state code (= AGS // 1000) >= EAST_STATE_MIN.
# 11=Berlin, 12=Brandenburg, 13=MV, 14=Sachsen, 15=Sachsen-Anhalt, 16=Thueringen.
# This exactly reproduces the toolkit's positional East(325:end) split on the full
# 401-county sample and is robust to dropping counties.
EAST_STATE_MIN = 11

# The Tutorial-9 travel-time matrix has a zero diagonal; the own-commute term
# tau_nn^(-PHI) dominates commuter market access, so tau_nn must be imputed.
# Convention: tau_nn = OWN_TIME_C * sqrt(Area_n / pi) / OWN_TIME_SPEED * 60  [minutes]
# i.e. average intra-county one-way trip = (2/3)*radius at a constant road speed.
# Swap-able; the West-East log-QoL difference is stable across speeds in {25,30,40}.
OWN_TIME_C     = 2.0 / 3.0
OWN_TIME_SPEED = 30.0     # km/h
OWN_TIME_FLOOR = 3.0      # minutes; guards against degenerate tiny cities

# Counterfactual convention: remove the systematic gap by moving EAST fundamentals
# to the WEST mean (gap = mean_West(log x) - mean_East(log x); apply exp(gap) to East).
# Set to "equalise_to_west" (only supported mode; documented for clarity).
GAP_MODE = "equalise_to_west"

# Solver tolerances
PROD_MAXITER = 5000
PROD_PREC    = 6          # rounding precision for |income-expenditure| == 0 test
CF_TOL       = 1e-4
CF_MAXITER   = 100_000
CF_RELAX     = 0.25       # convex-combination weight on the update (0.25 new / 0.75 old)


def ensure_dirs() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    FIGS_DIR.mkdir(parents=True, exist_ok=True)