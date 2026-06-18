"""
config.py — paths and model parameters for the Topic 7 ARSW pipeline.

Self-contained rebuild of Tutorial 7, Task 1 (quantification of the ARSW 2015 model).
Disconnected from Topic_7/arsw_python and the task_1*.ipynb notebooks.

All paths resolve relative to the repository root (three levels up from this file:
arsw_pipeline -> Topic_7 -> QSE-Tutorial), so the pipeline runs regardless of the
working directory.

Canonical block ordering
-------------------------
Every block-indexed object in this pipeline is keyed to the row order of
`caldata2006_ren.csv` (== caldata1986_ren.csv == Berlin4matlab.shp feature order ==
the .mat block order). This was verified empirically: block_id matches the shapefile
STAT_BLOCK column 1:1, positionally. Never reorder or drop blocks.
"""
from __future__ import annotations
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# --- canonical replication-directory CSV inputs (full ARSW replication dir) ---
_RD = REPO_ROOT / "Data" / "FinalBerlin" / "matlab" / "data"
CALDATA_2006   = _RD / "caldata2006_ren.csv"          # block data, canonical order
CALDATA_1986   = _RD / "caldata1986_ren.csv"          # block data (row-aligned to 2006)
TTFINAL_2006   = _RD / "ttfinal_2006_ren.csv"         # ARSW block travel times 2006 (big)
TTFINAL_1986   = _RD / "ttfinal_1986_ren.csv"         # ARSW block travel times 1986 (big)
BILAT_COMMUTE  = _RD / "bilat_commute_bzk.csv"        # 12x12 commuter counts
BILAT_MINUTES  = _RD / "bilat_minutes_bzk.csv"        # 12x12 ARSW Bezirke travel times
BILAT_PROB     = _RD / "bilat_probcommute_bzk.csv"    # 12x12 commuting probabilities

# --- ARSW teaching-toolkit inputs ---
_TK = REPO_ROOT / "ARSW2015" / "ARSW2015-toolkit"
WAGEWORKER_1986 = _TK / "matlab" / "data" / "input" / "wageworker1986.csv"
PREPDATA_TD     = _TK / "matlab" / "data" / "input" / "prepdata_big_TD.mat"     # has tt06
PREPDATA_TD86   = _TK / "matlab" / "data" / "input" / "prepdata_big_TD86.mat"   # has tt86rw
SHP_BERLIN      = _TK / "shapefile" / "Berlin4matlab.shp"        # 12309 blocks, canonical
SHP_WESTBERLIN  = _TK / "shapefile" / "WestBerlin4matlab.shp"    # 7050 blocks
SHP_BEZIRKE23   = _TK / "shapefile" / "Bezirke23.shp"            # boundaries (overlay)

# --- the student's own travel-time matrix (Topic 6 output) ---
TTM_DIR        = REPO_ROOT / "Topic_7_8" / "TTM"
#USER_TTM_FULL  = TTM_DIR / "sample_travel_time_matrix.parquet"        # all modes
USER_TTM_FULL= TTM_DIR / "updated_u5_travel_time_matrix.parquet"    # all modes
USER_TTM_SIMPLE= TTM_DIR / "simplified_travel_time_matrix.parquet"    # S/U-Bahn + walk


# --- outputs ---
OUT_DIR = REPO_ROOT / "Topic_7_8" / "arsw_pipeline" / "output"
FIG_DIR = REPO_ROOT / "Topic_7_8" / "arsw_pipeline" / "figs"

# --- caldata column layout (0-based), per prepdata_TD.m legend ---
CALDATA_COLS = ["block_id","area_id","bezirk1937","year","dummywest","latitude",
                "longitude","floor","gfz","finhalt","factories","emp_wpl","emp_rsd",
                "distCBD","d_wall","d_outb","USB36","USB86","dt1936","distku",
                "x_coord","y_coord"]

# --- model parameters (ARSW 2015 / toolkit defaults) ---
ALPHA    = 0.80     # labour share in production
BETA     = 0.75     # 1-beta = housing share in consumption
KAPPAEPS_DEFAULT = 0.07   # reduced-form eps*kappa (overwritten by gravity estimate)
EPSILON_DEFAULT  = 6.83   # Frechet shape (overwritten by Task 1b estimate)

# canonical sizes (verified)
NOBS_2006 = 12309
NOBS_1986_WEST = 7050

WORK_EPSG = 25833   # ETRS89 / UTM 33N — metric CRS for Berlin
