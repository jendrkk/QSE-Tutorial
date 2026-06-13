# Topic 7 — ARSW (2015) quantification pipeline

Clean rebuild of Tutorial 7 / Task 1: gravity estimation of ε·κ, moment estimation of ε
(hence κ), and recovery + mapping of fundamental productivities **A** and amenities **B**,
with a comparison between the student's own travel-time matrix and ARSW's original one.

This folder is **self-contained** and intentionally disconnected from `Topic_7/arsw_python`
and the `task_1*.ipynb` notebooks.

## Run

```bash
cd "<repo>/Topic_7/arsw_pipeline"
python -m tests.test_synthetic     # 10-second numeric self-test (no big data)
python run_task1.py                # full a–d, all-modes user TTM, ARSW comparison via .mat
```

Useful flags:

| flag | effect |
|------|--------|
| `--ttm simple` | use `simplified_travel_time_matrix.parquet` instead of the all-modes one |
| `--skip-arsw` | run a–c only; do not load any ARSW block TTM (lighter on RAM) |
| `--arsw-source csv` | read the ARSW TTM from `ttfinal_2006_ren.csv` instead of `prepdata_big_TD.mat` |
| `--no-maps` | skip figure rendering |

## Outputs

```
output/
  task1a_gravity.csv          ε·κ by specification (own simple / own emp-weighted / ARSW)
  fundamentals_userTTM.{npz,csv}   A, B, wage, V, theta per block (own TTM)
  fundamentals_arswTTM.npz         A, B, wage, V, theta per block (ARSW TTM)
  summary.json                all estimates + diagnostics
figs/
  A_userTTM.png  B_userTTM.png     fundamentals maps (own TTM)
  compare_A.png  compare_B.png     own vs ARSW, common Jenks colour scale (Task 1d)
```

## The alignment bug this rebuild fixes

`MAPIT.m` joins a block vector to `Berlin4matlab.shp` **by position**, and the shapefile is
sorted identically to the `caldata*_ren.csv` / `.mat` block order (verified: `block_id` ==
shapefile `STAT_BLOCK`, 1:1). The old `Final.py`/`helpers.py` cleaned geometry and called
`reset_index`, which **drops exactly one block** — canonical index **3100** (`STAT_BLOCK`
53632, a real West block with employment) is invalidated on reprojection. That makes the
student's TTM `12308×12308` (so it fails the `shape == (12309, 12309)` check) and shifts
every block after 3100, misaligning the maps.

Fixes here:
* `geo.load_geometry` repairs invalid geometry **in place** and never drops a row
  (asserts `len == 12309`), so the positional join is exact;
* `geo.realign_user_ttm` reproduces the old cleaning to learn the surviving-block order,
  scatters the user TTM back into a full `12309×12309` canonical matrix, fills the dropped
  block from its nearest neighbour and any disconnected pairs with a worst-case time, and
  zeroes the diagonal — output is NaN-free.

If you later fix the TTM build to keep all 12309 blocks, `realign_user_ttm` detects the
`12309×12309` input and uses it as-is.

## Method (matches ARSW2015-toolkit MATLAB)

* **1a** `ln π_IJ = ϑ_I + ς_J − (ε·κ) τ_IJ` with origin/destination FE → ε·κ. The block
  TTM is aggregated to the 12 modern Bezirke (simple and employment-weighted means).
  Validated on ARSW's `bilat_*` data: OLS ε·κ = 0.0697 ≈ toolkit's 0.07.
* **1b** Solve transformed wages once at the gravity ε·κ; pick ε so the 1986 West Bezirk
  log-wage variance matches `wageworker1986.csv` (target 0.004753); κ = ε·κ / ε.
* **1c** `comegaoptC → camen → expincome → calcal_adj_TD → cdensity` recovers A, B, wages,
  income, density of development, commercial share.
* **1d** Re-run 1c with ARSW's block TTM; compare A and B on a common colour scale.

Parameters: α = 0.80, β = 0.75 (1−β = housing expenditure share).

## Data dependencies (all already in the repo)

`Data/FinalBerlin/matlab/data/`: `caldata2006_ren.csv`, `caldata1986_ren.csv`,
`bilat_{commute,minutes,probcommute}_bzk.csv`, and (for `--arsw-source csv`)
`ttfinal_2006_ren.csv`.
`ARSW2015/ARSW2015-toolkit/`: `matlab/data/input/wageworker1986.csv`,
`matlab/data/input/prepdata_big_TD.mat` (ARSW `tt06`), `shapefile/Berlin4matlab.shp`,
`shapefile/Bezirke23.shp` (optional overlay).
`Topic_7/TTM/`: `sample_travel_time_matrix.parquet` (and/or `simplified_…parquet`).

## Memory

The 2006 block TTM is 12309² ≈ 1.2 GB (float64); Task 1d loads a second one (freed first).
≥16 GB RAM recommended, ≥32 GB comfortable. Per-residence-block batching keeps the
`expincome`/`adjust_levels` intermediates bounded.
