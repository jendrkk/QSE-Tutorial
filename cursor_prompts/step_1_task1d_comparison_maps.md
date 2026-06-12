# Step 1 — Task 1(d): Comparison Maps — Original ARSW TTM vs User TTM Fundamentals

## Project Context
QSE-Tutorial (Humboldt University Berlin, Summer 2026). Implements ARSW (2015) "Economics of Density" quantification: recovers adjusted productivities $\tilde{A}_j$ and amenities $\tilde{B}_i$ from a travel time matrix over 12 309 Berlin blocks. Language: Python 3; key libraries: `geopandas`, `matplotlib`, `numpy`, `scipy.io`, `seaborn`, `pandas`. Project-specific module `arsw_python.recover_fundamentals.run_calcal_TD` runs the full sequential calibration pipeline. Task 1(d) requires a 3-panel × 2-map figure comparing fundamentals produced by the original ARSW `tt06` (embedded in `prepdata_big_TD.mat`) against those produced by the user-computed multimodal Berlin TTM.

---

## Current State

`task_1c.ipynb` (already fixed) uses:
- `gdf_berlin.plot(ax=ax, color="#d0d0d0", edgecolor="none", zorder=1)` as background layer
- Streets overlay: `streets_gdf.plot(ax=ax, color="white", linewidth=0.15, alpha=0.4, zorder=3)`
- `STREETS_SHP = REPO_ROOT / "Data" / "Shapefiles-2022" / "Berlin" / "TransportNetworkParts2006" / "Streets.shp"`

`run_calcal_TD` with `user_ttm_path=None` falls back to the embedded ARSW `tt06` — this is the original matrix to compare against. Results from task_1c are in `calcal_1c_results.npz`.

**Relevant files:**
- `/Users/jedrek/Documents/Studium Volkswirschaftslehre/4. Semester/Quantitive Spatial Economics/QSE-Tutorial/Topic_7/task_1c.ipynb` — reference for plot pattern (background layer + streets)
- `/Users/jedrek/Documents/Studium Volkswirschaftslehre/4. Semester/Quantitive Spatial Economics/QSE-Tutorial/Topic_7/arsw_python/recover_fundamentals.py` — `run_calcal_TD`, `save_results`
- `/Users/jedrek/Documents/Studium Volkswirschaftslehre/4. Semester/Quantitive Spatial Economics/QSE-Tutorial/ARSW2015/ARSW2015-toolkit/matlab/data/output/calcal_1c_results.npz` — saved user TTM results (12 309 blocks)

**CRITICAL plot pattern (from task_1c — must be replicated in every subplot):**
```python
# 1. Background: render all blocks as gray so NaN blocks remain visible
gdf_berlin.plot(ax=ax, color="#d0d0d0", edgecolor="none", zorder=1)
# 2. Data layer on top
gdf_plot.plot(column=col, ax=ax, cmap=..., missing_kwds={"color": "#d0d0d0", ...}, zorder=2)
# 3. Streets overlay
if streets_gdf is not None:
    streets_gdf.plot(ax=ax, color="white", linewidth=0.15, alpha=0.4, zorder=3)
```
Without step 1 (background), blocks with NaN values are either invisible (matching white background) or silently dropped by geopandas. This is the root cause of the "missing blocks" issue in earlier map versions.

---

## Objective

Create `task_1d.ipynb` in the `Topic_7/` directory. The notebook:

1. Loads user TTM fundamentals from `calcal_1c_results.npz`.
2. Runs (or loads from cache) the calibration pipeline with original ARSW `tt06`; caches to `calcal_1d_orig_results.npz`.
3. Loads streets shapefile for overlay.
4. Builds a cleaned Berlin GeoDataFrame with all six map columns assigned positionally.
5. Produces one `(3 × 2)` matplotlib figure using the **background + streets** pattern from task_1c:
   - **Row 0** (Panel 1): $\log\tilde{A}_j$ — shared `vmin/vmax` across left and right
   - **Row 1** (Panel 2): $\log\tilde{B}_i$ — shared `vmin/vmax` across left and right
   - **Row 2** (Panel 3): Differences $\Delta\log\tilde{A}_j$ and $\Delta\log\tilde{B}_i$ (User − Original) on `RdBu_r` diverging colormap, symmetric about zero
6. Saves to `plots/task_1d_fundamentals_comparison.png` at 300 dpi.

---

## Tasks

### Task 1.1 — Create `task_1d.ipynb`
**File:** `/Users/jedrek/Documents/Studium Volkswirschaftslehre/4. Semester/Quantitive Spatial Economics/QSE-Tutorial/Topic_7/task_1d.ipynb`
**Action:** `Create`

---

#### Cell 0 — Style setup
```python
import matplotlib.pyplot as plt
import seaborn as sns

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

from pathlib import Path

PLOT_ROOT = Path.cwd() / "plots"
PLOT_ROOT.mkdir(exist_ok=True)
```

---

#### Cell 1 — Paths and parameters
```python
import sys
import numpy as np
import pandas as pd
import geopandas as gpd
import scipy.io as sio

sys.path.insert(0, str(Path.cwd()))
from arsw_python.recover_fundamentals import run_calcal_TD, save_results

# ── Repository tree ───────────────────────────────────────────────────────────
REPO_ROOT    = Path.cwd().parent
ARSW_TOOLKIT = REPO_ROOT / "ARSW2015" / "ARSW2015-toolkit"

# ── Data inputs ───────────────────────────────────────────────────────────────
MAT_PATH_TD    = ARSW_TOOLKIT / "matlab" / "data" / "input" / "prepdata_big_TD.mat"
TTM_CLEAN_PATH = Path.cwd() / "TTM" / "tt06_user_preprocessed.parquet"

# ── Saved results ─────────────────────────────────────────────────────────────
USER_NPZ = ARSW_TOOLKIT / "matlab" / "data" / "output" / "calcal_1c_results.npz"
ORIG_NPZ = ARSW_TOOLKIT / "matlab" / "data" / "output" / "calcal_1d_orig_results.npz"

# ── Shapefiles ────────────────────────────────────────────────────────────────
BLOCKS_SHP_FULL = ARSW_TOOLKIT / "shapefile" / "Berlin4matlab.shp"
STREETS_SHP     = REPO_ROOT / "Data" / "Shapefiles-2022" / "Berlin" / "TransportNetworkParts2006" / "Streets.shp"

# ── Parameters — must match task_1c exactly ───────────────────────────────────
EPSILON_HAT = 6.83
KAPPAEPS    = 0.07
ALPHA       = 0.80
BETA        = 0.75

print(f"Parameters: ε = {EPSILON_HAT},  κε = {KAPPAEPS},  κ = {KAPPAEPS/EPSILON_HAT:.6f}")
print(f"Streets.shp present: {STREETS_SHP.exists()}")
```

---

#### Cell 2 — [MARKDOWN]
```markdown
## Task 1(d) — Comparing Recovered Fundamentals: Original ARSW TTM vs User TTM

**Objective.** The structural fundamentals $\tilde{A}_j$ (adjusted productivity) and
$\tilde{B}_i$ (adjusted amenity) are inverted from the data conditional on the travel
time matrix. Differences in $\tau_{ij}$ between the original ARSW matrix and the
user-computed multimodal TTM propagate into the recovered fundamentals through two
channels:

- **Productivity** $\tilde{A}_j$: identified from the wage fixed-point system
  (ARSW eq. S.44). Any systematic difference in commuting access to workplace $j$
  shifts the implied productivity residual.
- **Amenity** $\tilde{B}_i$: inverted from ARSW eq. S.47 via commuting market access
  $\mathrm{CMA}_i = \sum_j e^{-\varepsilon\kappa\tau_{ij}} w_j^\varepsilon$.
  A higher CMA (shorter travel times to better-paying workplaces) reduces the amenity
  residual needed to rationalize observed residential sorting.

The comparison maps show where the user TTM leads to systematically higher or lower
fundamentals, revealing how TTM construction choices affect structural inference.
```

---

#### Cell 3 — Load user TTM results
```python
# ── Validate and load user results (from task_1c) ────────────────────────────
if not USER_NPZ.exists():
    raise FileNotFoundError(
        f"User TTM results not found: {USER_NPZ}\n"
        "Run task_1c.ipynb first to generate and save the calibration output."
    )

d_user = np.load(str(USER_NPZ))
NOBS06 = int(d_user["nobs06"])

A_user = d_user["A06"].copy()
B_user = d_user["B06"].copy()

with np.errstate(divide='ignore', invalid='ignore'):
    logA_user = np.where(A_user > 0, np.log(A_user), np.nan)
    logB_user = np.where(B_user > 0, np.log(B_user), np.nan)

print(f"User TTM results loaded: nobs06 = {NOBS06}")
print(f"  A06: {(A_user > 0).sum()} positive blocks,  "
      f"geomean = {np.exp(np.nanmean(logA_user)):.4f}")
print(f"  B06: {(B_user > 0).sum()} positive blocks")
```

---

#### Cell 4 — Run or load original ARSW calibration
```python
# ── Cache-aware: run original calibration only if not already saved ───────────
if ORIG_NPZ.exists():
    print(f"Loading cached original ARSW results: {ORIG_NPZ.name}")
    d_orig = np.load(str(ORIG_NPZ))
else:
    if not MAT_PATH_TD.exists():
        raise FileNotFoundError(
            f"prepdata_big_TD.mat not found: {MAT_PATH_TD}\n"
            "Download from: https://box.hu-berlin.de/f/54d2f718ec8644e5888f/?dl=1\n"
            f"Save to: {MAT_PATH_TD.parent}"
        )
    print("Running calibration with original ARSW tt06 (approx 5-10 min) ...")
    results_orig = run_calcal_TD(
        mat_path_TD=MAT_PATH_TD,
        user_ttm_path=None,          # <- None uses embedded tt06 from prepdata_big_TD.mat
        epsilon=EPSILON_HAT,
        kappaeps=KAPPAEPS,
        alpha=ALPHA,
        beta=BETA,
        verbose=True,
    )
    save_results(results_orig, ORIG_NPZ)
    print(f"Cached to: {ORIG_NPZ}")
    d_orig = np.load(str(ORIG_NPZ))

A_orig = d_orig["A06"].copy()
B_orig = d_orig["B06"].copy()

with np.errstate(divide='ignore', invalid='ignore'):
    logA_orig = np.where(A_orig > 0, np.log(A_orig), np.nan)
    logB_orig = np.where(B_orig > 0, np.log(B_orig), np.nan)

n_orig = int(d_orig["nobs06"])
print(f"\nOriginal ARSW results: nobs06 = {n_orig}")
print(f"  A06: {(A_orig > 0).sum()} positive blocks,  "
      f"geomean = {np.exp(np.nanmean(logA_orig)):.4f}")
print(f"  B06: {(B_orig > 0).sum()} positive blocks")

if n_orig != NOBS06:
    raise ValueError(
        f"nobs06 mismatch: user={NOBS06}, original={n_orig}. "
        "Both calibrations must operate on the same prepdata_big_TD.mat."
    )
```

---

#### Cell 5 — Build GeoDataFrame, load streets, compute differences
```python
def _clean_shapefile(gdf: gpd.GeoDataFrame, target_crs: int = 25833) -> gpd.GeoDataFrame:
    """
    Six-step geometry cleaning — byte-for-byte identical to TTM/Final.py and task_1c.ipynb.
    Must not deviate: the result arrays are assigned positionally by block index.
    """
    def _valid_coords(geom):
        try:
            b = geom.bounds
            return len(b) == 4 and not (np.any(np.isnan(b)) or np.any(np.isinf(b)))
        except Exception:
            return False
    gdf = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty].reset_index(drop=True)
    gdf = gdf[gdf.geometry.apply(_valid_coords)].reset_index(drop=True)
    gdf["geometry"] = gdf.geometry.make_valid()
    gdf = gdf.to_crs(epsg=target_crs)
    gdf = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty].reset_index(drop=True)
    gdf = gdf[gdf.geometry.apply(_valid_coords)].reset_index(drop=True)
    return gdf


print("Loading Berlin4matlab.shp ...")
gdf_berlin = _clean_shapefile(gpd.read_file(str(BLOCKS_SHP_FULL)))
n_shp = len(gdf_berlin)

if n_shp != NOBS06:
    raise ValueError(
        f"Shapefile has {n_shp} blocks but nobs06 = {NOBS06}. "
        "Geometry cleaning mismatch."
    )
print(f"  Blocks: {n_shp}  (matches nobs06)")

# ── Load streets for overlay ───────────────────────────────────────────────────
streets_gdf = None
if STREETS_SHP.exists():
    print("Loading Streets.shp for overlay ...")
    streets_gdf = gpd.read_file(str(STREETS_SHP)).to_crs(epsg=25833)
    print(f"  Street segments: {len(streets_gdf)}")
else:
    print(f"Streets.shp not found — overlay will be skipped")

# ── Assign result vectors by positional index ─────────────────────────────────
gdf_berlin["logA_orig"] = logA_orig
gdf_berlin["logA_user"] = logA_user
gdf_berlin["logB_orig"] = logB_orig
gdf_berlin["logB_user"] = logB_user

# ── Differences: user minus original (NaN where either is non-positive) ───────
both_A = (A_orig > 0) & (A_user > 0)
both_B = (B_orig > 0) & (B_user > 0)

dlogA = np.where(both_A, logA_user - logA_orig, np.nan)
dlogB = np.where(both_B, logB_user - logB_orig, np.nan)

gdf_berlin["dlogA"] = dlogA
gdf_berlin["dlogB"] = dlogB

# ── Diagnostics ───────────────────────────────────────────────────────────────
for name, arr, n_valid in [
    ("Delta log A", dlogA, both_A.sum()),
    ("Delta log B", dlogB, both_B.sum()),
]:
    finite = arr[~np.isnan(arr)]
    print(f"\n{name}: {n_valid} blocks with valid diff")
    print(f"  mean = {finite.mean():.4f},  std = {finite.std():.4f}")
    print(f"  range: [{finite.min():.3f},  {finite.max():.3f}]")
```

---

#### Cell 6 — Three-panel comparison figure (3 rows x 2 cols)

**CRITICAL**: Every subplot must follow the exact 3-step rendering pattern from task_1c Cell 7:
1. `gdf_berlin.plot(ax=ax, color="#d0d0d0", edgecolor="none", zorder=1)` — background layer (makes all 12309 blocks visible even when data is NaN)
2. `gdf_berlin.plot(column=col, ax=ax, ..., missing_kwds={"color": "#d0d0d0", ...}, zorder=2)` — data layer
3. `if streets_gdf is not None: streets_gdf.plot(ax=ax, color="white", linewidth=0.15, alpha=0.4, zorder=3)` — streets overlay

```python
# ── Shared color scales ───────────────────────────────────────────────────────
# Panels 0-1: shared vmin/vmax computed across BOTH original and user arrays.
vmin_A = float(np.nanmin([logA_orig, logA_user]))
vmax_A = float(np.nanmax([logA_orig, logA_user]))

vmin_B = float(np.nanmin([logB_orig, logB_user]))
vmax_B = float(np.nanmax([logB_orig, logB_user]))

# Panel 2: symmetric diverging scale about 0
_dA_fin = dlogA[~np.isnan(dlogA)]
_dB_fin = dlogB[~np.isnan(dlogB)]
vabs_A = float(np.abs(_dA_fin).max()) if len(_dA_fin) > 0 else 0.1
vabs_B = float(np.abs(_dB_fin).max()) if len(_dB_fin) > 0 else 0.1

# ── Figure ────────────────────────────────────────────────────────────────────
MISSING = {"color": "#d0d0d0", "label": "No data"}
fig, axes = plt.subplots(3, 2, figsize=(18, 24))

# ── Row 0: log A ──────────────────────────────────────────────────────────────
for c, (col, title) in enumerate([
    ("logA_orig", r"Original ARSW TTM --- $\log\tilde{A}_j$"),
    ("logA_user", r"User TTM --- $\log\tilde{A}_j$"),
]):
    ax = axes[0, c]
    gdf_berlin.plot(ax=ax, color="#d0d0d0", edgecolor="none", zorder=1)
    gdf_berlin.plot(
        column=col, ax=ax, cmap="YlOrRd", vmin=vmin_A, vmax=vmax_A,
        legend=True, missing_kwds=MISSING,
        legend_kwds={"label": r"$\log\tilde{A}$", "shrink": 0.70,
                     "orientation": "vertical", "pad": 0.02},
        zorder=2,
    )
    if streets_gdf is not None:
        streets_gdf.plot(ax=ax, color="white", linewidth=0.15, alpha=0.4, zorder=3)
    ax.set_title(title, pad=8, fontsize=12)
    ax.set_axis_off()

# ── Row 1: log B ──────────────────────────────────────────────────────────────
for c, (col, title) in enumerate([
    ("logB_orig", r"Original ARSW TTM --- $\log\tilde{B}_i$"),
    ("logB_user", r"User TTM --- $\log\tilde{B}_i$"),
]):
    ax = axes[1, c]
    gdf_berlin.plot(ax=ax, color="#d0d0d0", edgecolor="none", zorder=1)
    gdf_berlin.plot(
        column=col, ax=ax, cmap="YlOrRd", vmin=vmin_B, vmax=vmax_B,
        legend=True, missing_kwds=MISSING,
        legend_kwds={"label": r"$\log\tilde{B}$", "shrink": 0.70,
                     "orientation": "vertical", "pad": 0.02},
        zorder=2,
    )
    if streets_gdf is not None:
        streets_gdf.plot(ax=ax, color="white", linewidth=0.15, alpha=0.4, zorder=3)
    ax.set_title(title, pad=8, fontsize=12)
    ax.set_axis_off()

# ── Row 2: differences ────────────────────────────────────────────────────────
for c, (col, vabs, title, cbar_label) in enumerate([
    ("dlogA", vabs_A,
     r"$\Delta\log\tilde{A}_j = \log\tilde{A}^{\rm user}_j - \log\tilde{A}^{\rm ARSW}_j$",
     r"$\Delta\log\tilde{A}$"),
    ("dlogB", vabs_B,
     r"$\Delta\log\tilde{B}_i = \log\tilde{B}^{\rm user}_i - \log\tilde{B}^{\rm ARSW}_i$",
     r"$\Delta\log\tilde{B}$"),
]):
    ax = axes[2, c]
    gdf_berlin.plot(ax=ax, color="#d0d0d0", edgecolor="none", zorder=1)
    gdf_berlin.plot(
        column=col, ax=ax, cmap="RdBu_r", vmin=-vabs, vmax=vabs,
        legend=True, missing_kwds=MISSING,
        legend_kwds={"label": cbar_label, "shrink": 0.70,
                     "orientation": "vertical", "pad": 0.02},
        zorder=2,
    )
    if streets_gdf is not None:
        streets_gdf.plot(ax=ax, color="white", linewidth=0.15, alpha=0.4, zorder=3)
    ax.set_title(title, pad=8, fontsize=11)
    ax.set_axis_off()

# ── Row labels and suptitle ───────────────────────────────────────────────────
row_labels = [
    r"\textbf{Panel 1:} Adjusted Productivity $\log\tilde{A}_j$",
    r"\textbf{Panel 2:} Adjusted Amenity $\log\tilde{B}_i$",
    r"\textbf{Panel 3:} Difference (User $-$ ARSW)",
]
for r, label in enumerate(row_labels):
    axes[r, 0].set_ylabel(label, fontsize=11, labelpad=8)

plt.suptitle(
    r"Task 1(d): Fundamentals comparison --- Original ARSW TTM vs User-Computed TTM"
    "\n"
    rf"($\hat{{\varepsilon}} = {EPSILON_HAT}$, "
    rf"$\hat{{\kappa}} = {KAPPAEPS/EPSILON_HAT:.4f}$, "
    rf"$\alpha = {ALPHA}$, $\beta = {BETA}$)",
    fontsize=13, y=1.005,
)

plt.tight_layout(rect=[0, 0, 1, 1])

save_path = PLOT_ROOT / "task_1d_fundamentals_comparison.png"
fig.savefig(save_path, dpi=300, bbox_inches="tight")
plt.show()
print(f"Saved: {save_path}")
```

---

#### Cell 7 — Summary
```python
print("=" * 72)
print("  Task 1(d) -- Summary: Fundamentals Comparison")
print("=" * 72)
print()
print(f"  Parameters:  epsilon={EPSILON_HAT},  kappa={KAPPAEPS/EPSILON_HAT:.6f},  "
      f"alpha={ALPHA},  beta={BETA}")
print(f"  nobs06 = {NOBS06}  (East + West Berlin, 2006)")
print()
print("  Productivity A (positive blocks):")
print(f"    Original TTM : {(A_orig > 0).sum():5d}  geomean = "
      f"{np.exp(np.nanmean(logA_orig)):.4f}")
print(f"    User TTM     : {(A_user > 0).sum():5d}  geomean = "
      f"{np.exp(np.nanmean(logA_user)):.4f}")
_dA = dlogA[~np.isnan(dlogA)]
print(f"    Delta log A  : mean={_dA.mean():.4f}, std={_dA.std():.4f}, "
      f"p5={np.percentile(_dA, 5):.3f}, p95={np.percentile(_dA, 95):.3f}")
print()
print("  Amenity B (positive blocks):")
print(f"    Original TTM : {(B_orig > 0).sum():5d}")
print(f"    User TTM     : {(B_user > 0).sum():5d}")
_dB = dlogB[~np.isnan(dlogB)]
print(f"    Delta log B  : mean={_dB.mean():.4f}, std={_dB.std():.4f}, "
      f"p5={np.percentile(_dB, 5):.3f}, p95={np.percentile(_dB, 95):.3f}")
print()
print(f"  Original results cached : {ORIG_NPZ.name}")
print(f"  Figure saved            : plots/task_1d_fundamentals_comparison.png")
print()
print("  Interpretation:")
print("    Delta log A > 0 at j  =>  User TTM implies higher adjusted productivity")
print("      at block j. Consistent with user TTM overestimating commuting times TO j.")
print()
print("    Delta log B > 0 at i  =>  User TTM implies higher adjusted amenity at i.")
print("      Follows from CMA_i being lower in user TTM (longer average travel times")
print("      to workplaces): same residential employment with lower CMA requires")
print("      a larger amenity residual to rationalise observed sorting.")
print("=" * 72)
```

---

## Constraints
- **Do not modify:** `task_1c.ipynb`, any `arsw_python/` module
- **Every subplot in Cell 6 must use the 3-step pattern**: background → data → streets. This is non-negotiable — it is what makes all 12309 blocks visible regardless of geopandas NaN-handling behavior
- **`_clean_shapefile` must be byte-for-byte identical** to task_1c Cell 7
- **Dependency order:** Cell 1 → Cell 3 → Cell 4 → Cell 5 → Cell 6 → Cell 7
- **Cache gate in Cell 4:** `if ORIG_NPZ.exists(): load else: run + save`

## Acceptance Criteria
- [ ] `task_1d.ipynb` exists at `.../Topic_7/task_1d.ipynb`
- [ ] All cells run top-to-bottom without error
- [ ] Cell 4 log contains `">>>> Using embedded tt06 from prepdata_big_TD.mat <<<<<"` confirming original matrix path
- [ ] `calcal_1d_orig_results.npz` is written on first run; subsequent runs load it
- [ ] **Every one of the 6 subplots in Cell 6 calls `gdf_berlin.plot(ax=ax, color="#d0d0d0", ...)` FIRST before the data plot** — this is the critical fix preventing sparse maps
- [ ] Cell 6 panel 0–1 use `cmap="YlOrRd"` with shared bounds; panel 2 uses `cmap="RdBu_r"` symmetric about 0
- [ ] `plots/task_1d_fundamentals_comparison.png` saved at 300 dpi
- [ ] Cell 7 prints Delta statistics without error

---

## Execution Order

**Run:** Step 1 (standalone)

| Step | Title | Depends on | Rationale |
|------|-------|-----------|-----------|
| 1 | Task 1(d) Comparison Maps | task_1c complete | Reads `calcal_1c_results.npz`; runs original calibration; produces 3×2 comparison figure |

**Dependency graph:**
```
[task_1c — already complete] ──► [Step 1: task_1d.ipynb]
```
