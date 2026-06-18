"""
Topic 7 helpers — ARSW gravity regression utilities.

Used by task_1a.ipynb to replicate Table III and Figure 4A from
Ahlfeldt, Redding, Sturm & Wolf (2015, Econometrica).
"""

import warnings
import numpy as np
import pandas as pd
import geopandas as gpd
from pathlib import Path
import statsmodels.api as sm
from statsmodels.genmod.generalized_linear_model import GLM
from statsmodels.genmod import families


# ─────────────────────────────────────────────────────────────────────────────
# 1. DATA LOADING
# ─────────────────────────────────────────────────────────────────────────────

def load_tt_matrix(path: Path) -> pd.DataFrame:
    """Load block-level travel time matrix from parquet. Returns N×N DataFrame."""
    return pd.read_parquet(path)


def load_blocks_with_bezirk(shp_path: Path, crs_epsg: int = 25833) -> gpd.GeoDataFrame:
    """
    Load Berlin4matlab shapefile with the IDENTICAL geometry cleaning used in
    Topic_7/TTM/Final.py, so that centroid_id assignments align with the
    travel time matrix row/column indices.

    Cleaning steps (must match Final.py exactly):
      1. Drop null / empty geometries, reset index
      2. Drop corrupt coordinate geometries (NaN / Inf bounds), reset index
      3. make_valid(), reproject to crs_epsg
      4. Repeat null / corrupt drop, reset index
      5. Assign centroid_id = f"centroid_{i}"

    Returns GeoDataFrame with columns including BEZIRK, BEZIRK_NAM, centroid_id.
    """

    def _valid_coords(geom):
        try:
            b = geom.bounds
            return len(b) == 4 and not (np.any(np.isnan(b)) or np.any(np.isinf(b)))
        except Exception:
            return False

    gdf = gpd.read_file(shp_path)
    gdf = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty].reset_index(drop=True)
    gdf = gdf[gdf.geometry.apply(_valid_coords)].reset_index(drop=True)
    gdf["geometry"] = gdf.geometry.make_valid()
    gdf = gdf.to_crs(epsg=crs_epsg)
    gdf = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty].reset_index(drop=True)
    gdf = gdf[gdf.geometry.apply(_valid_coords)].reset_index(drop=True)
    gdf["centroid_id"] = [f"centroid_{i}" for i in range(len(gdf))]

    return gdf


def load_commuting_data(path: Path) -> pd.DataFrame:
    """
    Load Bezirke-level commuting flow data. Accepts CSV, DTA (Stata), XLSX.
    Raises FileNotFoundError with instructions if path does not exist.
    """
    if not path.exists():
        raise FileNotFoundError(
            f"Commuting data not found at: {path}\n"
            "Expected a file with one row per bilateral Bezirke pair containing:\n"
            "  - residence Bezirk identifier (int or str, 1–12)\n"
            "  - workplace Bezirk identifier (int or str, 1–12)\n"
            "  - commuter count (int)\n"
            "Obtain this file from the ARSW teaching/replication directory."
        )
    suffix = path.suffix.lower()
    if suffix == ".csv":
        return pd.read_csv(path)
    elif suffix == ".dta":
        return pd.read_stata(path)
    elif suffix in (".xlsx", ".xls"):
        return pd.read_excel(path)
    else:
        raise ValueError(f"Unsupported format '{suffix}'. Provide CSV, DTA, or XLSX.")


def normalize_commuting_columns(
    df: pd.DataFrame,
    res_col: str,
    work_col: str,
    flow_col: str,
) -> pd.DataFrame:
    """
    Rename and cast commuting flow columns to standard names.
    Ensures res, work are strings; n_comm is float.
    """
    df = df[[res_col, work_col, flow_col]].copy()
    df.columns = ["res", "work", "n_comm"]
    df["res"] = df["res"].astype(str)
    df["work"] = df["work"].astype(str)
    df["n_comm"] = pd.to_numeric(df["n_comm"], errors="coerce")
    return df.dropna(subset=["res", "work", "n_comm"]).reset_index(drop=True)


# ─────────────────────────────────────────────────────────────────────────────
# 2. BEZIRKE AGGREGATION
# ─────────────────────────────────────────────────────────────────────────────

def build_block_bezirk_map(
    blocks_gdf: gpd.GeoDataFrame,
    bezirk_col: str = "BEZIRK",
) -> pd.Series:
    """
    Extract centroid_id → Bezirk mapping from the loaded shapefile.
    Returns pd.Series indexed by centroid_id, values cast to str.
    """
    mapping = blocks_gdf.set_index("centroid_id")[bezirk_col].astype(str)
    return mapping


def aggregate_tt_to_bezirke(
    tt_matrix: pd.DataFrame,
    block_bezirk: pd.Series,
) -> pd.DataFrame:
    """
    Aggregate N×N block travel time matrix to K×K Bezirke-level matrix.

    τ̄_IJ = mean_{i∈I, j∈J} τ_{ij}    (ARSW eq. 25 approximation)

    Parameters
    ----------
    tt_matrix    : DataFrame (N×N) indexed and columned by centroid_id, values in minutes
    block_bezirk : Series mapping centroid_id → Bezirk ID (str)

    Returns
    -------
    DataFrame (K×K) with Bezirk IDs as index and columns.
    Diagonal (i=j, same district) included; caller drops if needed.
    """
    common = tt_matrix.index.intersection(block_bezirk.index)
    if len(common) == 0:
        raise ValueError(
            "No common indices between travel time matrix and block-Bezirk map. "
            "Ensure load_blocks_with_bezirk() uses the same shapefile as Final.py."
        )

    mat = tt_matrix.loc[common, common].copy().astype(float)
    bz = block_bezirk.loc[common]

    # Assign Bezirk labels to row and column indices
    mat.index = bz.values
    mat.columns = bz.values

    # Group rows, then columns
    # axis=0 groupby: mean residential blocks per Bezirk
    mat_row_grouped = mat.groupby(level=0).mean()
    # axis=1 groupby: mean workplace blocks per Bezirk
    bezirke_mat = mat_row_grouped.T.groupby(level=0).mean().T

    return bezirke_mat


# ─────────────────────────────────────────────────────────────────────────────
# 3. GRAVITY DATASET CONSTRUCTION
# ─────────────────────────────────────────────────────────────────────────────

def build_gravity_df(
    flows_df: pd.DataFrame,
    tt_bezirke: pd.DataFrame,
    min_n: int | None = None,
) -> pd.DataFrame:
    """
    Construct the bilateral gravity estimation dataset for ARSW eq. (25).

    Merges flows with Bezirke-level travel times, computes ln(π_IJ).

    Parameters
    ----------
    flows_df   : output of normalize_commuting_columns() — columns: res, work, n_comm
    tt_bezirke : output of aggregate_tt_to_bezirke() — K×K matrix, minutes
    min_n      : if set, restrict to bilateral pairs with n_comm >= min_n

    Returns
    -------
    DataFrame with columns: res, work, n_comm, pi, ln_pi, tau, res_fe, work_fe
    """
    df = flows_df.copy()

    # Bilateral commuting share π_IJ = n_IJ / Σ n_IJ
    total = df["n_comm"].sum()
    df["pi"] = df["n_comm"] / total

    # Pivot K×K matrix → long format for merge
    tt_long = (
        tt_bezirke
        .stack()
        .reset_index()
    )
    tt_long.columns = ["res", "work", "tau"]
    tt_long["res"] = tt_long["res"].astype(str)
    tt_long["work"] = tt_long["work"].astype(str)

    df = df.merge(tt_long, on=["res", "work"], how="left")
    df = df.dropna(subset=["tau"])

    if min_n is not None:
        df = df[df["n_comm"] >= min_n]

    df = df[df["pi"] > 0].copy()
    df["ln_pi"] = np.log(df["pi"])
    df["res_fe"] = df["res"].astype(str)
    df["work_fe"] = df["work"].astype(str)

    return df.reset_index(drop=True)


# ─────────────────────────────────────────────────────────────────────────────
# 4. REGRESSION ESTIMATORS
# ─────────────────────────────────────────────────────────────────────────────

def _build_fe_X(df: pd.DataFrame, treat_col: str = "tau") -> pd.DataFrame:
    """
    Build regressor matrix: treatment variable + two-way FE dummies.
    drop_first=True on both sets avoids perfect multicollinearity.
    Adds an intercept absorbed by the residence dummies.
    """
    res_d = pd.get_dummies(df["res_fe"], prefix="res", drop_first=True)
    work_d = pd.get_dummies(df["work_fe"], prefix="work", drop_first=True)
    X = pd.concat([df[[treat_col]], res_d, work_d], axis=1).astype(float)
    X = sm.add_constant(X)
    return X


def run_ols_gravity(df: pd.DataFrame) -> sm.regression.linear_model.RegressionResultsWrapper:
    """
    OLS: ln(π_IJ) = −ν·τ_IJ + ϑ_I + ς_J + e_IJ
    Two-way FE via explicit dummies. HC1 heteroscedasticity-robust SEs.
    The key coefficient is params['tau'] = −ν̂.
    """
    X = _build_fe_X(df, "tau")
    y = df["ln_pi"].astype(float)
    return sm.OLS(y, X).fit(cov_type="HC1")


def run_poisson_gravity(df: pd.DataFrame) -> GLM:
    """
    Poisson PML: E[π_IJ] = exp(−ν·τ_IJ + ϑ_I + ς_J)
    Dep. var.: commuting probability π_IJ (levels, not logs).
    Family: Poisson with canonical log link (PPML; Santos Silva & Tenreyro 2006).
    HC1 robust SEs. params['tau'] = −ν̂.
    """
    X = _build_fe_X(df, "tau")
    y = df["pi"].astype(float)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = GLM(y, X, family=families.Poisson()).fit(
            cov_type="HC1", maxiter=500, tol=1e-8
        )
    return result


def run_gamma_gravity(df: pd.DataFrame) -> GLM:
    """
    Gamma PML: E[π_IJ] = exp(−ν·τ_IJ + ϑ_I + ς_J)
    Dep. var.: commuting probability π_IJ (levels).
    Family: Gamma with log link. HC1 robust SEs. params['tau'] = −ν̂.
    """
    X = _build_fe_X(df, "tau")
    y = df["pi"].astype(float)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = GLM(
            y, X, family=families.Gamma(link=families.links.Log())
        ).fit(cov_type="HC1", maxiter=500, tol=1e-8)
    return result


# ─────────────────────────────────────────────────────────────────────────────
# 5. FWL RESIDUALIZATION  (for Figure 4A equivalent)
# ─────────────────────────────────────────────────────────────────────────────

def partial_out_fe(
    series: pd.Series,
    fe1: pd.Series,
    fe2: pd.Series,
    max_iter: int = 500,
    tol: float = 1e-12,
) -> pd.Series:
    """
    Frisch-Waugh-Lovell: residualize `series` on two-way FE dummies.

    Implements Gauss-Seidel alternating projections:
      repeat: resid -= group_mean(resid | fe1); resid -= group_mean(resid | fe2)
    Equivalent to OLS on explicit dummies for any panel size.

    Used to partial out residence and workplace district FEs from both
    ln(π_IJ) and τ_IJ before plotting the gravity fit (ARSW Figure 4A).
    """
    resid = series.copy().astype(float)
    for _ in range(max_iter):
        old = resid.copy()
        resid -= resid.groupby(fe1).transform("mean")
        resid -= resid.groupby(fe2).transform("mean")
        if np.abs(resid - old).max() < tol:
            break
    return resid


# ─────────────────────────────────────────────────────────────────────────────
# 6. LaTeX TABLE FORMATTER  (ARSW Table III style)
# ─────────────────────────────────────────────────────────────────────────────

def _significance_stars(pval: float) -> str:
    if pval < 0.01:
        return "***"
    elif pval < 0.05:
        return "**"
    elif pval < 0.10:
        return "*"
    return ""


def format_gravity_latex_table(
    results: list,
    ns: list,
    r2s: list,
    estimators: list,
    min_comm_flags: list,
    caption: str = "Commuting Gravity Equation",
    label: str = "tab:gravity_t7",
    notes: str = (
        r"Gravity equation estimates based on Bez\-irke-level commuting flows "
        r"and block-level travel times aggregated to the district level. "
        r"Observations are bilateral pairs of 12 post-2001 Berlin Bezirke. "
        r"Travel time $\bar{\tau}_{IJ}$ in minutes. "
        r"Fixed effects are residence and workplace district fixed effects. "
        r"Heteroscedasticity-robust SEs in parentheses. "
        r"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
    ),
) -> str:
    """
    Format a LaTeX table replicating ARSW (2015) Table III.

    Parameters
    ----------
    results        : list of 4 statsmodels result objects [OLS, OLS, Poisson, Gamma]
    ns             : list of 4 int — observation counts per specification
    r2s            : list of 4 float or None — R² (None for GLM specs)
    estimators     : list of 4 str — ['OLS', 'OLS', 'Poisson PML', 'Gamma PML']
    min_comm_flags : list of 4 bool — True if ≥10 commuters restriction applied

    Returns
    -------
    str — ready-to-paste LaTeX table environment
    """
    # Extract point estimates, SEs, p-values for τ coefficient
    nu_hats, ses, pvals = [], [], []
    for r in results:
        nu_hats.append(r.params["tau"])
        ses.append(r.bse["tau"])
        pvals.append(r.pvalues["tau"])

    def _coef_cell(v, pv):
        stars = _significance_stars(pv)
        if stars:
            return rf"${v:.4f}^{{{stars}}}$"
        return rf"${v:.4f}$"

    def _se_cell(v):
        return rf"$({v:.4f})$"

    lines = [
        r"\begin{table}[htbp]",
        r"\centering",
        rf"\caption{{{caption}}}",
        rf"\label{{{label}}}",
        r"\small",
        r"\begin{tabular}{lcccc}",
        r"\toprule",
        r" & (1) & (2) & (3) & (4) \\",
        r"\midrule",
        r" & \multicolumn{4}{c}{$\ln$ Bilateral Commuting Probability} \\",
        r"\midrule",
    ]

    # Coefficient row
    coef_vals = " & ".join(_coef_cell(nu_hats[i], pvals[i]) for i in range(4))
    lines.append(rf"Travel Time ($-\hat{{\nu}}$) & {coef_vals} \\")

    # SE row
    se_vals = " & ".join(_se_cell(ses[i]) for i in range(4))
    lines.append(rf" & {se_vals} \\")

    lines += [
        r"\midrule",
        "Estimator & " + " & ".join(estimators) + r" \\",
        r"$\geq 10$ Commuters & "
        + " & ".join("Yes" if f else "---" for f in min_comm_flags)
        + r" \\",
        r"Fixed Effects & Yes & Yes & Yes & Yes \\",
        "$N$ & " + " & ".join(str(n) for n in ns) + r" \\",
        "$R^2$ & "
        + " & ".join(f"{r:.3f}" if r is not None else "---" for r in r2s)
        + r" \\",
        r"\bottomrule",
        r"\end{tabular}",
        r"\medskip",
        r"\begin{minipage}{0.92\textwidth}",
        r"\footnotesize",
        rf"\textit{{Notes:}} {notes}",
        r"\end{minipage}",
        r"\end{table}",
    ]

    return "\n".join(lines)
