"""
recover_fundamentals.py
Sequential quantification procedure for ARSW2015 Section 6.

Translated from calcal_TD.m.

Pipeline:
  1. Load data (prepdata_big_TD + user TTM)
  2. Bezirk crosswalk (modbezirk)
  3. solve_omega_C → adjusted wages w, productivities A
  4. recover_amenities → adjusted amenities B, CMA, HRS
  5. adjust_levels → rescale A and B to match city population
  6. compute_expected_income → total worker income vv
  7. compute_density → φ (density), L (floor space), θ (commercial share)
"""
from __future__ import annotations
from pathlib import Path
import numpy as np

from .data_loaders import (
    load_prepdata_TD,
    load_user_ttm_mat,
    load_user_ttm_parquet,
)
from .solvers import solve_omega_C
from .calibration import (
    mod_bezirk,
    recover_amenities,
    compute_expected_income,
    compute_density,
    adjust_levels,
)


def run_calcal_TD(
    mat_path_TD: Path,
    user_ttm_path: Path | None = None,
    user_ttm_var: str = "tt06",
    epsilon: float = 6.83,
    kappaeps: float = 0.07,
    alpha: float = 0.80,
    beta: float = 0.75,
    verbose: bool = True,
) -> dict:
    """
    Full sequential calibration pipeline. Recovers Ã and B̃ for 2006 Berlin.
    Equivalent to calcal_TD.m.

    Parameters
    ----------
    mat_path_TD   : path to prepdata_big_TD.mat  (download from ARSW README)
    user_ttm_path : path to user travel time matrix (.mat or .parquet)
                    If None, uses the tt06 embedded in prepdata_big_TD.mat.
                    Provide your own matrix from Topic_7/TTM/ for task 1d
                    comparison. Matrix must be (nobs06 × nobs06) aligned to
                    the Berlin4matlab block ordering.
    user_ttm_var  : variable name inside .mat file (default 'tt06')
    epsilon       : Fréchet ε — use output of run_optimepsilon_TD86  (paper: 6.83)
    kappaeps      : κε commuting decay (paper: 0.07)
    alpha, beta   : factor shares (paper: 0.80, 0.75)
    verbose       : print pipeline progress

    Returns
    -------
    dict with keys (all (nobs06,) ndarrays unless noted):
        wage06     adjusted wages w
        A06        adjusted productivities Ã (geomean=1 among positive)
        B06        adjusted amenities B̃ (population-matched)
        CMA06      commuting market access
        HRS06      residential employment shares
        vv06       total worker income at residence
        V06        density of development φ
        L06        total floor space
        theta06    commercial floor space share
        modbzk06   modern Bezirk codes (1-12)
        nobs06     int — total blocks
        fwestr     (nobs06,) bool — West Berlin indicator (or None)
        obsvar06   (nobs06, 4) — [floor, empwpl, emprsd, area]
        kappa      float
        epsilon    float
    """
    kappa = kappaeps / epsilon

    # ── 1. Load data ──────────────────────────────────────────────────────────
    if verbose:
        print(">>>> Loading prepdata_big_TD <<<<")
    data06 = load_prepdata_TD(mat_path_TD)

    floor06  = data06["floor06"]
    empwpl06 = data06["empwpl06"]
    emprsd06 = data06["emprsd06"]
    area06   = data06["area06"]
    nobs06   = data06["nobs06"]
    bzk06    = data06["bzk06"]
    fwestr   = data06.get("fwestr")

    # ── 2. Inject travel time matrix ──────────────────────────────────────────
    if user_ttm_path is not None:
        sfx = Path(user_ttm_path).suffix.lower()
        if sfx == ".parquet":
            tt06 = load_user_ttm_parquet(user_ttm_path)
        else:
            tt06 = load_user_ttm_mat(user_ttm_path, var_name=user_ttm_var)
        if tt06.shape != (nobs06, nobs06):
            raise ValueError(
                f"User TTM shape {tt06.shape} != ({nobs06}, {nobs06}). "
                "The matrix must be aligned to Berlin4matlab block ordering. "
                "Check that Final.py used the same shapefile as prepdata_TD.m."
            )
        if verbose:
            print(f">>>> Using user TTM from: {user_ttm_path} <<<<")
    else:
        tt06 = data06["tt06"]
        if verbose:
            print(">>>> Using embedded tt06 from prepdata_big_TD.mat <<<<")

    # ── 3. Build obsvar06 ─────────────────────────────────────────────────────
    obsvar06 = np.zeros((nobs06, 4))
    obsvar06[:, 0] = floor06
    obsvar06[:, 1] = empwpl06
    obsvar06[:, 2] = emprsd06
    obsvar06[:, 3] = area06

    # ── 4. Modern Bezirk crosswalk ────────────────────────────────────────────
    modbzk06 = mod_bezirk(bzk06, nobs06)

    # ── 5. Initial adjusted wages (Eq. 12 with A=1) ───────────────────────────
    wage_06 = np.zeros(nobs06)
    pos = empwpl06 > 0
    wage_06[pos] = (
        ((1 - alpha) / floor06[pos]) ** ((1 - alpha) / alpha)
        * alpha
    )

    # ── 6. Solve for adjusted wages and productivities ────────────────────────
    if verbose:
        print(">>>> Step 1: solve_omega_C — wages + productivities <<<<")
    wage06, A06, cprob06, wconverge06, HMC06, wgap06 = solve_omega_C(
        obsvar06, tt06, nobs06, wage_06,
        alpha=alpha, beta=beta, epsilon=epsilon, kappa=kappa,
        verbose=verbose,
    )
    if verbose:
        print(f"     converge={wconverge06}, gap={wgap06}")

    # ── 7. Recover amenities ──────────────────────────────────────────────────
    if verbose:
        print(">>>> Step 2: recover_amenities — B, CMA, HRS <<<<")
    B06, CMA06, HRS06 = recover_amenities(
        obsvar06, tt06, nobs06, wage06,
        alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon,
    )

    # ── 8. Adjust levels ──────────────────────────────────────────────────────
    if verbose:
        print(">>>> Step 3: adjust_levels — rescale A and B <<<<")
    A06, B06, wage06 = adjust_levels(
        obsvar06, tt06, nobs06, A06, B06,
        alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon,
    )

    # ── 9. Total worker income ────────────────────────────────────────────────
    if verbose:
        print(">>>> Step 4: compute_expected_income — vv <<<<")
    vv06 = compute_expected_income(
        obsvar06, tt06, nobs06, wage06, B06,
        alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon,
    )

    # ── 10. Density of development ────────────────────────────────────────────
    if verbose:
        print(">>>> Step 5: compute_density — V, L, theta <<<<")
    V06, L06, theta06 = compute_density(
        obsvar06, A06, wage06, vv06, nobs06,
        alpha=alpha, beta=beta,
    )

    if verbose:
        print(">>>> Sequential quantification successfully completed <<<<")

    return {
        "wage06":   wage06,
        "A06":      A06,
        "B06":      B06,
        "CMA06":    CMA06,
        "HRS06":    HRS06,
        "vv06":     vv06,
        "V06":      V06,
        "L06":      L06,
        "theta06":  theta06,
        "modbzk06": modbzk06,
        "nobs06":   nobs06,
        "fwestr":   fwestr,
        "obsvar06": obsvar06,
        "kappa":    kappa,
        "epsilon":  epsilon,
    }


def save_results(results: dict, output_path: Path) -> None:
    """
    Save all ndarray outputs from run_calcal_TD to a .npz file.

    Parameters
    ----------
    results     : dict returned by run_calcal_TD
    output_path : path to .npz output file (e.g. 'data/output/calcal_big.npz')

    Usage
    -----
    results = run_calcal_TD(...)
    save_results(results, Path("data/output/calcal_big.npz"))
    # reload:  d = np.load("calcal_big.npz"); A06 = d["A06"]
    """
    arrays = {k: v for k, v in results.items() if isinstance(v, np.ndarray)}
    scalars = {k: np.array(v) for k, v in results.items()
               if isinstance(v, (int, float)) and not isinstance(v, bool)}
    if results.get("fwestr") is None:
        arrays.pop("fwestr", None)
    np.savez(str(output_path), **arrays, **scalars)
    print(f">>>> Results saved to {output_path} <<<<")
