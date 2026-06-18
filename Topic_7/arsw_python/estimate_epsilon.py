"""
estimate_epsilon.py
One-step GMM estimation of Fréchet shape parameter ε.

Translated from:
  optimepsilon_TD86.m  — main driver
  cdensityoptren.m     — objective function

Moment condition (Eq. S.64):
  min_ε  ( Var[ln w_J^{model}] − Var[ln w_J^{data}] )²
where w_J^{model} = Bezirk-level employment-weighted wages derived from
pre-solved transformed wages ω = w^ε, and J indexes the K Bezirke.

MATLAB patternsearch → scipy.optimize.minimize_scalar(method='bounded').
"""
from __future__ import annotations
from pathlib import Path
import numpy as np
import pandas as pd
from scipy.optimize import minimize_scalar
from scipy.stats import gmean

from .data_loaders import (
    load_prepdata_TD86,
    load_bezirke_wages_1986,
    load_user_ttm_mat,
    load_user_ttm_parquet,
)
from .solvers import solve_omega_O


# ─────────────────────────────────────────────────────────────────────────────
# Objective function  ≡  cdensityoptren.m
# ─────────────────────────────────────────────────────────────────────────────

def _objective_epsilon(
    epsilon: float,
    omega86rw: np.ndarray,
    empwpl86rw: np.ndarray,
    bzk86rw: np.ndarray,
    varlwdata: float,
) -> float:
    """
    Scalar objective for ε GMM.

    Residual sum of squares between model and data Bezirk log-wage variance,
    scaled by 1e6 to match MATLAB numerics.

    Parameters
    ----------
    epsilon    : candidate ε value
    omega86rw  : (noj,) pre-solved transformed wages (FIXED across ε calls)
    empwpl86rw : (noj,) workplace employment
    bzk86rw    : (noj,) int Bezirk indicators
    varlwdata  : float — target: Var(ln Bezirk wages, data)

    Returns
    -------
    f : scaled squared gap  (varlwmod - varlwdata)² × 1e6
    """
    # Recover adjusted wages: w = ω^{1/ε}  (reverse transformation)
    wage86rw = omega86rw ** (1.0 / epsilon)
    pos = wage86rw > 0
    if pos.any():
        wage86rw[pos] = wage86rw[pos] / gmean(wage86rw[pos])

    # Bezirke employment-weighted wages (≡ grpstats in MATLAB)
    wbill = wage86rw * empwpl86rw
    df = pd.DataFrame({"wbill": wbill, "emp": empwpl86rw, "bzk": bzk86rw})
    g = df.groupby("bzk")
    bzkwage = g["wbill"].sum() / g["emp"].sum()

    lbwmod = np.log(bzkwage.values)
    lbwmod -= lbwmod.mean()
    varlwmod = float(np.var(lbwmod, ddof=1))   # MATLAB var() uses ddof=1

    return ((varlwmod - varlwdata) ** 2) * 1e6


# ─────────────────────────────────────────────────────────────────────────────
# Main driver  ≡  optimepsilon_TD86.m
# ─────────────────────────────────────────────────────────────────────────────

def run_optimepsilon_TD86(
    mat_path_TD86: Path,
    csv_path_wages: Path,
    tt86rw_override: np.ndarray | None = None,
    alpha: float = 0.80,
    beta: float = 0.75,
    kappaeps: float = 0.07,
    epsilon_lb: float = 2.0,
    epsilon_ub: float = 24.0,
    verbose: bool = True,
) -> dict:
    """
    Estimate ε via one-step GMM.

    Procedure:
      1. Load prepdata_big_TD86.mat and wageworker1986.csv
      2. Solve for ω once using comegaoptO with kappaeps
      3. Find ε* = argmin f(ε) over [epsilon_lb, epsilon_ub]
      4. Derive κ = κε / ε*

    Parameters
    ----------
    mat_path_TD86    : path to prepdata_big_TD86.mat
    csv_path_wages   : path to wageworker1986.csv
    tt86rw_override  : optional (noj, noj) ndarray to replace tt86rw from .mat
                       (the task 1b TTM from your Topic_7/TTM pipeline)
    alpha, beta      : factor shares (0.80, 0.75 from paper)
    kappaeps         : κε commuting decay (0.07 from paper's reduced-form)
    epsilon_lb/ub    : bounds for scalar optimizer (MATLAB LBD=2, UBD=24)
    verbose          : print progress

    Returns
    -------
    dict with keys:
        epsilon      float   — estimated ε (rounded to 2 decimal places)
        kappa        float   — derived κ = κε/ε
        varlwdata    float   — target moment: Var(ln Bezirk wages, data)
        varlwage86rw float   — achieved moment: Var(ln block wages, model)
        omega86rw    ndarray — (noj,) solved transformed wages
        wage86rw     ndarray — (noj,) block adjusted wages at estimated ε
        obsvar86rw   ndarray — (noj, 4) obsdata matrix used
    """
    # ── 1. Load data ──────────────────────────────────────────────────────────
    if verbose:
        print(">>>> Loading prepdata_big_TD86 <<<<")
    data86 = load_prepdata_TD86(mat_path_TD86)

    floor86rw  = data86["floor86rw"]
    empwpl86rw = data86["empwpl86rw"]
    emprsd86rw = data86["emprsd86rw"]
    nobs86rw   = data86["nobs86rw"]
    bzk86rw    = data86["bzk86rw"]
    tt86rw     = data86["tt86rw"] if tt86rw_override is None else tt86rw_override

    if tt86rw_override is not None and verbose:
        print(f">>>> Using overridden travel time matrix (shape {tt86rw.shape}) <<<<")

    lbwdata, varlwdata = load_bezirke_wages_1986(csv_path_wages)
    if verbose:
        print(f">>>> Target Var(ln Bezirk wages): {varlwdata:.6f} <<<<")

    # ── 2. Build obsvar86rw ───────────────────────────────────────────────────
    obsvar86rw = np.zeros((nobs86rw, 4))
    obsvar86rw[:, 0] = floor86rw
    obsvar86rw[:, 1] = empwpl86rw
    obsvar86rw[:, 2] = emprsd86rw
    # column 3 (area) left as zeros — unused by comegaoptO

    # ── 3. Initial omega (Eq. 12 with A=1, brings values into plausible range) ─
    omega_init = np.zeros(nobs86rw)
    pos = empwpl86rw > 0
    omega_init[pos] = (
        ((1 - alpha) / floor86rw[pos]) ** ((1 - alpha) / alpha)
        * alpha
    )

    # ── 4. Solve for ω once (pre-optimization, ε not yet known) ──────────────
    if verbose:
        print(">>>> Solving for transformed wages ω (pre-optimization) <<<<")
    omega86rw, _, wconverge, HMC86rw, wgap86rw = solve_omega_O(
        obsvar86rw, tt86rw, nobs86rw, omega_init, kappaeps, verbose=verbose
    )
    if verbose:
        print(f"     converge={wconverge}, gap={wgap86rw}")

    # ── 5. Optimize ε ─────────────────────────────────────────────────────────
    if verbose:
        print(">>>> Optimizing ε via minimize_scalar (bounded) <<<<")

    result = minimize_scalar(
        fun=_objective_epsilon,
        bounds=(epsilon_lb, epsilon_ub),
        method="bounded",
        args=(omega86rw, empwpl86rw, bzk86rw, varlwdata),
        options={"xatol": 1e-6, "maxiter": 500},
    )

    # Round to 2 decimal places (MATLAB: epsilon=round(epsilon.*100)./100)
    epsilon_hat = round(result.x * 100) / 100
    kappa_hat   = kappaeps / epsilon_hat

    # ── 6. Final block wages and variance ────────────────────────────────────
    wage86rw = omega86rw ** (1.0 / epsilon_hat)
    pos = wage86rw > 0
    if pos.any():
        wage86rw[pos] = wage86rw[pos] / gmean(wage86rw[pos])

    Ewage = wage86rw[wage86rw > 0]
    varlwage86rw = float(np.var(np.log(Ewage), ddof=1))

    if verbose:
        print(f">>>> Estimated ε:          {epsilon_hat} <<<<")
        print(f">>>> Implied κ:            {kappa_hat:.6f} <<<<")
        print(f">>>> Var(ln wage, data):   {varlwdata:.6f} <<<<")
        print(f">>>> Var(ln wage, blocks): {varlwage86rw:.6f} <<<<")

    return {
        "epsilon":      epsilon_hat,
        "kappa":        kappa_hat,
        "varlwdata":    varlwdata,
        "varlwage86rw": varlwage86rw,
        "omega86rw":    omega86rw,
        "wage86rw":     wage86rw,
        "obsvar86rw":   obsvar86rw,
    }
