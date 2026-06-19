"""
single_pipeline.py — full Topic-7 (Task 1b ε estimation + Task 1c calibration)
+ Topic-8 (baseline equilibrium) pipeline for ONE reference TTM. Plus a thin
`solve_alt` that re-uses the REF's fund matrix on an ALT TTM.

Closed-city ARSW canonical counterfactual: fundamentals (A, B, V, θ, w, vv) are
exogenous to the TTM change. They are inverted from data + the REF TTM and the
same `fund` matrix is used for both the REF and the ALT equilibrium solve.
"""
from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Optional
import time

import numpy as np

import config
import dataio
import estimation
import calibration
import solver

from .ttm_catalog import TTMSpec


@dataclass
class RefState:
    """Calibrated reference state. The same `fund` and (ε, κ, εκ) drive every
    ALT equilibrium solve in the closed-city counterfactual.

    Attributes
    ----------
    spec        : TTMSpec for the REF TTM
    epsilon     : Frechet shape (Task 1b)
    kappa       : commuting cost intensity = εκ / ε
    kappaeps    : reduced-form ε·κ (Task 1a, REF row)
    est         : full output dict of estimation.estimate_epsilon_kappa
    cal         : full output dict of calibration.calibrate
    fund        : (N, 12) ndarray from solver.build_fund
    res         : baseline equilibrium solver result for the REF TTM
    consistency : solver.self_consistency_report for the REF baseline
    timings_s   : per-stage wall-clock timings
    """
    spec:        TTMSpec
    epsilon:     float
    kappa:       float
    kappaeps:    float
    est:         dict
    cal:         dict
    fund:        np.ndarray
    res:         dict
    consistency: dict
    timings_s:   dict = field(default_factory=dict)


def calibrate_and_solve_ref(spec: TTMSpec, tt_ref: np.ndarray, d06: dict, *,
                            kappaeps: float,
                            wageworker_var_data: float,
                            alpha: float = config.ALPHA,
                            beta:  float = config.BETA,
                            max_iter: int = 1000,
                            solver_weight: float = 0.5,
                            verbose: bool = True) -> RefState:
    """Run Task 1b (ε estimation via West-restricted forward solve) + Task 1c
    (full calibration on REF TTM) + Topic 8 baseline equilibrium for REF.

    Parameters
    ----------
    spec : TTMSpec for the REF TTM (metadata only; tt_ref is the actual matrix).
    tt_ref : (N, N) canonical-order float64 TTM (NaN-free, diag zero).
    d06 : dataio.load_block_data(2006) output.
    kappaeps : Task 1a ε·κ for this REF TTM (already estimated by gravity).
    wageworker_var_data : target moment from dataio.load_wageworker_var().
    alpha, beta : ARSW parameters (defaults from config).
    max_iter, solver_weight : equilibrium solver knobs.
    """
    timings = {}

    # ─── Task 1b: ε estimation (West-restricted forward solve) ──────────
    if verbose:
        print(f"\n  [REF={spec.key}]  Task 1b — ε estimation ...")
    t0 = time.time()
    d86w = dataio.load_block_data(1986, west_only=True)
    west_idx = d06["west_idx"]
    tt_west = tt_ref[np.ix_(west_idx, west_idx)]
    est = estimation.estimate_epsilon_kappa(
        d86w["obsvar"], tt_west, d86w["bzk1937"], kappaeps,
        wageworker_var_data, alpha=alpha, beta=beta, verbose=verbose)
    del tt_west, d86w
    timings["task1b_sec"] = time.time() - t0
    epsilon = float(est["epsilon"])
    kappa   = float(est["kappa"])
    if verbose:
        print(f"    ε = {epsilon:.3f}, κ = {kappa:.5f} "
              f"(elapsed {timings['task1b_sec']:.1f}s)")

    # ─── Task 1c: full calibration on REF TTM ──────────────────────────
    if verbose:
        print(f"  [REF={spec.key}]  Task 1c — full calibration ...")
    t0 = time.time()
    cal = calibration.calibrate(
        d06["obsvar"], tt_ref,
        epsilon=epsilon, kappa=kappa,
        alpha=alpha, beta=beta, verbose=verbose)
    timings["task1c_sec"] = time.time() - t0
    if verbose:
        print(f"    calibration converged={bool(cal['converged'])} "
              f"(elapsed {timings['task1c_sec']:.1f}s)")

    # ─── Topic 8 baseline: equilibrium on REF TTM with REF fundamentals ─
    if verbose:
        print(f"  [REF={spec.key}]  Topic 8 baseline equilibrium ...")
    t0 = time.time()
    fund = solver.build_fund(d06["obsvar"], cal)
    res = solver.solve_equilibrium(
        fund, tt_ref, epsilon=epsilon, kappaeps=kappaeps,
        alpha=alpha, beta=beta, max_iter=max_iter, weight=solver_weight,
        verbose=verbose)
    timings["solve_ref_sec"] = time.time() - t0
    consistency = solver.self_consistency_report(res, d06["obsvar"], cal,
                                                 verbose=verbose)
    if verbose:
        print(f"    REF equilibrium: converged={bool(res['converged'])}, "
              f"iters={res['n_iter']} (elapsed {timings['solve_ref_sec']:.1f}s)")

    return RefState(
        spec=spec, epsilon=epsilon, kappa=kappa, kappaeps=kappaeps,
        est=est, cal=cal, fund=fund, res=res,
        consistency=consistency, timings_s=timings)


def solve_alt(tt_alt: np.ndarray, ref_state: RefState, *,
              alpha: float = config.ALPHA, beta: float = config.BETA,
              max_iter: int = 1000, solver_weight: float = 0.5,
              verbose: bool = True) -> dict:
    """Solve the ALT equilibrium reusing REF's fund matrix and (ε, εκ).

    The closed-city ARSW counterfactual holds fundamentals fixed; only τ_ij
    changes. The solver re-derives commuting probabilities, predicted (HM, HR,
    Y, w, q, Q, θ), and the resulting aggregates (Ū, Φ).
    """
    t0 = time.time()
    res = solver.solve_equilibrium(
        ref_state.fund, tt_alt,
        epsilon=ref_state.epsilon, kappaeps=ref_state.kappaeps,
        alpha=alpha, beta=beta, max_iter=max_iter, weight=solver_weight,
        verbose=verbose)
    if verbose:
        print(f"    ALT equilibrium: converged={bool(res['converged'])}, "
              f"iters={res['n_iter']} (elapsed {time.time() - t0:.1f}s)")
    return res
