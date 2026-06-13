"""
calibration.py — Task 1c: recover fundamental productivities A and amenities B
(and wages, expected income, density, commercial share) by inverting the model.

Sequence (calcal_TD.m):
  1. solve_omega(mode='C')  -> adjusted wages w and raw productivity A   (comegaoptC)
  2. recover_amenities       -> adjusted amenities B, residential CMA      (camen)
  3. compute_expected_income -> total worker income at residence vv        (expincome)
  4. adjust_levels           -> A (geomean=1), B (model pop = data pop), w  (calcal_adj_TD)
  5. compute_density         -> density of development V, floor space L, θ  (cdensity)

Inputs use ε·κ (Task 1a) for the commuting cost and (ε, κ) (Task 1b) for the wage/income
margins, exactly as the MATLAB toolkit does.
"""
from __future__ import annotations
import numpy as np
import core


def calibrate(obsvar, tt, *, epsilon, kappa, alpha=0.80, beta=0.75, verbose=True):
    """Return a dict of recovered fundamentals, all in canonical block order:
       A, B, wage, CMA, vv, V, L, theta, plus solver diagnostics."""
    noj = obsvar.shape[0]
    QT, HMT = obsvar[:, 0], obsvar[:, 1]
    Iwpl = HMT != 0
    init = np.zeros(noj)
    init[Iwpl] = ((1 - alpha) / QT[Iwpl]) ** ((1 - alpha) / alpha) * alpha

    if verbose:
        print("  [1/5] solving wages + productivity (comegaoptC) ...")
    wage, A, conv, HMC, gap = core.solve_omega(
        obsvar, tt, noj, init, mode="C",
        alpha=alpha, beta=beta, epsilon=epsilon, kappa=kappa, verbose=verbose)

    if verbose:
        print("  [2/5] recovering amenities (camen) ...")
    B, CMA, HRS = core.recover_amenities(
        obsvar, tt, noj, wage, alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon)

    if verbose:
        print("  [3/5] expected income (expincome) ...")
    vv = core.compute_expected_income(
        obsvar, tt, noj, wage, B, alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon)

    if verbose:
        print("  [4/5] adjusting levels (calcal_adj_TD) ...")
    A, B, wage = core.adjust_levels(
        obsvar, tt, noj, A, B, alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon)
    # expected income consistent with rescaled fundamentals
    vv = core.compute_expected_income(
        obsvar, tt, noj, wage, B, alpha=alpha, beta=beta, kappa=kappa, epsilon=epsilon)

    if verbose:
        print("  [5/5] density of development (cdensity) ...")
    V, L, theta = core.compute_density(obsvar, A, wage, vv, noj, alpha=alpha, beta=beta)

    return {"A": A, "B": B, "wage": wage, "CMA": CMA, "vv": vv,
            "V": V, "L": L, "theta": theta,
            "converged": conv, "gap": gap, "Iwpl": Iwpl, "Irsd": HMT != 0}
