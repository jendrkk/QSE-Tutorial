"""
estimation.py — Task 1b: estimate ε (and hence κ) by moment matching.

optimepsilon_TD86 logic: with ε·κ fixed at the gravity estimate, solve the transformed
wages omega ONCE (the inner commuting solve depends only on ε·κ, not on ε separately),
then choose ε so the model's cross-Bezirk variance of log wages matches the data moment
Var(log Bezirke wages) = 0.004753 from wageworker1986.csv (West Berlin, 1986). Finally
κ = (ε·κ) / ε.
"""
from __future__ import annotations
import numpy as np
import core


def estimate_epsilon_kappa(obsvar, tt_west, bzk1937, kappaeps, var_data,
                           alpha=0.80, beta=0.75, verbose=True):
    """
    obsvar  : (Nw,4) West 1986 block data [Q, empwpl(adj), emprsd, area].
    tt_west : (Nw,Nw) West travel-time matrix (canonical West order).
    bzk1937 : historic Bezirke (1-23) of the West blocks.
    kappaeps: ε·κ from Task 1a.
    var_data: target moment (Var of log Bezirke wages).
    Returns dict with epsilon, kappa, var_model, converged, gap.
    """
    noj = obsvar.shape[0]
    HMT = obsvar[:, 1]
    Iwpl = HMT != 0
    QT = obsvar[:, 0]
    init = np.zeros(noj)
    init[Iwpl] = ((1 - alpha) / QT[Iwpl]) ** ((1 - alpha) / alpha) * alpha   # wage guess

    omega, conv, HMC, gap = core.solve_omega(
        obsvar, tt_west, noj, init, mode="O",
        alpha=alpha, beta=beta, kappaeps=kappaeps, verbose=verbose)

    # ε identified from the Bezirk-wage dispersion; only workplace blocks contribute
    eps = core.estimate_epsilon(omega[Iwpl], HMT[Iwpl], bzk1937[Iwpl], var_data)
    kappa = kappaeps / eps
    var_model = core.bezirk_logwage_var(omega[Iwpl].copy(), HMT[Iwpl], bzk1937[Iwpl], eps)
    if verbose:
        print(f"  ε = {eps:.2f}   κ = {kappa:.5f}   "
              f"Var_model = {var_model:.6f}  (target {var_data:.6f})  converged={conv}")
    return {"epsilon": eps, "kappa": kappa, "kappaeps": kappaeps,
            "var_model": var_model, "var_data": var_data, "converged": conv, "gap": gap}
