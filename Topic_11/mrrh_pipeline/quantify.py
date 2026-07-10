"""
quantify.py — Task 1(a) and 1(b) quantification.

(a) solve_product_trade: recovers fundamental productivities A_n that satisfy the
    income = expenditure condition (SW2020 eq. 12), together with bilateral trade
    shares pi_ni (eq. 10) and the tradable price index P_n. Direct translation of
    progs/solveProductTradeTK.m.

(b) qol_residual: recovers residential amenity (quality of life) b_n as an
    ARSW2015-style structural residual from the unconditional residence-choice
    probability:

        R_n / L  ∝  b_n · (P_n^ALP · Q_n^(1-ALP))^(-EPSI) · CMA_n
        CMA_n     =  Σ_i tau_ni^(-EPSI*MU) · w_i^EPSI       (commuter market access)

    invert:  b_n ∝ (R_n/L) · (P_n^ALP Q_n^(1-ALP))^EPSI / CMA_n,   geomean-normalised.

    b_n and CMA_n are not separately identified without commuting costs, so tau
    (Tutorial-9 travel time, with imputed diagonal) is required — this is exactly
    the ARSW inversion logic.
"""
from __future__ import annotations
import numpy as np
from . import config as C


def solve_product_trade(L_n, R_n, w_n, v_n, dni,
                        sigg=C.SIGG, nu=C.NU, fixC=C.FIXC,
                        relax=0.25, maxiter=C.PROD_MAXITER, prec=C.PROD_PREC):
    """Return (A_n, tradesh[n,i], tradeshOwn, P_n, iters, final_gap)."""
    rrho = nu
    n = len(L_n)
    product = np.ones(n)
    income = w_n * L_n
    it = 0
    for it in range(maxiter):
        num = product ** (sigg - 1) * L_n ** (1 - (1 - sigg) * rrho) * w_n ** (1 - sigg)
        nummat = dni ** (1 - sigg) * np.tile(num, (n, 1))       # [n, i], origins in cols
        tradesh = nummat / nummat.sum(axis=1, keepdims=True)
        expend = tradesh.T @ (v_n * R_n)
        if np.all(np.round(np.abs(income - expend), prec) == 0):
            break
        product = relax * (product * (income / expend)) + (1 - relax) * product
        product = product / product.mean()

    # final trade shares with spending DESTINATION i in columns, plus P_n
    num = product ** (sigg - 1) * L_n ** (1 - (1 - sigg) * rrho) * w_n ** (1 - sigg)
    nummat = dni ** (1 - sigg) * np.tile(num[:, None], (1, n))
    tradesh = nummat / nummat.sum(axis=0, keepdims=True)
    tradeshOwn = np.diag(tradesh)
    P_n = sigg / (sigg - 1) * (L_n / (sigg * fixC * tradeshOwn)) ** (1 / (1 - sigg)) \
        * (w_n / product)
    return product, tradesh, tradeshOwn, P_n, it, float(np.max(np.abs(income - expend)))


def commuter_market_access(tau, w_n, epsi=C.EPSI, mu=C.MU):
    """CMA_n = Σ_i tau_ni^(-epsi*mu) w_i^epsi  (sum over workplaces i)."""
    return (tau ** (-epsi * mu) * (w_n ** epsi)[None, :]).sum(axis=1)


def qol_residual(R_n, L, P_n, Q_n, tau, w_n,
                 alp=C.ALP, epsi=C.EPSI, mu=C.MU):
    """Return (b_n geomean-normalised, CMA_n, CPI_n)."""
    CMA = commuter_market_access(tau, w_n, epsi, mu)
    CPI = P_n ** alp * Q_n ** (1 - alp)
    b = (R_n / L) * (CPI ** epsi) / CMA
    b = b / np.exp(np.log(b).mean())          # geometric mean 1
    return b, CMA, CPI