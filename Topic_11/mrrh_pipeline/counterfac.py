"""
counterfac.py — Task 1(c) counterfactual solver.

Exact-hat-algebra translation of progs/counterFactsTK.m and the eight
updateXxxTK.m inner-loop functions (Codebook Section A.2). All matrices are
oriented [n, i] = [residence, workplace] for lambda, and [n, i] for trade shares
pi with spending destination i in columns — identical to the quantification.

Forcing variables are relative changes (hats): aChange (Nx1 productivity),
bChange (NxN residence-amenity), kapChange (NxN commuting cost), dChange (NxN
trade cost). Unchanged primitives are ones.

The welfare change is a common scalar (the same for every cell of the NxN welfare
matrix, up to numerical noise); we return welf[0, 0].
"""
from __future__ import annotations
import numpy as np
from . import config as C


# ---- inner-loop updates ---------------------------------------------------- #
def _upd_res_wage(bC, wC, kapC, lamObs, vObs, wObs, epsi):
    numr = (bC * lamObs * kapC ** (-epsi)) @ (wC ** (1 + epsi) * wObs)
    den = (bC * lamObs * kapC ** (-epsi)) @ (wC ** epsi)
    return (numr / den) / vObs


def _upd_empl(lamC, lamObs, lObs, lBar):
    return lBar * ((lamObs * lamC).sum(axis=0) / lObs)


def _upd_resid(lamC, lamObs, rObs, lBar):
    return lBar * ((lamObs * lamC).sum(axis=1) / rObs)


def _upd_house(vC, rC, delta):
    return (vC * rC) ** (1.0 / (1.0 + delta))


def _upd_tradesh(lC, dC, wC, aC, piObs, sigg, nu):
    n = len(lC)
    num = aC ** (sigg - 1) * lC ** (1 - (1 - sigg) * nu) * wC ** (1 - sigg)
    nummat = dC ** (1 - sigg) * np.tile(num[:, None], (1, n))
    denom = (piObs * nummat).sum(axis=0)
    return nummat / np.tile(denom, (n, 1))


def _upd_prices(lC, wC, piC, aC, dC, sigg, nu):
    return (lC ** (1 - (1 - sigg) * nu) / np.diag(piC)) ** (1.0 / (1 - sigg)) \
        * np.diag(dC) * wC / aC


def _upd_wage(lC, piC, vC, rC, lObs, wObs, piObs, vObs, rObs):
    n = len(lC)
    nummat = piObs * piC
    vr = np.tile((vC * rC * vObs * rObs)[None, :], (n, 1))
    num = (nummat * vr).sum(axis=1)
    denom = wObs * lObs * lC
    return num / denom


def _upd_lam(bC, pC, qC, wC, kapC, lamObs, alp, epsi):
    n = len(pC)
    pq = np.tile((pC ** alp * qC ** (1 - alp))[:, None], (1, n))
    wm = np.tile(wC[None, :], (n, 1))
    nummat = bC * pq ** (-epsi) * (wm / kapC) ** epsi
    denom = (lamObs * nummat).sum()
    return nummat / denom


# ---- outer solver ---------------------------------------------------------- #
def counter_facts(aChange, bChange, kapChange, dChange,
                  wObs, vObs, lamObs, lObs, rObs, piObs,
                  alp=C.ALP, epsi=C.EPSI, delta=C.DELTA, sigg=C.SIGG, nu=C.NU,
                  tol=C.CF_TOL, maxit=C.CF_MAXITER, relax=C.CF_RELAX):
    """Solve for relative changes. Returns dict of hats + welfare + iters."""
    n = len(aChange)
    lBar = lObs.sum()
    wC = np.ones(n)
    lamC = np.ones((n, n))
    k = 0
    for k in range(maxit):
        vC = _upd_res_wage(bChange, wC, kapChange, lamObs, vObs, wObs, epsi)
        lC = _upd_empl(lamC, lamObs, lObs, lBar)
        rC = _upd_resid(lamC, lamObs, rObs, lBar)
        qC = _upd_house(vC, rC, delta)
        piC = _upd_tradesh(lC, dChange, wC, aChange, piObs, sigg, nu)
        pC = _upd_prices(lC, wC, piC, aChange, dChange, sigg, nu)
        wT = _upd_wage(lC, piC, vC, rC, lObs, wObs, piObs, vObs, rObs)
        wnew = (wT * wObs) / np.mean(wT * wObs)          # renormalise wage level
        wT = wnew / wObs
        lamT = _upd_lam(bChange, pC, qC, wC, kapChange, lamObs, alp, epsi)
        if np.all(np.abs(wC - wT) < tol) and np.all(np.abs(lamC - lamT) < tol):
            wC, lamC = wT, lamT
            break
        wC = relax * wT + (1 - relax) * wC
        lamC = relax * lamT + (1 - relax) * lamC

    # recompute dependent hats at the converged (wC, lamC)
    vC = _upd_res_wage(bChange, wC, kapChange, lamObs, vObs, wObs, epsi)
    lC = _upd_empl(lamC, lamObs, lObs, lBar)
    rC = _upd_resid(lamC, lamObs, rObs, lBar)
    qC = _upd_house(vC, rC, delta)
    piC = _upd_tradesh(lC, dChange, wC, aChange, piObs, sigg, nu)
    pC = _upd_prices(lC, wC, piC, aChange, dChange, sigg, nu)

    pq = np.tile((pC ** alp * qC ** (1 - alp))[:, None], (1, n))
    wm = np.tile(wC[None, :], (n, 1))
    welf = bChange ** (1.0 / epsi) * (kapChange * pq) ** (-1) * wm * lamC ** (-1.0 / epsi)

    return dict(w=wC, v=vC, q=qC, pi=piC, lam=lamC, p=pC, r=rC, l=lC,
                welf=float(welf[0, 0]), iters=k, welf_mat_sd=float(np.std(welf)))