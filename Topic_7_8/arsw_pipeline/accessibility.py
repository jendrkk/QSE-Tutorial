"""
accessibility.py — block-level commuter market access for Topic 8 / Task 1(b).

ARSW commuter market access for a residence block i:

    CMA_i = Σ_{j ∈ Iwpl}  exp(−εκ τ_ij) · w_j^ε

This is the same object that appears inside the Phi_i denominator of the
solver's commuting probabilities:

    Phi_i = (Q_i^{−(1−β)ε} · B_i^ε) · CMA_i

so the CMA vector can either be (a) built directly from (tt, w) — a single
matvec — or (b) recovered from solver internals via division by the QB term.
Both routines are exposed; the orchestrator uses (a) for cross-equilibrium
comparisons (where wages differ between base and U5) and (b) as a sanity
check on the solver's own residence component.

Memory: (a) materialises exp(−εκ τ[Irsd, Iwpl]) once (~1.2 GB at Berlin
scale); this is identical to the kernel the solver builds. Callers that
already have the solver result for the same TTM should prefer (b).
"""
from __future__ import annotations
import numpy as np


def compute_cma_residence(tt, wage, *, epsilon, kappaeps, Iwpl, Irsd,
                          fill=0.0):
    """Build the residence-side CMA vector for an arbitrary (tt, wage) pair.

    Parameters
    ----------
    tt : (N, N) ndarray
        Travel time matrix in canonical block order (rows = residences,
        cols = workplaces). Must contain no NaN.
    wage : (N,) ndarray
        Workplace wage vector; only entries on `Iwpl` are read.
    epsilon, kappaeps : floats
        Frechet shape and reduced-form ε·κ from Tasks 1b / 1a.
    Iwpl, Irsd : (N,) bool ndarrays
        Specialisation masks (typically those returned by `solve_equilibrium`).
    fill : float, default 0.0
        Value placed at non-Irsd positions in the output.

    Returns
    -------
    cma : (N,) ndarray
        CMA[i] for i in Irsd; `fill` elsewhere.
    """
    tt = np.asarray(tt, float)
    wage = np.asarray(wage, float)
    N = tt.shape[0]
    if tt.shape != (N, N):
        raise ValueError(f"tt must be square (N×N); got {tt.shape}")
    if wage.shape[0] != N:
        raise ValueError(f"wage length {wage.shape[0]} != N={N}")
    ri = np.where(Irsd)[0]
    wi = np.where(Iwpl)[0]
    d = np.exp(-kappaeps * tt[np.ix_(ri, wi)])         # (nrsd, nwpl)
    w_pow = wage[wi] ** epsilon                          # (nwpl,)
    cma_block = d @ w_pow                                # (nrsd,)
    cma = np.full(N, fill, dtype=float)
    cma[Irsd] = cma_block
    return cma


def extract_cma_from_phi(Phi_i, Q, B, *, epsilon, beta, Irsd):
    """Recover CMA from a solver's Phi_i by dividing out the Q^{−(1−β)ε}·B^ε term.

    Use this when CMA is needed for the *same* (tt, wage) the solver just used
    (e.g. for solver self-consistency checks) and you want to avoid rebuilding
    the ~1 GB exp(−εκ τ) kernel.

    Parameters
    ----------
    Phi_i : (nrsd,) ndarray
        Row sums of Ephi_ij from the solver iteration (NOT divided by Phi).
        If you only have `result["ucprob_block"]`, recover Phi_i with
        `Phi_i = result["ucprob_block"].sum(axis=1) * result["Phi"]`.
    Q : (N,) ndarray
        Floor space prices used INSIDE the commuting probability (= `fund[:,4]`
        = data QT; not the solver's iterated Q_i).
    B : (N,) ndarray
        Amenities (calibration output).
    epsilon, beta : floats
    Irsd : (N,) bool ndarray

    Returns
    -------
    cma : (N,) ndarray
        CMA[i] for i in Irsd; 0 elsewhere.
    """
    Q = np.asarray(Q, float); B = np.asarray(B, float)
    Phi_i = np.asarray(Phi_i, float)
    N = Q.shape[0]
    QB_term = (Q[Irsd] ** (-(1.0 - beta) * epsilon)) * (B[Irsd] ** epsilon)
    cma_block = Phi_i / QB_term
    cma = np.zeros(N, dtype=float)
    cma[Irsd] = cma_block
    return cma
