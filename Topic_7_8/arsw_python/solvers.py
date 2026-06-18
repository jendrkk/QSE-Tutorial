"""
solvers.py
ARSW2015 inner-loop omega solvers.

solve_omega_O  ≡  comegaoptO.m   (Task 1b: epsilon estimation)
solve_omega_C  ≡  comegaoptC.m   (Task 1c: calibration)
"""
from __future__ import annotations
import numpy as np
from scipy.stats import gmean
from scipy.sparse import lil_matrix


# ─────────────────────────────────────────────────────────────────────────────
# Shared fixed-point loop
# ─────────────────────────────────────────────────────────────────────────────

def _iterate_omega(
    Eomega: np.ndarray,
    EHMT: np.ndarray,
    EHRT: np.ndarray,
    Ecc: np.ndarray,
    max_iter: int = 500,
    gaptol: float = 0.0,
    round_scale: int = 100_000,
    verbose: bool = True,
) -> tuple[np.ndarray, np.ndarray, bool, np.ndarray, float]:
    """
    Fixed-point iteration for ARSW Eq. S.44.

    Parameters
    ----------
    Eomega      : (nto,) initial transformed wages for workplace locations
    EHMT        : (nto,) observed workplace employment
    EHRT        : (nfrom,) observed residence employment
    Ecc         : (nto, nfrom) commuting cost matrix  exp(rate * τ_{ij})
    max_iter    : maximum iterations before forced exit
    gaptol      : convergence tolerance on scaled employment gaps (0 = exact)
    round_scale : integer scaling before rounding for gap check
                  comegaoptO uses 100_000; comegaoptC uses 10_000
    verbose     : print convergence messages

    Returns
    -------
    Eomega    : (nto,) converged transformed wages
    Ecprob    : (nto, nfrom) commuting probability matrix
    wconverge : bool — True if gap <= gaptol reached
    EHMC      : (nto,) predicted workplace employment at convergence
    gap       : largest absolute scaled employment gap
    """
    rng = np.random.default_rng(seed=1)
    nto, nfrom = Ecc.shape
    wconverge = False
    gap = np.inf
    Ecprob = np.empty((nto, nfrom))

    for _ in range(max_iter):
        # numerator: ω_j / c_{ij}  shape (nto, nfrom)
        Ecnum = Eomega[:, np.newaxis] / Ecc
        # denominator: Σ_k ω_k / c_{ik}  shape (nfrom,)
        Ecdenom = Ecnum.sum(axis=0)
        # conditional commuting probabilities (nto, nfrom)
        Ecprob = Ecnum / Ecdenom[np.newaxis, :]

        # predicted workplace employment  (nto,)
        EHMC = Ecprob @ EHRT

        # tolerance check (replicates MATLAB rounding)
        gap_vec = np.abs(
            np.round(EHMC * round_scale).astype(np.int64)
            - np.round(EHMT * round_scale).astype(np.int64)
        )
        gap = float(gap_vec.max())

        if np.isnan(gap):
            # NaN fallback: random restart (MATLAB lines 100-104 in comegaoptO)
            Eomega = rng.uniform(0.95, 1.05, size=nto)
            Ecprob = np.ones((nto, nfrom)) / nto
            continue

        if gap <= gaptol:
            wconverge = True
            if verbose:
                print(">>>> Wage System Converged <<<<")
            break

        # Proportional update: inflate ω where observed > predicted
        Eomega_e = (EHMT / EHMC) * Eomega
        # Damped combination (weight 0.5 — MATLAB line 108)
        Eomega = 0.5 * Eomega_e + 0.5 * Eomega
        # Normalize to geometric mean of 1 (identified up to a constant)
        Eomega = Eomega / gmean(Eomega)

    if verbose and not wconverge:
        print(f"Warning: omega solver stopped at gap={gap:.2f} without convergence")

    return Eomega, Ecprob, wconverge, EHMC, gap


# ─────────────────────────────────────────────────────────────────────────────
# comegaoptO  —  Task 1b (epsilon estimation)
# ─────────────────────────────────────────────────────────────────────────────

def solve_omega_O(
    obsdata: np.ndarray,
    distvar: np.ndarray,
    noj: int,
    omega: np.ndarray,
    kappaeps: float,
    verbose: bool = True,
) -> tuple[np.ndarray, np.ndarray, bool, np.ndarray, float]:
    """
    Solve for TRANSFORMED wages ω satisfying Eq. S.44.
    Equivalent to comegaoptO.m.

    Commuting cost matrix:  c_{ij} = exp(κε · τ_{ij})
    Tolerance round scale:  100 000

    Parameters
    ----------
    obsdata  : (noj, 4)  columns: [floor_price, empwpl, emprsd, area]
               (area column is accepted but unused, matching MATLAB)
    distvar  : (noj, noj) travel time matrix
    noj      : total number of blocks
    omega    : (noj,) initial guess of transformed wages
    kappaeps : κε commuting decay parameter

    Returns
    -------
    omout     : (noj,) equilibrium transformed wages
                (zeros for blocks with zero workplace employment)
    cprob     : (noj, noj) full commuting probability matrix
    wconverge : bool
    HMC       : (noj,) predicted workplace employment
    gap       : largest absolute scaled employment gap
    """
    HMT = obsdata[:, 1]
    HRT = obsdata[:, 2]

    Iwpl = HMT != 0
    Irsd = HRT != 0
    wpl_idx = np.where(Iwpl)[0]
    rsd_idx = np.where(Irsd)[0]

    EHMT    = HMT[Iwpl]
    EHRT    = HRT[Irsd]
    Eomega  = omega[Iwpl].copy()
    # Cost matrix: rows = workplaces, cols = residences
    Ecc     = np.exp(kappaeps * distvar[np.ix_(wpl_idx, rsd_idx)])

    if verbose:
        print(">>>> Calibrating omegas (comegaoptO) <<<<")

    Eomega, Ecprob, wconverge, EHMC, gap = _iterate_omega(
        Eomega, EHMT, EHRT, Ecc,
        round_scale=100_000, verbose=verbose,
    )

    # Fill back into full-length (noj,) vectors
    omout = np.zeros(noj)
    omout[Iwpl] = Eomega

    HMC = np.zeros(noj)
    HMC[Iwpl] = EHMC

    cprob = np.zeros((noj, noj))
    cprob[np.ix_(wpl_idx, rsd_idx)] = Ecprob

    return omout, cprob, wconverge, HMC, gap


# ─────────────────────────────────────────────────────────────────────────────
# comegaoptC  —  Task 1c (calibration)
# ─────────────────────────────────────────────────────────────────────────────

def solve_omega_C(
    obsdata: np.ndarray,
    distvar: np.ndarray,
    noj: int,
    wgin: np.ndarray,
    alpha: float,
    beta: float,
    epsilon: float,
    kappa: float,
    verbose: bool = True,
) -> tuple[np.ndarray, np.ndarray, object, bool, np.ndarray, float]:
    """
    Solve for ADJUSTED wages w, then recover productivities A.
    Equivalent to comegaoptC.m.

    Commuting cost matrix:  c_{ij} = exp(ε·κ·τ_{ij})
    Tolerance round scale:  10 000

    Parameters
    ----------
    obsdata  : (noj, 4)  columns: [floor_price, empwpl, emprsd, area]
    distvar  : (noj, noj) travel time matrix
    noj      : total number of blocks
    wgin     : (noj,) initial guess of ADJUSTED wages
    alpha, beta, epsilon, kappa : model parameters

    Returns
    -------
    wgout     : (noj,) equilibrium adjusted wages (normalized to geomean=1)
    Aout      : (noj,) adjusted productivities (Eq. S.48)
    cprob     : (noj, noj) sparse CSR commuting probability matrix
    wconverge : bool
    HMC       : (noj,) predicted workplace employment
    gap       : largest absolute scaled employment gap
    """
    QT  = obsdata[:, 0]
    HMT = obsdata[:, 1]
    HRT = obsdata[:, 2]

    # Convert adjusted wages → transformed wages  (ω = w^ε)
    omega = wgin ** epsilon

    Iwpl = HMT != 0
    Irsd = HRT != 0
    wpl_idx = np.where(Iwpl)[0]
    rsd_idx = np.where(Irsd)[0]

    EHMT    = HMT[Iwpl]
    EHRT    = HRT[Irsd]
    Eomega  = omega[Iwpl].copy()
    # Cost matrix uses ε·κ (differs from comegaoptO which uses kappaeps only)
    Ecc     = np.exp(epsilon * kappa * distvar[np.ix_(wpl_idx, rsd_idx)])

    if verbose:
        print(">>>> Calibrating omegas (comegaoptC) <<<<")

    Eomega, Ecprob, wconverge, EHMC, gap = _iterate_omega(
        Eomega, EHMT, EHRT, Ecc,
        round_scale=10_000, verbose=verbose,
    )

    # Map transformed wages back to full vector
    omega_full = np.zeros(noj)
    omega_full[Iwpl] = Eomega

    HMC = np.zeros(noj)
    HMC[Iwpl] = EHMC

    # Build sparse commuting probability matrix (memory-efficient for large noj)
    cprob = lil_matrix((noj, noj))
    cprob[np.ix_(wpl_idx, rsd_idx)] = Ecprob
    cprob = cprob.tocsr()

    # Recover adjusted wages from transformed wages  (w = ω^{1/ε})
    wgout = omega_full ** (1.0 / epsilon)
    pos = wgout > 0
    if pos.any():
        wgout[pos] = wgout[pos] / gmean(wgout[pos])

    # Recover adjusted productivity A (Eq. S.48 bottom):
    #   Ã_j = (q_j / (1-α))^{1-α} · (w_j / α)^α
    Aout = np.zeros(noj)
    Aout[Iwpl] = (
        (QT[Iwpl] / (1 - alpha)) ** (1 - alpha)
        * (wgout[Iwpl] / alpha) ** alpha
    )

    return wgout, Aout, cprob, wconverge, HMC, gap
