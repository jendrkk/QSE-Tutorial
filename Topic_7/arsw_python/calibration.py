"""
calibration.py
ARSW2015 Section 6 calibration functions.

mod_bezirk         ≡  modbezirk.m
recover_amenities  ≡  camen.m
compute_expected_income  ≡  expincome.m  (GA amended code path)
compute_density    ≡  cdensity.m
adjust_levels      ≡  calcal_adj_TD.m
"""
from __future__ import annotations
import numpy as np
from scipy.stats import gmean


# ─────────────────────────────────────────────────────────────────────────────
# modbezirk.m
# ─────────────────────────────────────────────────────────────────────────────

# Crosswalk: historic Bezirk code (1-23) → modern Bezirk code (1-12)
# Exactly mirrors modbezirk.m
_BEZIRK_CROSSWALK: dict[int, int] = {
    1: 1, 22: 1,        # Charlottenburg-Wilmersdorf
    2: 2,  5: 2,        # Friedrichshain-Kreuzberg
    7: 3,  4: 3,        # Lichtenberg
    8: 4,  3: 4,        # Marzahn-Hellersdorf
    9: 5, 18: 5, 20: 5, # Mitte
    10: 6,              # Neukolln
    11: 7, 12: 7, 21: 7,# Pankow
    13: 8,              # Reinickendorf
    15: 9,              # Spandau
    16: 10, 23: 10,     # Steglitz-Zehlendorf
    17: 11, 14: 11,     # Tempelhof-Schoneberg
    19: 12,  6: 12,     # Treptow-Kopenick
}


def mod_bezirk(bzk: np.ndarray, noj: int) -> np.ndarray:
    """
    Map 23 historic Bezirke codes → 12 modern Bezirke.
    Equivalent to modbezirk.m.

    Parameters
    ----------
    bzk : (noj,) int — historic Bezirk codes, 1-based (1-23)
    noj : int — total observations

    Returns
    -------
    (noj,) int — modern Bezirk codes (1-12); 0 for unmapped codes
    """
    out = np.zeros(noj, dtype=int)
    for old, new in _BEZIRK_CROSSWALK.items():
        out[bzk == old] = new
    return out


# ─────────────────────────────────────────────────────────────────────────────
# camen.m
# ─────────────────────────────────────────────────────────────────────────────

def recover_amenities(
    obsdata: np.ndarray,
    distvar: np.ndarray,
    noj: int,
    wage: np.ndarray,
    alpha: float,
    beta: float,
    kappa: float,
    epsilon: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Recover adjusted amenities B̃, commuting market access CMA, and
    residential employment shares HRS. Eq. S.47.
    Equivalent to camen.m.

    Note: camen.m accepts a `cprob` argument that it never uses internally.
    This translation omits it.

    Parameters
    ----------
    obsdata  : (noj, 4) [floor_price, empwpl, emprsd, area]
    distvar  : (noj, noj) travel time matrix
    noj      : int
    wage     : (noj,) adjusted wages from solve_omega_C
    alpha, beta, kappa, epsilon : model parameters

    Returns
    -------
    B   : (noj,) adjusted amenities
    CMA : (noj,) commuting market access (levels, not normalized)
    HRS : (noj,) residential employment share = HR_i / Σ HR_i
    """
    QT  = obsdata[:, 0]
    HMT = obsdata[:, 1]
    HRT = obsdata[:, 2]

    Iwpl = HMT != 0
    Irsd = HRT != 0
    wpl_idx = np.where(Iwpl)[0]
    rsd_idx = np.where(Irsd)[0]

    EHMT  = HMT[Iwpl]
    EHRT  = HRT[Irsd]
    Ewage = wage[Iwpl]

    # Spatial weights: residence rows, workplace cols  (nfrom × nto)
    Edistvar = distvar[np.ix_(rsd_idx, wpl_idx)]
    d_ij_eps = np.exp(-epsilon * kappa * Edistvar)

    # Residential employment share normalized to geomean=1
    EHRS = EHRT / EHRT.sum()
    EHRS_norm = EHRS / gmean(EHRS)

    # Floor space prices normalized to geomean=1
    EQT = QT[Irsd]
    EQT_norm = EQT / gmean(EQT)

    # Commuting market access: CMA_i = Σ_j exp(-εκτ_ij) w_j^ε  (nfrom,)
    ECMA = d_ij_eps @ (Ewage ** epsilon)
    ECMA_norm = ECMA / gmean(ECMA)

    # Eq. S.47: B̃_i^ε ∝ HR_i · q_i^{(1-β)ε} / CMA_i
    EB = (
        (EHRS_norm ** (1.0 / epsilon))
        * (EQT_norm ** (1 - beta))
        / (ECMA_norm ** (1.0 / epsilon))
    )

    B = np.zeros(noj)
    B[Irsd] = EB

    CMA = np.zeros(noj)
    CMA[Irsd] = ECMA        # levels, not normalized

    HRS = np.zeros(noj)
    HRS[Irsd] = EHRS        # not normalized

    return B, CMA, HRS


# ─────────────────────────────────────────────────────────────────────────────
# expincome.m  (GA amended code path)
# ─────────────────────────────────────────────────────────────────────────────

def compute_expected_income(
    obsdata: np.ndarray,
    distvar: np.ndarray,
    noj: int,
    wage: np.ndarray,
    B: np.ndarray,
    alpha: float,
    beta: float,
    kappa: float,
    epsilon: float,
) -> np.ndarray:
    """
    Total worker income at residence = E[w_j | i] × HR_i.
    Implements Eq. 4 (bilateral commuting probability numerator φ_ij)
    and Eq. S.20 (expected wage at residence).
    Equivalent to expincome.m (GA code path).

    Memory note: creates an (nrsd × nwpl) matrix Ephi_ij. For full Berlin
    this can be ~300MB. If you run out of memory, process in row-batches.

    Parameters
    ----------
    obsdata : (noj, 4)
    distvar : (noj, noj)
    noj     : int
    wage    : (noj,) adjusted wages
    B       : (noj,) adjusted amenities (from recover_amenities)
    alpha, beta, kappa, epsilon : model parameters

    Returns
    -------
    vvout : (noj,) total worker income at each residence block
    """
    QT  = obsdata[:, 0]
    HMT = obsdata[:, 1]
    HRT = obsdata[:, 2]

    Iwpl = HMT != 0
    Irsd = HRT != 0
    wpl_idx = np.where(Iwpl)[0]
    rsd_idx = np.where(Irsd)[0]
    nwpl = int(Iwpl.sum())
    nrsd = int(Irsd.sum())

    Ewage = wage[Iwpl]
    EB    = B[Irsd]
    EQT   = QT[Irsd]
    EHRT  = HRT[Irsd]

    # Spatial weights (nrsd × nwpl)
    Edistvar = distvar[np.ix_(rsd_idx, wpl_idx)]
    d_ij_eps = np.exp(-epsilon * kappa * Edistvar)

    # Eq. 4 numerator components (broadcasting replaces MATLAB repmat)
    EQQ = EQT[:, np.newaxis] ** (-(1 - beta) * epsilon)   # (nrsd, 1)
    EBB = EB[:, np.newaxis] ** epsilon                      # (nrsd, 1)
    EWW = Ewage[np.newaxis, :] ** epsilon                   # (1, nwpl)
    Ephi_ij = d_ij_eps * EQQ * EBB * EWW                   # (nrsd, nwpl)

    # Row sum = Σ_j φ_ij  (nrsd, 1)
    Ephi_i = Ephi_ij.sum(axis=1, keepdims=True)

    # Conditional P(work in j | live in i) = φ_ij / Σ_j φ_ij
    Epp_iji = Ephi_ij / Ephi_i                              # (nrsd, nwpl)

    # E[w | i] = Σ_j P(j|i) w_j  (Eq. S.20)
    EEWI = Epp_iji @ Ewage                                  # (nrsd,)

    # Total worker income at i
    ETWI = EEWI * EHRT                                      # (nrsd,)

    vvout = np.zeros(noj)
    vvout[Irsd] = ETWI
    return vvout


# ─────────────────────────────────────────────────────────────────────────────
# cdensity.m
# ─────────────────────────────────────────────────────────────────────────────

def compute_density(
    obsdata: np.ndarray,
    A: np.ndarray,
    wage: np.ndarray,
    vv: np.ndarray,
    noj: int,
    alpha: float,
    beta: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Recover density of development φ, total floor space L, and
    commercial floor space share θ. Eqs. S.29-S.31.
    Equivalent to cdensity.m.

    Parameters
    ----------
    obsdata : (noj, 4)  — area in column 3
    A       : (noj,) adjusted productivities
    wage    : (noj,) adjusted wages
    vv      : (noj,) total worker income (from compute_expected_income)
    noj     : int
    alpha, beta : model parameters

    Returns
    -------
    V     : (noj,) density of development φ  (Eq. S.31: L / K^0.75)
    LD    : (noj,) total floor space L
    theta : (noj,) commercial floor space share (LM / L)
    """
    QT  = obsdata[:, 0]
    HMT = obsdata[:, 1]
    HRT = obsdata[:, 2]
    K   = obsdata[:, 3]

    Iwpl = HMT != 0
    Irsd = HRT != 0

    # Commercial floor space demand (1-θ)L  (Eq. S.30)
    LM = np.zeros(noj)
    LM[Iwpl] = (
        ((1 - alpha) * A[Iwpl] / QT[Iwpl]) ** (1.0 / alpha)
        * HMT[Iwpl]
    )

    # Residential floor space demand θL  (Eq. S.29)
    LR = np.zeros(noj)
    LR[Irsd] = (1 - beta) * vv[Irsd] / QT[Irsd]

    LD = LM + LR

    # Density of development  φ = L / K^0.75  (Eq. S.31)
    V = np.zeros(noj)
    K_pos = K > 0
    V[K_pos] = LD[K_pos] / (K[K_pos] ** 0.75)

    # Commercial share
    theta = np.zeros(noj)
    LD_pos = LD > 0
    theta[LD_pos] = LM[LD_pos] / LD[LD_pos]

    return V, LD, theta


# ─────────────────────────────────────────────────────────────────────────────
# calcal_adj_TD.m
# ─────────────────────────────────────────────────────────────────────────────

def adjust_levels(
    obsdata: np.ndarray,
    distvar: np.ndarray,
    noj: int,
    A: np.ndarray,
    B: np.ndarray,
    alpha: float,
    beta: float,
    kappa: float,
    epsilon: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Rescale productivities (geomean=1) and amenities (to match city population).
    Equivalent to calcal_adj_TD.m.

    This step is needed only when A and B will be used as inputs to equilibrium
    solvers (counterfactual analysis). It adjusts levels so that the model
    reproduces the observed total city population.

    Intuition (from supplement p.18):
      H = "adjustment"^ε · Φ  →  adjustment = (H/Φ)^{1/ε}
    where Φ = Σ_ij φ_ij is model population before adjustment.

    Parameters
    ----------
    obsdata : (noj, 4)
    distvar : (noj, noj)
    noj     : int
    A       : (noj,) initial productivities from solve_omega_C
    B       : (noj,) initial amenities from recover_amenities
    alpha, beta, kappa, epsilon : model parameters

    Returns
    -------
    Aout    : (noj,) normalized productivities (geomean of positive values = 1)
    Bout    : (noj,) rescaled amenities (model population = data population)
    wageout : (noj,) updated adjusted wages consistent with normalized A
    """
    QT  = obsdata[:, 0]
    HMT = obsdata[:, 1]
    HRT = obsdata[:, 2]

    Iwpl = HMT != 0
    Irsd = HRT != 0
    wpl_idx = np.where(Iwpl)[0]
    rsd_idx = np.where(Irsd)[0]
    nwpl = int(Iwpl.sum())
    nrsd = int(Irsd.sum())

    EHMT = HMT[Iwpl]
    EHRT = HRT[Irsd]
    HH   = EHMT.sum()

    EA = A[Iwpl]
    EA = EA / gmean(EA)     # normalize A to geomean=1

    EB = B[Irsd]

    # Updated wages consistent with normalized A  (Eq. 12 with A=EA)
    Ewage = (
        ((1 - alpha) / QT[Iwpl]) ** ((1 - alpha) / alpha)
        * alpha
        * (EA ** (1.0 / alpha))
    )

    # Model population Φ = Σ_{ij} φ_ij
    Edistvar = distvar[np.ix_(rsd_idx, wpl_idx)]
    d_ij_eps = np.exp(-epsilon * kappa * Edistvar)
    EQQ = QT[Irsd][:, np.newaxis] ** (-(1 - beta) * epsilon)
    EBB = EB[:, np.newaxis] ** epsilon
    EWW = Ewage[np.newaxis, :] ** epsilon
    Ephi_ij = d_ij_eps * EQQ * EBB * EWW
    Ephi = Ephi_ij.sum()   # scalar Φ

    # Rescale amenities: B_adj = (H/Φ)^{1/ε} · B
    EB = ((HH / Ephi) ** (1.0 / epsilon)) * EB

    Aout    = np.zeros(noj); Aout[Iwpl]    = EA
    Bout    = np.zeros(noj); Bout[Irsd]    = EB
    wageout = np.zeros(noj); wageout[Iwpl] = Ewage

    print(">>>> Productivities and amenities updated <<<<")
    return Aout, Bout, wageout
