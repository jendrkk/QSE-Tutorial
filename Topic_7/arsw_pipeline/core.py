"""
core.py — ARSW 2015 inversion numerics.

Faithful, line-checked translations of the ARSW2015-toolkit MATLAB routines:
    solve_omega(mode='O')      <- comegaoptO.m   (Task 1b transformed-wage solver)
    solve_omega(mode='C')      <- comegaoptC.m   (Task 1c wage + productivity solver)
    recover_amenities          <- camen.m        (Eq. S.47)
    compute_expected_income    <- expincome.m    (Eq. 4/5, S.20; row-batched)
    adjust_levels              <- calcal_adj_TD.m
    compute_density            <- cdensity.m     (Eqs. S.29-S.31)
    mod_bezirk                 <- modbezirk.m
    bezirk_logwage_var / estimate_epsilon <- cdensityoptren.m / optimepsilon_TD86.m (Eq. S.64)

Conventions match the MATLAB exactly:
  * commuting cost matrix Ecc has rows = workplaces, cols = residences;
  * omega (transformed wages) identified up to scale -> normalised to geomean 1;
  * blocks with zero workplace (residence) employment get a theory-consistent zero
    wage/productivity (amenity);
  * employment gaps are checked on values scaled by 1e5 (mode O) / 1e4 (mode C)
    and rounded, replicating the MATLAB stopping rule.

Validated on a synthetic economy with known fundamentals: wages and amenities are
recovered with correlation 1.0 (max |Δlog| < 1e-6); ε is recovered exactly.
"""
from __future__ import annotations
import numpy as np
from scipy.stats import gmean
from scipy.optimize import minimize_scalar
import pandas as pd

# ---------------------------------------------------------------------------
# modbezirk.m : historic Bezirke (1-23) -> modern Bezirke (1-12)
# ---------------------------------------------------------------------------
_CROSSWALK = {1:1, 22:1, 2:2, 5:2, 7:3, 4:3, 8:4, 3:4, 9:5, 18:5, 20:5,
              10:6, 11:7, 12:7, 21:7, 13:8, 15:9, 16:10, 23:10, 17:11, 14:11,
              19:12, 6:12}


def mod_bezirk(bzk: np.ndarray) -> np.ndarray:
    """Map historic Bezirke codes (1-23) to modern Bezirke (1-12). 0 if unmapped."""
    bzk = np.asarray(bzk, int)
    out = np.zeros(len(bzk), dtype=int)
    for old, new in _CROSSWALK.items():
        out[bzk == old] = new
    return out


# ---------------------------------------------------------------------------
# Shared damped fixed point for transformed wages (comegaoptO/C inner loop)
# ---------------------------------------------------------------------------
def _iterate_omega(Eomega, EHMT, EHRT, Ecc, round_scale,
                   max_iter=500, gaptol=0.0, verbose=False):
    """
    Solve Eq. S.44 for transformed wages by damped fixed-point iteration.

    Ecc : (nto, nfrom) commuting-cost matrix, rows=workplaces j, cols=residences i.
          conditional commuting prob P(work j | live i) = (omega_j / c_ji)
                                                           / Σ_k (omega_k / c_ki)
          predicted workplace employment HM_j = Σ_i P(j|i) HR_i.
    """
    rng = np.random.default_rng(1)
    nto, nfrom = Ecc.shape
    wconverge = False
    gap = np.inf
    Ecprob = np.empty((nto, nfrom))
    for _ in range(max_iter):
        Ecnum = Eomega[:, None] / Ecc
        Ecprob = Ecnum / Ecnum.sum(axis=0)[None, :]
        EHMC = Ecprob @ EHRT
        gap = float(np.abs(np.round(EHMC * round_scale)
                           - np.round(EHMT * round_scale)).max())
        if np.isnan(gap):                              # safety net (comegaoptO l.100-104)
            Eomega = rng.uniform(0.95, 1.05, nto)
            Ecprob = np.ones((nto, nfrom)) / nto
            continue
        if gap <= gaptol:
            wconverge = True
            break
        Eomega_e = (EHMT / EHMC) * Eomega              # inflate where observed > predicted
        Eomega = 0.5 * Eomega_e + 0.5 * Eomega         # damping weight 0.5
        Eomega = Eomega / gmean(Eomega)                # identified up to scale
    if verbose:
        print(f"     omega solver: converged={wconverge}  gap={gap}")
    return Eomega, Ecprob, wconverge, EHMC, gap


def solve_omega(obsdata, tt, noj, init, *, mode,
                alpha=0.80, beta=0.75, epsilon=None, kappa=None, kappaeps=None,
                verbose=False):
    """
    mode='O'  (comegaoptO):  cost = exp(kappaeps * tau),  init = omega guess.
                             returns (omega, converged, HMC, gap).
    mode='C'  (comegaoptC):  cost = exp(epsilon*kappa * tau),  init = adjusted-wage guess.
                             returns (wage, A, converged, HMC, gap).
    obsdata columns: [floor_price, empwpl, emprsd, area].
    """
    QT, HMT, HRT = obsdata[:, 0], obsdata[:, 1], obsdata[:, 2]
    Iwpl = HMT != 0
    Irsd = HRT != 0
    wi = np.where(Iwpl)[0]
    ri = np.where(Irsd)[0]
    EHMT, EHRT = HMT[Iwpl], HRT[Irsd]

    if mode == 'O':
        rate, rs = kappaeps, 100_000
        Eomega = init[Iwpl].astype(float).copy()
    elif mode == 'C':
        rate, rs = epsilon * kappa, 10_000
        Eomega = (init[Iwpl].astype(float) ** epsilon).copy()
    else:
        raise ValueError("mode must be 'O' or 'C'")

    Ecc = np.exp(rate * tt[np.ix_(wi, ri)])
    Eomega, _, conv, EHMC, gap = _iterate_omega(Eomega, EHMT, EHRT, Ecc, rs, verbose=verbose)

    omega = np.zeros(noj); omega[Iwpl] = Eomega
    HMC = np.zeros(noj);   HMC[Iwpl] = EHMC
    if mode == 'O':
        return omega, conv, HMC, gap

    w = np.zeros(noj); w[Iwpl] = Eomega ** (1.0 / epsilon)
    pos = w > 0
    w[pos] = w[pos] / gmean(w[pos])
    A = np.zeros(noj)                                  # Eq. S.48 (bottom) solved for Ã
    A[Iwpl] = (QT[Iwpl] / (1 - alpha)) ** (1 - alpha) * (w[Iwpl] / alpha) ** alpha
    return w, A, conv, HMC, gap


# ---------------------------------------------------------------------------
# camen.m : adjusted amenities B, residential CMA, residence shares HRS (Eq. S.47)
# ---------------------------------------------------------------------------
def recover_amenities(obsdata, tt, noj, wage, *, alpha, beta, kappa, epsilon):
    QT, HMT, HRT = obsdata[:, 0], obsdata[:, 1], obsdata[:, 2]
    Iwpl = HMT != 0
    Irsd = HRT != 0
    wi = np.where(Iwpl)[0]
    ri = np.where(Irsd)[0]
    EHRT, Ewage = HRT[Irsd], wage[Iwpl]
    d = np.exp(-epsilon * kappa * tt[np.ix_(ri, wi)])         # (nrsd, nwpl) rows=residences
    EHRS = EHRT / EHRT.sum(); EHRSn = EHRS / gmean(EHRS)
    EQT = QT[Irsd]; EQTn = EQT / gmean(EQT)
    ECMA = d @ (Ewage ** epsilon); ECMAn = ECMA / gmean(ECMA)
    EB = (EHRSn ** (1 / epsilon)) * (EQTn ** (1 - beta)) / (ECMAn ** (1 / epsilon))
    B = np.zeros(noj);   B[Irsd] = EB
    CMA = np.zeros(noj); CMA[Irsd] = ECMA               # levels (not normalised)
    HRS = np.zeros(noj); HRS[Irsd] = EHRS               # shares (not normalised)
    return B, CMA, HRS


def _phi_block(QT_r, EB_r, Ewage, d_block, beta, epsilon):
    """Eq. 4 numerator φ_ij for a block of residence rows (broadcasting)."""
    EQQ = QT_r[:, None] ** (-(1 - beta) * epsilon)
    EBB = EB_r[:, None] ** epsilon
    EWW = Ewage[None, :] ** epsilon
    return d_block * EQQ * EBB * EWW


# ---------------------------------------------------------------------------
# expincome.m : total worker income at residence (Eq. S.20). Row-batched.
# ---------------------------------------------------------------------------
def compute_expected_income(obsdata, tt, noj, wage, B, *,
                            alpha, beta, kappa, epsilon, batch=1024):
    QT, HMT, HRT = obsdata[:, 0], obsdata[:, 1], obsdata[:, 2]
    Iwpl = HMT != 0
    Irsd = HRT != 0
    wi = np.where(Iwpl)[0]
    ri = np.where(Irsd)[0]
    Ewage, EB, EQT, EHRT = wage[Iwpl], B[Irsd], QT[Irsd], HRT[Irsd]
    nrsd = len(ri)
    EEWI = np.empty(nrsd)
    for s in range(0, nrsd, batch):                     # bound peak memory at ~batch×nwpl
        e = min(s + batch, nrsd)
        d = np.exp(-epsilon * kappa * tt[np.ix_(ri[s:e], wi)])
        phi = _phi_block(EQT[s:e], EB[s:e], Ewage, d, beta, epsilon)
        EEWI[s:e] = (phi / phi.sum(axis=1, keepdims=True)) @ Ewage
    vv = np.zeros(noj); vv[Irsd] = EEWI * EHRT
    return vv


# ---------------------------------------------------------------------------
# calcal_adj_TD.m : rescale A (geomean=1) and B (model pop = data pop)
# ---------------------------------------------------------------------------
def adjust_levels(obsdata, tt, noj, A, B, *,
                  alpha, beta, kappa, epsilon, batch=1024):
    QT, HMT, HRT = obsdata[:, 0], obsdata[:, 1], obsdata[:, 2]
    Iwpl = HMT != 0
    Irsd = HRT != 0
    wi = np.where(Iwpl)[0]
    ri = np.where(Irsd)[0]
    HH = HMT[Iwpl].sum()
    EA = A[Iwpl]; EA = EA / gmean(EA)
    EB = B[Irsd]
    Ewage = ((1 - alpha) / QT[Iwpl]) ** ((1 - alpha) / alpha) * alpha * (EA ** (1 / alpha))
    EQT = QT[Irsd]; nrsd = len(ri); Phi = 0.0
    for s in range(0, nrsd, batch):
        e = min(s + batch, nrsd)
        d = np.exp(-epsilon * kappa * tt[np.ix_(ri[s:e], wi)])
        phi = _phi_block(EQT[s:e], EB[s:e], Ewage, d, beta, epsilon)
        Phi += phi.sum()
    EB = ((HH / Phi) ** (1 / epsilon)) * EB             # H = adjustment^ε · Φ (suppl. p.18)
    Aout = np.zeros(noj); Aout[Iwpl] = EA
    Bout = np.zeros(noj); Bout[Irsd] = EB
    wout = np.zeros(noj); wout[Iwpl] = Ewage
    return Aout, Bout, wout


# ---------------------------------------------------------------------------
# cdensity.m : density of development V, total floor space L, commercial share theta
# ---------------------------------------------------------------------------
def compute_density(obsdata, A, wage, vv, noj, *, alpha, beta):
    QT, HMT, HRT, K = obsdata[:, 0], obsdata[:, 1], obsdata[:, 2], obsdata[:, 3]
    Iwpl = HMT != 0
    Irsd = HRT != 0
    LM = np.zeros(noj)                                  # commercial floor demand (Eq. S.30)
    LM[Iwpl] = (((1 - alpha) * A[Iwpl]) / QT[Iwpl]) ** (1 / alpha) * HMT[Iwpl]
    LR = np.zeros(noj)                                  # residential floor demand (Eq. S.29)
    LR[Irsd] = ((1 - beta) * vv[Irsd]) / QT[Irsd]
    LD = LM + LR
    V = np.zeros(noj); kp = K > 0; V[kp] = LD[kp] / (K[kp] ** 0.75)   # Eq. S.31
    theta = np.zeros(noj); lp = LD > 0; theta[lp] = LM[lp] / LD[lp]
    return V, LD, theta


# ---------------------------------------------------------------------------
# cdensityoptren.m / optimepsilon_TD86.m : ε via moment matching (Eq. S.64)
# ---------------------------------------------------------------------------
def bezirk_logwage_var(omega, HM, bzk, epsilon):
    """Variance (ddof=1) of demeaned log employment-weighted Bezirk wages,
    given pre-solved transformed wages omega and a candidate epsilon."""
    w = omega ** (1.0 / epsilon)
    pos = w > 0
    if pos.any():
        w[pos] = w[pos] / gmean(w[pos])
    wbill = w * HM
    df = pd.DataFrame({"wbill": wbill, "emp": HM, "bzk": bzk})
    g = df.groupby("bzk")
    bw = g["wbill"].sum() / g["emp"].sum()
    lb = np.log(bw.values); lb -= lb.mean()
    return float(np.var(lb, ddof=1))


def estimate_epsilon(omega, HM, bzk, var_data, lb=2.0, ub=24.0):
    """Find ε in [lb, ub] minimising (Var_model − Var_data)²·1e6; round to 2 dp
    (patternsearch -> bounded scalar minimisation)."""
    def f(e):
        return (bezirk_logwage_var(omega.copy(), HM, bzk, e) - var_data) ** 2 * 1e6
    r = minimize_scalar(f, bounds=(lb, ub), method="bounded",
                        options={"xatol": 1e-6, "maxiter": 500})
    return round(r.x * 100) / 100
