"""
solver.py — Topic 8 / Task 1(b): closed-city forward equilibrium solver.

Faithful Python port of ARSW2015-toolkit/matlab/section6/exogcftual/smodexog.m.
Given primitives (A, B, V, K, LM, LR, LD) inverted by Topic 7's calibration step
and a travel-time matrix τ, plus elasticities (ε, εκ) from Task 1a/1b, solves the
closed-city ARSW equilibrium for:
    workplace wages         w_j
    residential floor price Q_i
    commercial floor price  q_j
    commercial floor share  θ_j
and returns the implied output Y_j, employment HM_j / HR_i, total worker income
vv_i, combined floor price Crent, expected utility Ū = Γ((ε−1)/ε) · Φ^{1/ε}, and
unconditional commuting probabilities π_ij (returned as the (nrsd, nwpl) block).

ALGORITHM
---------
Damped fixed-point on (w, q, Q, θ). Each iteration:

  1. φ_ij ∝ d_ij · Q_i^{−(1−β)ε} · B_i^ε · w_j^ε        (d_ij = exp(−εκ τ_ij))
     → predict HM_j, HR_i, Φ
  2. Y_j = A_j · HM_j^α · (θ_j · LD_j)^{1−α}
  3. w_j^pred = α Y_j / HM_j                              (FOC on labour)
  4. vv_i = (Σ_j (φ_ij/φ_i·) · w_j_current) · HR_i        (total worker income)
  5. q_e, Q_e from FOCs on land (separate cases by specialisation)
  6. θ_e (mixed-use only) from FOC on commercial land
  7. converge iff all four (w, q, Q, θ) match their predictions when ROUNDED to
     two decimal places (i.e. multiplied by 100, rounded to the nearest integer)
  8. damped update: x_i ← 0.5·x_e + 0.5·x_i

FAITHFULNESS NOTES (verified line-by-line against smodexog.m)
------------------------------------------------------------
* Iwpl, Irsd are derived from A != 0, B != 0 (NOT from HMT, HRT). After
  `adjust_levels`, the two are equivalent, but we keep the A/B convention.
* `EQQ` in the commuting-probability numerator uses **QT** (=fund[:,4]), held
  FROZEN. The iterates Q_i, Q_e evolve separately via the land-market FOC and
  are only used by the convergence test. This matches the toolkit. It means
  the solver is fixed-point in (w, q, Q, θ) only via the FOCs, not via
  re-injecting Q_i back into commuting decisions.
* `vv` is updated in step 4 of each iteration and then used in step 5 (FOCs
  on Q, q) — order matters.
* `Crent[Iwpl] = q_i[Iwpl]` then `Crent[Irsd] = Q_i[Irsd]` — for mixed-use
  blocks, the residential overwrite means Crent ends up holding Q. We match.
* The damping weight is 0.5 in both MATLAB branches.
* NaN restart: when Φ is non-finite (extreme starting guesses), Ewage_i is
  replaced with U(0.95, 1.05) and the iteration continues. Matches MATLAB.

MEMORY
------
The dominant array is `d_ij_eps = exp(−εκ τ[Irsd, Iwpl])` ≈ nrsd × nwpl × 8 B
(~1.2 GB at Berlin scale). A second buffer of the same size is pre-allocated
for the per-iteration Ephi_ij and reused in place via `np.multiply(..., out=)`,
so the working set stays at ~2 × kernel size. Returning `ucprob_block` aliases
the second buffer (~1.2 GB) — callers that have memory pressure should `del`
the buffer once they have computed WATT, transport-planner valuation, etc.

REFERENCES
----------
* MATLAB: ARSW2015-toolkit/matlab/section6/exogcftual/smodexog.m
* Paper : Ahlfeldt, Redding, Sturm, Wolf (2015, Econometrica), Eqs. 4, 9, 10,
          12, and supplement S.20, S.29-S.31.
"""
from __future__ import annotations
import numpy as np
from scipy.special import gamma as _gamma_fn


# ---------------------------------------------------------------------------
# fund-matrix assembly
# ---------------------------------------------------------------------------

def build_fund(obsvar, cal):
    """Assemble the 12-column `fund` matrix `smodexog.m` expects.

    Column layout (0-indexed; MATLAB column = Python col + 1):

        0  A      productivities                      cal["A"]
        1  B      amenities                           cal["B"]
        2  V      density of development              cal["V"]
        3  K      block area                          obsvar[:, 3]
        4  QT     floor space prices (frozen in π)    obsvar[:, 0]
        5  HMT    workplace employment (adjusted)     obsvar[:, 1]
        6  HRT    residence employment                obsvar[:, 2]
        7  LM     commercial floor space              cal["L"] * cal["theta"]
        8  LR     residential floor space             cal["L"] * (1 − cal["theta"])
        9  LD     total floor space                   cal["L"]
       10  wage   wage initial guess                  cal["wage"]
       11  vv     total worker income initial guess   cal["vv"]

    Parameters
    ----------
    obsvar : (N, 4) ndarray
        Block data array from `dataio.load_block_data` (columns Q, HMT, HRT, K).
    cal : dict
        Output of `calibration.calibrate` — must contain A, B, V, L, theta, wage, vv.

    Returns
    -------
    fund : (N, 12) ndarray
    """
    obsvar = np.asarray(obsvar, float)
    N = obsvar.shape[0]
    if obsvar.shape[1] != 4:
        raise ValueError(f"obsvar must have 4 columns; got {obsvar.shape}")
    needed = ("A", "B", "V", "L", "theta", "wage", "vv")
    missing = [k for k in needed if k not in cal]
    if missing:
        raise ValueError(f"cal dict missing keys: {missing}")
    for k in needed:
        if len(cal[k]) != N:
            raise ValueError(f"cal['{k}'] has length {len(cal[k])}, expected {N}")
    fund = np.zeros((N, 12), dtype=float)
    fund[:, 0]  = cal["A"]
    fund[:, 1]  = cal["B"]
    fund[:, 2]  = cal["V"]
    fund[:, 3]  = obsvar[:, 3]
    fund[:, 4]  = obsvar[:, 0]
    fund[:, 5]  = obsvar[:, 1]
    fund[:, 6]  = obsvar[:, 2]
    fund[:, 7]  = cal["L"] * cal["theta"]
    fund[:, 8]  = cal["L"] * (1.0 - cal["theta"])
    fund[:, 9]  = cal["L"]
    fund[:, 10] = cal["wage"]
    fund[:, 11] = cal["vv"]
    return fund


# ---------------------------------------------------------------------------
# Equilibrium solver
# ---------------------------------------------------------------------------

def solve_equilibrium(fund, tt, *, epsilon, kappaeps,
                      alpha=0.80, beta=0.75,
                      max_iter=1000, weight=0.5, verbose=True,
                      log_every=50, seed=1):
    """Solve the closed-city ARSW equilibrium given primitives and a TTM.

    Parameters
    ----------
    fund : (N, 12) ndarray
        See `build_fund` for the column layout.
    tt : (N, N) ndarray (float64)
        Bilateral travel times in canonical block order. tt[i, j] is the time
        from residence i to workplace j. Must contain NO NaN; diagonal expected
        to be 0 (or near zero); units are minutes.
    epsilon : float
        Frechet shape (e.g. 6.83 from Task 1b).
    kappaeps : float
        Reduced-form ε·κ (e.g. 0.07 from Task 1a).
    alpha : float (default 0.80)
        Labour share in production.
    beta : float (default 0.75)
        Consumption share on the traded good (1−β is the housing share).
    max_iter : int (default 1000)
        Outer loop iteration cap (matches smodexog.m).
    weight : float (default 0.5)
        Damping weight on predicted values (matches smodexog.m).
    verbose : bool
    log_every : int
        Print diagnostics every N iterations when verbose=True.
    seed : int
        RNG seed used only for the NaN-restart fallback.

    Returns
    -------
    result : dict with keys

      endog       : (N, 9) ndarray  -- columns [wage, vv, theta, Y, Q, q, HM, HR, Crent]
      ucprob_block: (nrsd, nwpl) ndarray  -- unconditional commuting probabilities φ_ij/Φ
                                            (aliases the iteration scratch buffer)
      ri          : (nrsd,) int ndarray   -- canonical positions of Irsd blocks
      wi          : (nwpl,) int ndarray   -- canonical positions of Iwpl blocks
      Iwpl, Irsd  : (N,) bool ndarrays
      Ubar        : float  -- Γ((ε−1)/ε) · Φ^{1/ε}
      Phi         : float
      HH          : float  -- total population (= Σ HMT)
      converged   : bool
      n_iter      : int
      cpath       : (n_iter, 7) ndarray  -- per-iteration log gaps
                    columns [maxLD_wage, maxLD_q, maxLD_Q, maxLD_θ, maxLD, MLSE, iter]
      diagnostics : dict  -- final maxLD values per target, plus Phi/Ubar.

    Notes
    -----
    The returned `ucprob_block` is the **block** restricted to (Irsd × Iwpl) and
    aliases the iteration scratch buffer (~1 GB). It already satisfies
    `ucprob_block.sum() == 1`. To materialise an (N, N) dense matrix:
        UC = np.zeros((N, N)); UC[np.ix_(ri, wi)] = ucprob_block
    but this is rarely needed (WATT and transport-planner work directly on the
    block + the corresponding tt[ri, wi] slice).
    """
    rng = np.random.default_rng(seed)
    fund = np.asarray(fund, float)
    tt = np.asarray(tt, float)
    N = fund.shape[0]
    if fund.shape != (N, 12):
        raise ValueError(f"fund must be (N, 12); got {fund.shape}")
    if tt.shape != (N, N):
        raise ValueError(f"tt must be ({N},{N}); got {tt.shape}")
    if np.isnan(tt).any():
        raise ValueError("tt contains NaN; realign / fill first.")

    # ---- Unpack primitives ----
    A   = fund[:, 0]
    B   = fund[:, 1]
    K   = fund[:, 3]
    QT  = fund[:, 4]
    HMT = fund[:, 5]
    HRT = fund[:, 6]
    LM_init = fund[:, 7]
    LR_init = fund[:, 8]
    LD  = fund[:, 9]

    HH = float(HMT.sum())
    if HH <= 0:
        raise ValueError("HH (total workplace employment) must be positive.")

    # ---- Specialisation masks (smodexog uses A != 0, B != 0) ----
    Iwpl = A != 0
    Irsd = B != 0
    IcsA = Iwpl & ~Irsd          # commercial-only
    IcsB = Irsd & ~Iwpl          # residential-only
    Iis  = Iwpl & Irsd           # mixed-use
    nwpl = int(Iwpl.sum()); nrsd = int(Irsd.sum())
    wi = np.where(Iwpl)[0]
    ri = np.where(Irsd)[0]

    EA = A[Iwpl]                 # (nwpl,)
    EB = B[Irsd]                 # (nrsd,)

    if verbose:
        print(f">>>> Solving equilibrium: N={N}  nwpl={nwpl}  nrsd={nrsd}")
        print(f"     ε={epsilon}  εκ={kappaeps}  α={alpha}  β={beta}  HH={HH:.0f}")
        print(f"     d_ij_eps memory ≈ {nrsd * nwpl * 8 / 1e9:.2f} GB")

    # ---- Spatial kernel: exp(−εκ τ) restricted to (Irsd × Iwpl) ----
    d_ij_eps = np.exp(-kappaeps * tt[np.ix_(ri, wi)])

    # ---- Iteration constants (commuting-prob numerator excluding wages) ----
    Q_pow  = QT[Irsd] ** (-(1.0 - beta) * epsilon)     # (nrsd,)
    B_pow  = EB ** epsilon                              # (nrsd,)
    QB_pow = Q_pow * B_pow                              # (nrsd,)

    # ---- Pre-allocate Ephi_ij (reused across iterations) ----
    Ephi_ij = np.empty_like(d_ij_eps)

    # ---- Initial guesses ----
    Ewage_i = fund[Iwpl, 10].astype(float).copy()
    vv = fund[:, 11].astype(float).copy()

    # Q_i, q_i start at the data floor prices; Q_e, q_e start equal (smodexog convention)
    Q_i = QT.copy()
    q_i = QT.copy()
    Q_e = QT.copy()
    q_e = QT.copy()

    # theta initialised from LM/LD on positive-LD blocks; 0 elsewhere
    theta_i = np.zeros(N)
    LD_pos = LD > 0
    theta_i[LD_pos] = LM_init[LD_pos] / LD[LD_pos]
    theta_e = theta_i.copy()

    # Y initialised from data (smodexog line: Y(Iwpl)=EA.*(HMT(Iwpl).^alpha).*(LM(Iwpl).^(1-alpha)))
    Y = np.zeros(N)
    Y[Iwpl] = EA * (HMT[Iwpl] ** alpha) * (LM_init[Iwpl] ** (1.0 - alpha))

    cpath = []
    converged = False
    Phi = np.nan; Ubar = np.nan
    gammaf = _gamma_fn((epsilon - 1.0) / epsilon)

    # placeholders we update in-loop; declared so they are bound after a NaN restart
    EHM = np.zeros(nwpl)
    EHR = np.zeros(nrsd)
    mLDw = mLDq = mLDQ = mLDt = mLD = np.inf
    MLSE = np.inf

    # ============================ ITERATION LOOP =============================
    for x in range(1, max_iter + 1):

        # ---- Step 1: commuting probabilities given current Ewage_i ----------
        W_pow = Ewage_i ** epsilon                          # (nwpl,)
        np.multiply(d_ij_eps, QB_pow[:, None], out=Ephi_ij) # Ephi_ij ← d · QB
        Ephi_ij *= W_pow[None, :]                            # Ephi_ij ← d · QB · W

        Phi = float(Ephi_ij.sum())
        if not np.isfinite(Phi) or Phi <= 0.0:
            # NaN-restart fallback (smodexog l.100-104)
            if verbose:
                print(f"  iter {x}: non-finite/zero Φ — random restart on Ewage_i")
            Ewage_i = rng.uniform(0.95, 1.05, nwpl)
            continue

        Phi_i = Ephi_ij.sum(axis=1)                         # (nrsd,)
        Phi_j = Ephi_ij.sum(axis=0)                         # (nwpl,)
        EHR = (Phi_i / Phi) * HH
        EHM = (Phi_j / Phi) * HH

        # ---- Step 2: predicted output Y_j -----------------------------------
        theta_wpl = theta_i[Iwpl]
        LD_wpl    = LD[Iwpl]
        Y_iwpl = EA * (EHM ** alpha) * ((theta_wpl * LD_wpl) ** (1.0 - alpha))
        Y[Iwpl] = Y_iwpl

        # ---- Step 3: predicted wage w_e_j = α Y_j / HM_j --------------------
        safe_EHM = EHM > 0
        Ewage_e = np.zeros(nwpl)
        Ewage_e[safe_EHM] = (alpha * Y_iwpl[safe_EHM]) / EHM[safe_EHM]

        # ---- Step 4: total worker income vv (uses current Ewage_i) ----------
        # EEWI = (Ephi_ij @ Ewage_i) / Phi_i   (algebraic identity avoiding Pcond)
        EEWI = (Ephi_ij @ Ewage_i) / Phi_i                  # (nrsd,)
        vv[Irsd] = EEWI * EHR

        # ---- Step 5: predicted floor prices ---------------------------------
        # Start q_e, Q_e at QT (smodexog convention) and override per regime.
        q_e = QT.copy()
        Q_e = QT.copy()

        # commercial-only blocks
        IcsA_pos = IcsA & (theta_i * LD > 0)
        if IcsA_pos.any():
            denom = theta_i[IcsA_pos] * LD[IcsA_pos]
            q_e[IcsA_pos] = (1.0 - alpha) * Y[IcsA_pos] / denom

        # residential-only blocks
        IcsB_pos = IcsB & (LD > 0) & ((1.0 - theta_i) > 0)
        if IcsB_pos.any():
            denom = (1.0 - theta_i[IcsB_pos]) * LD[IcsB_pos]
            Q_e[IcsB_pos] = (1.0 - beta) * vv[IcsB_pos] / denom

        # mixed-use blocks: q_e = Q_e = ((1−α) Y + (1−β) vv) / LD
        Iis_pos = Iis & (LD > 0)
        if Iis_pos.any():
            avg = ((1.0 - alpha) * Y[Iis_pos] + (1.0 - beta) * vv[Iis_pos]) / LD[Iis_pos]
            q_e[Iis_pos] = avg
            Q_e[Iis_pos] = avg

        # ---- Step 6: predicted θ for mixed-use blocks -----------------------
        theta_e = theta_i.copy()
        Iis_q_pos = Iis & (q_e > 0) & (LD > 0)
        if Iis_q_pos.any():
            theta_e[Iis_q_pos] = (1.0 - alpha) * Y[Iis_q_pos] / (q_e[Iis_q_pos] * LD[Iis_q_pos])

        # ---- Step 7: convergence check (rounded to 2 dp) --------------------
        wage_i_full = np.zeros(N); wage_i_full[Iwpl] = Ewage_i
        wage_e_full = np.zeros(N); wage_e_full[Iwpl] = Ewage_e
        wir = np.round(wage_i_full * 100.0); wer = np.round(wage_e_full * 100.0)
        qir = np.round(q_i * 100.0);         qer = np.round(q_e * 100.0)
        Qir = np.round(Q_i * 100.0);         Qer = np.round(Q_e * 100.0)
        tir = np.round(theta_i * 100.0);     ter = np.round(theta_e * 100.0)

        # log-gap diagnostics
        dW = np.abs(np.log(wir + 1.0) - np.log(wer + 1.0))
        dq = np.abs(np.log(qir + 1.0) - np.log(qer + 1.0))
        dQ = np.abs(np.log(Qir + 1.0) - np.log(Qer + 1.0))
        dt = np.abs(np.log(tir + 1.0) - np.log(ter + 1.0))
        mLDw = float(dW.max()); mLDq = float(dq.max())
        mLDQ = float(dQ.max()); mLDt = float(dt.max())
        mLD = max(mLDw, mLDq, mLDQ, mLDt)
        MLSE = float((1_000_000.0 / 4.0) *
                     ((dW ** 2).mean() + (dq ** 2).mean()
                      + (dQ ** 2).mean() + (dt ** 2).mean()))
        cpath.append((mLDw, mLDq, mLDQ, mLDt, mLD, MLSE, x))

        Ubar = float(gammaf * (Phi ** (1.0 / epsilon)))

        if verbose and (x == 1 or x % log_every == 0):
            print(f"  iter {x:4d}  maxLD={mLD:.4e}  MLSE={MLSE:.4e}  Φ={Phi:.4e}  Ū={Ubar:.4f}")

        if (np.array_equal(wir, wer)
                and np.array_equal(qir, qer)
                and np.array_equal(Qir, Qer)
                and np.array_equal(tir, ter)):
            converged = True
            if verbose:
                print(f">>>> Convergence achieved at iter {x}.  Ū={Ubar:.6f}  Φ={Phi:.6e}")
            break

        # ---- Step 8: damped update ------------------------------------------
        Ewage_i = weight * Ewage_e + (1.0 - weight) * Ewage_i
        q_i = weight * q_e + (1.0 - weight) * q_i
        Q_i = weight * Q_e + (1.0 - weight) * Q_i
        theta_i = weight * theta_e + (1.0 - weight) * theta_i

    if not converged and verbose:
        print(f">>>> NO convergence after {max_iter} iters; final maxLD={mLD:.4e}")

    # ============================ BUILD OUTPUTS ==============================
    wage_full = np.zeros(N); wage_full[Iwpl] = Ewage_i
    HM_full   = np.zeros(N); HM_full[Iwpl]   = EHM
    HR_full   = np.zeros(N); HR_full[Irsd]   = EHR

    Crent = np.zeros(N)
    Crent[Iwpl] = q_i[Iwpl]
    Crent[Irsd] = Q_i[Irsd]   # mixed-use: residential overwrites (matches MATLAB)

    endog = np.column_stack([wage_full, vv, theta_i, Y, Q_i, q_i,
                             HM_full, HR_full, Crent])
    # Ephi_ij now holds the LAST iteration's numerator; divide once to get
    # the unconditional commuting probability block (~1 GB; alias, not copy)
    ucprob_block = Ephi_ij / Phi

    diagnostics = {
        "maxLD_wage": mLDw, "maxLD_q": mLDq, "maxLD_Q": mLDQ, "maxLD_theta": mLDt,
        "maxLD": mLD, "MLSE": MLSE,
    }

    return {
        "endog": endog,
        "ucprob_block": ucprob_block,
        "ri": ri, "wi": wi,
        "Iwpl": Iwpl, "Irsd": Irsd,
        "Ubar": float(Ubar),
        "Phi": float(Phi),
        "HH": HH,
        "converged": bool(converged),
        "n_iter": int(x),
        "cpath": np.array(cpath, dtype=float),
        "diagnostics": diagnostics,
    }


# ---------------------------------------------------------------------------
# Self-consistency checker (optional helper for diagnostics)
# ---------------------------------------------------------------------------

def self_consistency_report(result, obsvar, cal, *, verbose=True):
    """Compute and (optionally) print how closely the solved baseline equilibrium
    reproduces the data it was calibrated on. Used as a sanity check before
    running counterfactuals.

    Parameters
    ----------
    result : dict
        Output of `solve_equilibrium`.
    obsvar : (N, 4) ndarray
        Original block data.
    cal : dict
        Calibration dictionary used to build `fund`.

    Returns
    -------
    report : dict with max-abs errors for HM, HR, Q (and wage if available),
             plus correlations of solved-vs-data on common-support blocks.
    """
    endog = result["endog"]
    HMT = obsvar[:, 1]; HRT = obsvar[:, 2]; QT = obsvar[:, 0]
    wage_cal = np.asarray(cal["wage"], float)
    HM_s = endog[:, 6]; HR_s = endog[:, 7]
    Q_s  = endog[:, 4]; wage_s = endog[:, 0]

    out = {
        "max_abs_HM_minus_HMT": float(np.abs(HM_s - HMT).max()),
        "max_abs_HR_minus_HRT": float(np.abs(HR_s - HRT).max()),
        "max_abs_Q_minus_QT_Irsd": float(np.abs(Q_s[result["Irsd"]] - QT[result["Irsd"]]).max()),
        "max_abs_wage_minus_calwage_Iwpl": float(
            np.abs(wage_s[result["Iwpl"]] - wage_cal[result["Iwpl"]]).max()),
        "corr_HM": float(np.corrcoef(HM_s[result["Iwpl"]], HMT[result["Iwpl"]])[0, 1]),
        "corr_HR": float(np.corrcoef(HR_s[result["Irsd"]], HRT[result["Irsd"]])[0, 1]),
    }
    if verbose:
        print(">>>> Baseline self-consistency:")
        for k, v in out.items():
            print(f"     {k}: {v:.6g}")
    return out
