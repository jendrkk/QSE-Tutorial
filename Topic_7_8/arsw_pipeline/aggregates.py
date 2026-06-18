"""
aggregates.py — aggregate summaries of an ARSW equilibrium (Topic 8 / Task 1 d–f).

All functions take the output of `solver.solve_equilibrium` (or pieces thereof)
and return scalars / small dicts. No I/O, no plotting, no side effects.

Conventions
-----------
* `endog`: the (N, 9) array with columns
      0 wage    1 vv     2 theta  3 Y     4 Q
      5 q       6 HM     7 HR     8 Crent
* `ucprob_block`: the (nrsd, nwpl) probability block from the solver.
  It already satisfies `ucprob_block.sum() == 1` (closed-city, ARSW Eq. 4).
* `tt`: full (N, N) travel-time matrix in canonical block order.
* `ri, wi`: the canonical positions of `Irsd` and `Iwpl` blocks.
"""
from __future__ import annotations
from typing import Optional
import numpy as np


# ---------------------------------------------------------------------------
# Task 1(d) — aggregate model outcomes
# ---------------------------------------------------------------------------

def compute_aggregates(endog, Ubar, HH):
    """Return per-equilibrium aggregate model outcomes.

    Returns
    -------
    out : dict with keys
        GDP             Σ_j Y_j                (total output)
        wage_bill       Σ_j w_j · HM_j         (= α · GDP at the FOC)
        mean_wage_emp   Σ_j w_j·HM_j / Σ_j HM_j  (employment-weighted)
        total_HM        Σ_j HM_j               (≈ HH; closed city invariant)
        total_HR        Σ_i HR_i               (≈ HH)
        sum_vv          Σ_i vv_i               (total residence income)
        Ubar            scalar, expected utility
        HH              scalar, total population (input)
    """
    endog = np.asarray(endog, float)
    wage = endog[:, 0]; Y = endog[:, 3]
    HM = endog[:, 6]; HR = endog[:, 7]; vv = endog[:, 1]
    HM_sum = float(HM.sum())
    wage_bill = float((wage * HM).sum())
    return {
        "GDP": float(Y.sum()),
        "wage_bill": wage_bill,
        "mean_wage_emp": wage_bill / HM_sum if HM_sum > 0 else float("nan"),
        "total_HM": HM_sum,
        "total_HR": float(HR.sum()),
        "sum_vv": float(vv.sum()),
        "Ubar": float(Ubar),
        "HH": float(HH),
    }


# ---------------------------------------------------------------------------
# Task 1(e) — population-weighted average travel time
# ---------------------------------------------------------------------------

def compute_weighted_travel_time(ucprob_block, tt, ri, wi):
    """Weighted average travel time on a closed-city equilibrium.

    WATT  =  Σ_ij  π_ij · τ_ij    (since Σ π_ij = 1 over all i ∈ Irsd, j ∈ Iwpl)

    Equivalently  WATT  =  (1 / HH) · Σ_ij  (commuters_ij) · τ_ij.

    Parameters
    ----------
    ucprob_block : (nrsd, nwpl) ndarray
        Unconditional commuting probability block from `solver.solve_equilibrium`.
    tt : (N, N) ndarray
        Travel times in canonical block order.
    ri, wi : 1-d int ndarrays
        Canonical positions of Irsd and Iwpl blocks.

    Returns
    -------
    watt : float
        Population-weighted mean commute time (minutes).
    """
    tt_block = tt[np.ix_(ri, wi)]
    if tt_block.shape != ucprob_block.shape:
        raise ValueError(f"shape mismatch: ucprob {ucprob_block.shape} vs tt[ri,wi] {tt_block.shape}")
    # in-place product to save ~1 GB allocation:
    # we *cannot* mutate ucprob_block (caller may reuse it), so compute via einsum
    # which streams the multiply-add without materialising the product matrix.
    return float(np.einsum("ij,ij->", ucprob_block, tt_block, optimize=True))


# ---------------------------------------------------------------------------
# Task 1(f) — transport planner valuation
# ---------------------------------------------------------------------------

def compute_transport_planner_savings(tt_base, tt_u5, ucprob_block_base,
                                       ri, wi, HH, *,
                                       wage_base: Optional[np.ndarray] = None,
                                       vot_scenarios=(0.50, 0.75, 1.00),
                                       hours_per_year: float = 220.0 * 2):
    """Standard transport-planner valuation under the rule-of-half / fixed-flow
    approximation: total person-minutes of commute time saved per day, with
    baseline commuting flows held constant. The valuation IGNORES spatial
    reallocation (workers and firms staying put), and so under-states the
    welfare gains computed by the model when amenities, productivity differences,
    or congestion are present.

    Time savings (per day, person-minutes):

        ΔT = Σ_ij  commuters_ij^base · (τ_ij^base − τ_ij^U5)
           = HH · Σ_ij  π_ij^base · (τ_ij^base − τ_ij^U5)

    Monetisation (per year, €): we apply three Value-of-Time (VoT) scenarios as a
    fraction of the baseline employment-weighted mean wage, treated as an hourly
    wage (the ARSW model carries no time unit). The output reports time savings
    and monetised welfare under each scenario; the orchestrator compares this to
    model Δ GDP and Δ Ū.

    Parameters
    ----------
    tt_base, tt_u5 : (N, N) ndarrays
        Travel-time matrices (minutes). Same row/col ordering.
    ucprob_block_base : (nrsd, nwpl) ndarray
        Baseline unconditional commuting probabilities (sums to 1).
    ri, wi : 1-d int ndarrays
    HH : float
        Total population.
    wage_base : (N,) ndarray or None
        Baseline workplace wages (only `wage[wi]` is used). If None, the
        monetised columns are NaN.
    vot_scenarios : tuple of floats
        VoT as a fraction of the mean wage. Default 50, 75, 100 percent.
    hours_per_year : float
        Working hours per year used to convert minute-savings to annual flows
        (default 220 working days × 2 commutes ≈ daily round-trip × workdays;
        we treat ΔT already as a per-day quantity since flows are daily commutes).

    Returns
    -------
    out : dict
        total_savings_minutes_per_day   : float
        total_savings_hours_per_year    : float (≈ minutes/60 * 220 days)
        mean_savings_minutes_per_worker : float (= ΔT / HH)
        mean_wage_hourly_base           : float or NaN
        monetised_value_eur_per_year    : dict[vot_frac → €] (∅ if wage_base None)
    """
    tt_base = np.asarray(tt_base, float); tt_u5 = np.asarray(tt_u5, float)
    if tt_base.shape != tt_u5.shape:
        raise ValueError(f"TTM shapes differ: {tt_base.shape} vs {tt_u5.shape}")
    dtau_block = tt_base[np.ix_(ri, wi)] - tt_u5[np.ix_(ri, wi)]
    # ΔT in person-minutes per day:
    delta_t_min_per_day = float(HH * np.einsum("ij,ij->", ucprob_block_base, dtau_block,
                                                optimize=True))
    mean_min_per_worker = delta_t_min_per_day / HH if HH > 0 else float("nan")
    delta_t_hours_per_year = delta_t_min_per_day / 60.0 * 220.0  # 220 work days

    monetised = {}
    mean_wage_hr = float("nan")
    if wage_base is not None:
        wage_base = np.asarray(wage_base, float)
        wb = wage_base[wi]
        emp_weights = ucprob_block_base.sum(axis=0)         # (nwpl,)
        if emp_weights.sum() > 0:
            mean_wage_hr = float(np.average(wb, weights=emp_weights))
            for frac in vot_scenarios:
                vot_per_min = (mean_wage_hr * frac) / 60.0
                monetised[f"vot_{int(round(100 * frac)):d}pct"] = (
                    delta_t_min_per_day * vot_per_min * 220.0)   # €/year
    return {
        "total_savings_minutes_per_day": delta_t_min_per_day,
        "total_savings_hours_per_year": delta_t_hours_per_year,
        "mean_savings_minutes_per_worker": mean_min_per_worker,
        "mean_wage_hourly_base": mean_wage_hr,
        "monetised_value_eur_per_year": monetised,
    }
