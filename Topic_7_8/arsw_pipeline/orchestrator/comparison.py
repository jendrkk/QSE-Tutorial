"""
comparison.py — Topic-8 / Task-1 deltas for one (REF, ALT) pair.

Inputs:
  * a RefState (calibration + REF equilibrium)
  * the solved ALT equilibrium dict
  * both TTMs in canonical order (needed for CMA decomposition, WATT, planner)

Outputs (one dict):
  pcc         dict[str, (N,) float ndarray]   block-level Δ% vectors
                wage, HM, HR, Crent, Q, q, Y, theta, cma, cma_ttm_only
  agg_ref, agg_alt    dict   aggregates.compute_aggregates outputs
  delta_agg   dict          ΔGDP{€,%}, ΔŪ{abs,%}, Δwage_bill_pct, …
  watt        dict          ref_min, alt_min, delta_min, delta_pct
  planner     dict          aggregates.compute_transport_planner_savings output
  ttm_diff    dict          max/min/mean/median saved minutes; pair counts
"""
from __future__ import annotations
import numpy as np

import config
import accessibility
import aggregates

from .single_pipeline import RefState


def _pct_change(x_alt, x_ref, mask=None, eps=1e-12):
    """(x_alt / x_ref − 1) · 100 with NaN-safe masking. Returns NaN where
    |x_ref| < eps, where mask is False, or where x_ref is non-finite."""
    x_alt = np.asarray(x_alt, float)
    x_ref = np.asarray(x_ref, float)
    out = np.full_like(x_ref, np.nan)
    ok = np.isfinite(x_ref) & np.isfinite(x_alt) & (np.abs(x_ref) >= eps)
    if mask is not None:
        ok &= mask
    out[ok] = (x_alt[ok] / x_ref[ok] - 1.0) * 100.0
    return out


def compute_comparison(ref_state: RefState, res_alt: dict,
                       tt_ref: np.ndarray, tt_alt: np.ndarray, *,
                       epsilon: float | None = None,
                       kappaeps: float | None = None,
                       beta: float = config.BETA) -> dict:
    res_ref = ref_state.res
    if epsilon is None:
        epsilon = ref_state.epsilon
    if kappaeps is None:
        kappaeps = ref_state.kappaeps

    endog_ref = res_ref["endog"]
    endog_alt = res_alt["endog"]
    Iwpl = res_ref["Iwpl"]; Irsd = res_ref["Irsd"]
    Iemp = Iwpl | Irsd

    # ─── block-level Δ% ──────────────────────────────────────────────
    pcc = {
        "wage":  _pct_change(endog_alt[:, 0], endog_ref[:, 0], mask=Iwpl),
        "HM":    _pct_change(endog_alt[:, 6], endog_ref[:, 6], mask=Iwpl),
        "HR":    _pct_change(endog_alt[:, 7], endog_ref[:, 7], mask=Irsd),
        "Crent": _pct_change(endog_alt[:, 8], endog_ref[:, 8], mask=Iemp),
        "Q":     _pct_change(endog_alt[:, 4], endog_ref[:, 4], mask=Irsd),
        "q":     _pct_change(endog_alt[:, 5], endog_ref[:, 5], mask=Iwpl),
        "Y":     _pct_change(endog_alt[:, 3], endog_ref[:, 3], mask=Iwpl),
        "theta": _pct_change(endog_alt[:, 2], endog_ref[:, 2], mask=Iemp),
    }

    # ─── residence CMA decomposition ─────────────────────────────────
    wage_ref = endog_ref[:, 0]
    wage_alt = endog_alt[:, 0]
    cma_ref = accessibility.compute_cma_residence(
        tt_ref, wage_ref, epsilon=epsilon, kappaeps=kappaeps,
        Iwpl=Iwpl, Irsd=Irsd)
    cma_alt_full = accessibility.compute_cma_residence(
        tt_alt, wage_alt, epsilon=epsilon, kappaeps=kappaeps,
        Iwpl=Iwpl, Irsd=Irsd)
    cma_alt_ttm_only = accessibility.compute_cma_residence(
        tt_alt, wage_ref, epsilon=epsilon, kappaeps=kappaeps,
        Iwpl=Iwpl, Irsd=Irsd)
    pcc["cma"]          = _pct_change(cma_alt_full,     cma_ref, mask=Irsd)
    pcc["cma_ttm_only"] = _pct_change(cma_alt_ttm_only, cma_ref, mask=Irsd)

    # ─── aggregates ──────────────────────────────────────────────────
    agg_ref = aggregates.compute_aggregates(endog_ref, res_ref["Ubar"], res_ref["HH"])
    agg_alt = aggregates.compute_aggregates(endog_alt, res_alt["Ubar"], res_alt["HH"])
    delta_agg = {
        "delta_GDP_eur":       agg_alt["GDP"] - agg_ref["GDP"],
        "delta_GDP_pct":       (agg_alt["GDP"] / agg_ref["GDP"] - 1.0) * 100.0,
        "delta_Ubar":          agg_alt["Ubar"] - agg_ref["Ubar"],
        "delta_Ubar_pct":      (agg_alt["Ubar"] / agg_ref["Ubar"] - 1.0) * 100.0,
        "delta_wage_bill_pct": (agg_alt["wage_bill"] / agg_ref["wage_bill"] - 1.0) * 100.0,
        "delta_meanwage_pct":  (agg_alt["mean_wage_emp"] / agg_ref["mean_wage_emp"] - 1.0) * 100.0,
        "delta_sum_vv_pct":    (agg_alt["sum_vv"] / agg_ref["sum_vv"] - 1.0) * 100.0,
        "total_HM_check_HH_ref": agg_ref["total_HM"] - agg_ref["HH"],
        "total_HM_check_HH_alt": agg_alt["total_HM"] - agg_alt["HH"],
        "total_HR_check_HH_ref": agg_ref["total_HR"] - agg_ref["HH"],
        "total_HR_check_HH_alt": agg_alt["total_HR"] - agg_alt["HH"],
    }

    # ─── WATT ────────────────────────────────────────────────────────
    watt_ref = aggregates.compute_weighted_travel_time(
        res_ref["ucprob_block"], tt_ref, res_ref["ri"], res_ref["wi"])
    watt_alt = aggregates.compute_weighted_travel_time(
        res_alt["ucprob_block"], tt_alt, res_alt["ri"], res_alt["wi"])
    watt = {
        "ref_min":   float(watt_ref),
        "alt_min":   float(watt_alt),
        "delta_min": float(watt_alt - watt_ref),
        "delta_pct": float((watt_alt / watt_ref - 1.0) * 100.0),
    }

    # ─── transport planner valuation (fixed-flow rule-of-half) ───────
    planner = aggregates.compute_transport_planner_savings(
        tt_ref, tt_alt, res_ref["ucprob_block"],
        res_ref["ri"], res_ref["wi"], res_ref["HH"], wage_base=wage_ref)

    # ─── TTM time-difference summary ─────────────────────────────────
    diff = (tt_ref - tt_alt)
    np.fill_diagonal(diff, 0.0)
    off_diag = ~np.eye(diff.shape[0], dtype=bool)
    diff_off = diff[off_diag]
    ttm_diff = {
        "max_saved_min":      float(np.max(diff_off)),
        "min_saved_min":      float(np.min(diff_off)),
        "mean_saved_min":     float(np.mean(diff_off)),
        "median_saved_min":   float(np.median(diff_off)),
        "pairs_with_savings": int(np.sum(diff_off > 0)),
        "pairs_with_loss":    int(np.sum(diff_off < 0)),
        "pairs_unchanged":    int(np.sum(diff_off == 0)),
    }
    del diff, diff_off

    return {
        "pcc": pcc,
        "agg_ref": agg_ref, "agg_alt": agg_alt, "delta_agg": delta_agg,
        "watt": watt, "planner": planner, "ttm_diff": ttm_diff,
    }
