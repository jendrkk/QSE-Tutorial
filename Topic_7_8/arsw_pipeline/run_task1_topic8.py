"""
run_task1_topic8.py — Tutorial 8 / Task 1 orchestrator (U5 subway counterfactual).

Pipeline (subtasks of Task 1):
  (a) [DONE upstream]  U5 TTM precomputed and saved as
      Topic_7_8/TTM/updated_u5_travel_time_matrix.parquet.
  (b) Solve baseline + U5 equilibria. Map block-level Δ% in residence commuter
      market access (CMA).
  (c) Map block-level Δ% in workplace employment HM, residence employment HR,
      wage, and combined floor price Crent.
  (d) Aggregate: ΔGDP, ΔŪ; verify total HM ≈ HH (closed city) and Σ HR ≈ HH.
  (e) Population-weighted average travel time before (baseline) and after (U5).
  (f) Transport planner valuation under several VoT scenarios; compare to model.

Outputs
-------
  Topic_7_8/arsw_pipeline/output/topic8/
    baseline_fundamentals.npz   (calibration on baseline TTM)
    baseline_equilibrium.npz    (endog, cpath, Phi, Ubar, ri/wi/Iwpl/Irsd)
    u5_equilibrium.npz
    summary.json                (all Task-1 b–f scalars)
  Topic_7_8/arsw_pipeline/figs/topic8/
    delta_cma_pct.png
    delta_cma_ttm_only_pct.png  (held-wage variant; isolates TTM effect)
    delta_HM_pct.png
    delta_HR_pct.png
    delta_wage_pct.png
    delta_Crent_pct.png
    convergence_baseline.png
    convergence_u5.png

Usage
-----
  cd Topic_7_8/arsw_pipeline
  python run_task1_topic8.py                        # default: read ε, εκ from summary.json
  python run_task1_topic8.py --epsilon 6.83 --kappaeps 0.07   # ARSW benchmark
  python run_task1_topic8.py --recalibrate          # force fresh calibration
  python run_task1_topic8.py --max-iter 2000        # larger solver budget
  python run_task1_topic8.py --no-maps              # skip plot rendering

Memory: each equilibrium solve holds ~2 GB inside the loop (kernel +
scratch); both TTMs at ~1.2 GB each. Plan for 16 GB peak.
"""
from __future__ import annotations
import argparse
import json
import time
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import config
import dataio
import geo
import calibration
import solver
import accessibility
import aggregates


# ===========================================================================
# Helpers
# ===========================================================================

def _read_topic7_estimates():
    """Try to read (epsilon, kappaeps) from Topic-7 summary.json. Returns
    (epsilon, kappaeps, source) where source ∈ {'task7_summary', 'defaults'}."""
    path = config.OUT_DIR / "summary.json"
    if path.exists():
        try:
            j = json.loads(path.read_text())
            eps = float(j["task1b"]["epsilon"])
            ek = float(j["task1b"].get("kappaeps", j["task1a"]["kappaeps_used"]))
            return eps, ek, "task7_summary"
        except Exception as e:
            print(f"  (couldn't parse {path}: {e!r}; using defaults)")
    return (config.EPSILON_HAT_DEFAULT, config.KAPPAEPS_HAT_DEFAULT, "defaults")


def _pct_change(x_u5, x_base, mask=None, eps=1e-12):
    """Element-wise percentage change (x_u5/x_base - 1)*100 with safe handling.

    NaN/inf wherever |x_base| < eps; optionally restricted to a boolean mask
    (positions outside the mask returned as NaN).
    """
    x_u5 = np.asarray(x_u5, float); x_base = np.asarray(x_base, float)
    out = np.full_like(x_base, np.nan)
    ok = np.abs(x_base) >= eps
    if mask is not None:
        ok &= mask
    out[ok] = (x_u5[ok] / x_base[ok] - 1.0) * 100.0
    return out


def _save_equilibrium_npz(path, result, *, save_ucprob_block=False):
    """Persist solver output. `ucprob_block` is ~1 GB — saved only if asked."""
    payload = {
        "endog": result["endog"],
        "Iwpl": result["Iwpl"], "Irsd": result["Irsd"],
        "ri": result["ri"], "wi": result["wi"],
        "Ubar": np.array(result["Ubar"]),
        "Phi": np.array(result["Phi"]),
        "HH": np.array(result["HH"]),
        "converged": np.array(result["converged"]),
        "n_iter": np.array(result["n_iter"]),
        "cpath": result["cpath"],
    }
    if save_ucprob_block:
        payload["ucprob_block"] = result["ucprob_block"].astype(np.float32)
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    np.savez_compressed(path, **payload)


def _plot_convergence(cpath, out_path, title):
    """Convergence-path plot: max-log-gap (per target) vs iteration."""
    cp = np.asarray(cpath)
    if cp.size == 0:
        return
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.semilogy(cp[:, 6], cp[:, 0], label="max |Δlog wage|", color="C0")
    ax.semilogy(cp[:, 6], cp[:, 1], label="max |Δlog q|",    color="C1", ls="--")
    ax.semilogy(cp[:, 6], cp[:, 2], label="max |Δlog Q|",    color="C2", ls=":")
    ax.semilogy(cp[:, 6], cp[:, 3], label="max |Δlog θ|",    color="C3", ls="-.")
    ax.set_xlabel("iteration"); ax.set_ylabel("max |Δlog| (rounded 2dp)")
    ax.set_title(title); ax.grid(True, which="both", alpha=0.3); ax.legend(fontsize=9)
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=200, bbox_inches="tight"); plt.close(fig)


def _python_floats(obj):
    """Recursively convert NumPy scalars/bools/arrays in a nested dict to JSON-safe Python."""
    if isinstance(obj, dict):
        return {k: _python_floats(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_python_floats(v) for v in obj]
    if isinstance(obj, np.ndarray):
        return obj.tolist()
    if isinstance(obj, (np.floating,)):
        return float(obj)
    if isinstance(obj, (np.integer,)):
        return int(obj)
    if isinstance(obj, (np.bool_,)):
        return bool(obj)
    return obj


# ===========================================================================
# Main
# ===========================================================================

def main():
    ap = argparse.ArgumentParser(description="Topic 8 / Task 1: U5 counterfactual")
    ap.add_argument("--epsilon", type=float, default=None,
                    help="Frechet shape; default: from output/summary.json or config")
    ap.add_argument("--kappaeps", type=float, default=None,
                    help="reduced-form ε·κ; default: from output/summary.json or config")
    ap.add_argument("--recalibrate", action="store_true",
                    help="force calibration even if Topic-8 cache exists")
    ap.add_argument("--max-iter", type=int, default=1000,
                    help="solver max iterations (default 1000)")
    ap.add_argument("--no-maps", action="store_true",
                    help="skip rendering of maps")
    ap.add_argument("--save-ucprob", action="store_true",
                    help="persist ucprob_block to .npz (large; ~500 MB each)")
    args = ap.parse_args()

    config.OUT_DIR_TOPIC8.mkdir(parents=True, exist_ok=True)
    config.FIG_DIR_TOPIC8.mkdir(parents=True, exist_ok=True)
    bez_path = config.SHP_BEZIRKE23 if config.SHP_BEZIRKE23.exists() else None
    summary = {}
    t_start = time.time()

    # ----- 1. parameters ----------------------------------------------------
    eps_default, ek_default, source = _read_topic7_estimates()
    epsilon = args.epsilon if args.epsilon is not None else eps_default
    kappaeps = args.kappaeps if args.kappaeps is not None else ek_default
    # `kappa` for calibration is εκ / ε:
    kappa = kappaeps / epsilon
    summary["parameters"] = {
        "epsilon": epsilon, "kappaeps": kappaeps, "kappa": kappa,
        "epsilon_source": "cli" if args.epsilon is not None else source,
        "kappaeps_source": "cli" if args.kappaeps is not None else source,
        "alpha": config.ALPHA, "beta": config.BETA,
    }
    print(f"=== Parameters (epsilon={epsilon}, kappaeps={kappaeps}, kappa={kappa:.5g}) ===")
    print(f"    source: {source}")

    # ----- 2. data ----------------------------------------------------------
    print("\n=== Loading 2006 block data ===")
    d06 = dataio.load_block_data(2006, west_only=False)
    N = d06["nobs"]
    assert N == config.NOBS_2006, f"expected {config.NOBS_2006} blocks, got {N}"
    obsvar = d06["obsvar"]

    print(f"\n=== Loading baseline TTM ({config.USER_TTM_BASELINE.name}) ===")
    M_base = dataio.load_user_ttm(config.USER_TTM_BASELINE)
    tt_base = geo.realign_user_ttm(M_base, config.SHP_BERLIN, N)
    del M_base
    print(f"    aligned to canonical ({tt_base.shape[0]}, {tt_base.shape[1]}), "
          f"no NaN: {not np.isnan(tt_base).any()}")

    print(f"\n=== Loading U5 TTM ({config.USER_TTM_U5.name}) ===")
    M_u5 = dataio.load_user_ttm(config.USER_TTM_U5)
    tt_u5 = geo.realign_user_ttm(M_u5, config.SHP_BERLIN, N)
    del M_u5
    print(f"    aligned to canonical ({tt_u5.shape[0]}, {tt_u5.shape[1]}), "
          f"no NaN: {not np.isnan(tt_u5).any()}")

    # Sanity: U5 should reduce some travel times (never increase any meaningfully)
    dmax = float((tt_base - tt_u5).max())
    dmin = float((tt_base - tt_u5).min())
    print(f"    max time saved per O-D pair: {dmax:.2f} min; "
          f"min (negative = increases): {dmin:.2f} min")
    summary["ttm"] = {
        "max_time_saved_per_pair_min": dmax,
        "min_time_saved_per_pair_min": dmin,
        "mean_time_saved_per_pair_min": float((tt_base - tt_u5).mean()),
    }

    # ----- 3. fundamentals (baseline TTM) ----------------------------------
    cache = config.OUT_DIR_TOPIC8 / "baseline_fundamentals.npz"
    if cache.exists() and not args.recalibrate:
        print(f"\n=== Loading cached fundamentals from {cache.name} ===")
        z = np.load(cache, allow_pickle=False)
        cal = {k: z[k] for k in z.files}
        cal["converged"] = bool(cal["converged"])
        # rederive masks (not stored to keep file small)
        cal["Iwpl"] = cal["A"] != 0
        cal["Irsd"] = cal["B"] != 0
    else:
        print("\n=== Calibrating fundamentals on BASELINE TTM (Topic-8 fresh run) ===")
        cal = calibration.calibrate(obsvar, tt_base,
                                     epsilon=epsilon, kappa=kappa,
                                     alpha=config.ALPHA, beta=config.BETA)
        np.savez_compressed(cache,
                            A=cal["A"], B=cal["B"], wage=cal["wage"],
                            vv=cal["vv"], V=cal["V"], L=cal["L"], theta=cal["theta"],
                            CMA=cal["CMA"], converged=np.array(cal["converged"]),
                            gap=np.array(cal["gap"]), block_id=d06["block_id"])
        print(f"    saved -> {cache}")
    summary["calibration"] = {
        "cached": cache.exists() and not args.recalibrate,
        "A_gt0": int((cal["A"] > 0).sum()), "B_gt0": int((cal["B"] > 0).sum()),
        "converged": bool(cal["converged"]),
    }

    # ----- 4. assemble fund matrix ----------------------------------------
    fund = solver.build_fund(obsvar, cal)
    print(f"\n=== fund matrix assembled: {fund.shape} ===")

    # ----- 5. baseline equilibrium ----------------------------------------
    print(f"\n=== Solving BASELINE equilibrium ===")
    t0 = time.time()
    res_base = solver.solve_equilibrium(
        fund, tt_base, epsilon=epsilon, kappaeps=kappaeps,
        alpha=config.ALPHA, beta=config.BETA,
        max_iter=args.max_iter, weight=0.5, verbose=True)
    print(f"    elapsed: {time.time() - t0:.1f}s  converged={res_base['converged']}  "
          f"iters={res_base['n_iter']}")
    consistency = solver.self_consistency_report(res_base, obsvar, cal, verbose=True)
    summary["baseline_consistency"] = consistency

    # warn if reproduction is unusually poor — but do not abort (small deviations
    # are normal since the solver converges on rounded values)
    if (consistency["max_abs_HM_minus_HMT"] > 50.0
            or consistency["max_abs_HR_minus_HRT"] > 50.0):
        print(">>>> WARNING: baseline reproduction is unusually poor; check inputs.")

    _save_equilibrium_npz(config.OUT_DIR_TOPIC8 / "baseline_equilibrium.npz",
                          res_base, save_ucprob_block=args.save_ucprob)
    if not args.no_maps:
        _plot_convergence(res_base["cpath"],
                          config.FIG_DIR_TOPIC8 / "convergence_baseline.png",
                          "Convergence path — baseline equilibrium")

    # ----- 6. U5 equilibrium ----------------------------------------------
    print(f"\n=== Solving U5 equilibrium ===")
    t0 = time.time()
    res_u5 = solver.solve_equilibrium(
        fund, tt_u5, epsilon=epsilon, kappaeps=kappaeps,
        alpha=config.ALPHA, beta=config.BETA,
        max_iter=args.max_iter, weight=0.5, verbose=True)
    print(f"    elapsed: {time.time() - t0:.1f}s  converged={res_u5['converged']}  "
          f"iters={res_u5['n_iter']}")
    _save_equilibrium_npz(config.OUT_DIR_TOPIC8 / "u5_equilibrium.npz",
                          res_u5, save_ucprob_block=args.save_ucprob)
    if not args.no_maps:
        _plot_convergence(res_u5["cpath"],
                          config.FIG_DIR_TOPIC8 / "convergence_u5.png",
                          "Convergence path — U5 counterfactual equilibrium")

    # ----- 7. block-level Δ% ----------------------------------------------
    endog_base = res_base["endog"]
    endog_u5 = res_u5["endog"]
    Iwpl = res_base["Iwpl"]; Irsd = res_base["Irsd"]
    Iemp = Iwpl | Irsd

    pcc = {
        "wage":  _pct_change(endog_u5[:, 0], endog_base[:, 0], mask=Iwpl),
        "HM":    _pct_change(endog_u5[:, 6], endog_base[:, 6], mask=Iwpl),
        "HR":    _pct_change(endog_u5[:, 7], endog_base[:, 7], mask=Irsd),
        "Crent": _pct_change(endog_u5[:, 8], endog_base[:, 8], mask=Iemp),
        "Q":     _pct_change(endog_u5[:, 4], endog_base[:, 4], mask=Irsd),
        "q":     _pct_change(endog_u5[:, 5], endog_base[:, 5], mask=Iwpl),
        "Y":     _pct_change(endog_u5[:, 3], endog_base[:, 3], mask=Iwpl),
        "theta": _pct_change(endog_u5[:, 2], endog_base[:, 2], mask=Iemp),
    }

    # ----- 8. CMA: baseline, U5, and held-wage (TTM-only) -----------------
    print(f"\n=== Computing residence CMA (baseline, U5, TTM-only) ===")
    wage_base = endog_base[:, 0]
    wage_u5 = endog_u5[:, 0]
    cma_base = accessibility.compute_cma_residence(
        tt_base, wage_base, epsilon=epsilon, kappaeps=kappaeps,
        Iwpl=Iwpl, Irsd=Irsd)
    cma_u5_full = accessibility.compute_cma_residence(
        tt_u5, wage_u5, epsilon=epsilon, kappaeps=kappaeps,
        Iwpl=Iwpl, Irsd=Irsd)
    cma_u5_ttm_only = accessibility.compute_cma_residence(
        tt_u5, wage_base, epsilon=epsilon, kappaeps=kappaeps,
        Iwpl=Iwpl, Irsd=Irsd)
    pcc["cma"] = _pct_change(cma_u5_full, cma_base, mask=Irsd)
    pcc["cma_ttm_only"] = _pct_change(cma_u5_ttm_only, cma_base, mask=Irsd)

    # ----- 9. aggregates --------------------------------------------------
    print(f"\n=== Computing aggregates ===")
    agg_base = aggregates.compute_aggregates(endog_base, res_base["Ubar"], res_base["HH"])
    agg_u5 = aggregates.compute_aggregates(endog_u5, res_u5["Ubar"], res_u5["HH"])
    delta_agg = {
        "delta_GDP_eur":       agg_u5["GDP"] - agg_base["GDP"],
        "delta_GDP_pct":       (agg_u5["GDP"] / agg_base["GDP"] - 1.0) * 100.0,
        "delta_Ubar":          agg_u5["Ubar"] - agg_base["Ubar"],
        "delta_Ubar_pct":      (agg_u5["Ubar"] / agg_base["Ubar"] - 1.0) * 100.0,
        "delta_wage_bill_pct": (agg_u5["wage_bill"] / agg_base["wage_bill"] - 1.0) * 100.0,
        "delta_meanwage_pct":  (agg_u5["mean_wage_emp"] / agg_base["mean_wage_emp"] - 1.0) * 100.0,
        "total_HM_check_HH_base": agg_base["total_HM"] - agg_base["HH"],   # ≈ 0
        "total_HM_check_HH_u5":   agg_u5["total_HM"]   - agg_u5["HH"],
        "total_HR_check_HH_base": agg_base["total_HR"] - agg_base["HH"],
        "total_HR_check_HH_u5":   agg_u5["total_HR"]   - agg_u5["HH"],
    }
    summary["aggregates_baseline"] = agg_base
    summary["aggregates_u5"] = agg_u5
    summary["aggregates_delta"] = delta_agg

    # ----- 10. WATT --------------------------------------------------------
    print(f"\n=== Weighted average travel time (before / after) ===")
    watt_base = aggregates.compute_weighted_travel_time(
        res_base["ucprob_block"], tt_base, res_base["ri"], res_base["wi"])
    watt_u5 = aggregates.compute_weighted_travel_time(
        res_u5["ucprob_block"], tt_u5, res_u5["ri"], res_u5["wi"])
    summary["watt"] = {
        "watt_base_min": watt_base,
        "watt_u5_min": watt_u5,
        "delta_watt_min": watt_u5 - watt_base,
        "delta_watt_pct": (watt_u5 / watt_base - 1.0) * 100.0,
    }
    print(f"    WATT baseline = {watt_base:.3f} min  |  WATT U5 = {watt_u5:.3f} min  "
          f"|  Δ = {watt_u5 - watt_base:+.3f} min "
          f"({(watt_u5/watt_base - 1)*100:+.2f}%)")

    # ----- 11. transport planner valuation --------------------------------
    print(f"\n=== Transport planner valuation ===")
    planner = aggregates.compute_transport_planner_savings(
        tt_base, tt_u5, res_base["ucprob_block"],
        res_base["ri"], res_base["wi"], res_base["HH"],
        wage_base=endog_base[:, 0])
    summary["transport_planner"] = planner
    print(f"    ΔT = {planner['total_savings_minutes_per_day']:.0f} person-min/day "
          f"≈ {planner['total_savings_hours_per_year']:.0f} person-h/year")
    print(f"    monetised (VoT scenarios): {planner['monetised_value_eur_per_year']}")
    print(f"    model ΔGDP/yr               : "
          f"{delta_agg['delta_GDP_eur']:.0f}  ({delta_agg['delta_GDP_pct']:+.3f}%)")

    # ----- 12. maps --------------------------------------------------------
    if not args.no_maps:
        print(f"\n=== Rendering maps ===")
        gdf = geo.load_geometry(config.SHP_BERLIN, N)
        geo.plot_pcc_map(gdf, pcc["cma"],
                         title="Δ% in residence commuter market access (full equilibrium)",
                         out_path=config.FIG_DIR_TOPIC8 / "delta_cma_pct.png",
                         truncate=(-50, 100), bezirke_path=bez_path,
                         legend_label="% change")
        geo.plot_pcc_map(gdf, pcc["cma_ttm_only"],
                         title="Δ% in residence CMA (TTM-only, wages held)",
                         out_path=config.FIG_DIR_TOPIC8 / "delta_cma_ttm_only_pct.png",
                         truncate=(-50, 100), bezirke_path=bez_path,
                         legend_label="% change")
        for key, title in [
            ("HM",    "Δ% workplace employment (HM)"),
            ("HR",    "Δ% residence employment (HR)"),
            ("wage",  "Δ% workplace wage"),
            ("Crent", "Δ% combined floor price (Crent)"),
        ]:
            geo.plot_pcc_map(gdf, pcc[key],
                             title=title,
                             out_path=config.FIG_DIR_TOPIC8 / f"delta_{key}_pct.png",
                             truncate=(-50, 100), bezirke_path=bez_path,
                             legend_label="% change")

    # ----- 13. save full summary -----------------------------------------
    pcc_df = pd.DataFrame({"block_id": d06["block_id"], **pcc})
    pcc_df.to_csv(config.OUT_DIR_TOPIC8 / "block_pcc.csv", index=False)
    summary["elapsed_seconds"] = time.time() - t_start
    with open(config.OUT_DIR_TOPIC8 / "summary.json", "w") as f:
        json.dump(_python_floats(summary), f, indent=2)

    print(f"\n=== Done. Elapsed: {summary['elapsed_seconds']:.1f}s ===")
    print(f"    Outputs -> {config.OUT_DIR_TOPIC8}")
    print(f"    Figures -> {config.FIG_DIR_TOPIC8}")


if __name__ == "__main__":
    main()
