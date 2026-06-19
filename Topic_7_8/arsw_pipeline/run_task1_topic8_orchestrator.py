"""
run_task1_topic8_orchestrator.py — single driver for Topic-8 / Task 1 across
multiple TTM pairs.

Default REF: Standard TTM. With --arsw-ttm: REF is the ARSW Original TTM, and
output folders get an `_arsw` suffix so default-mode outputs are not overwritten.

Comparisons (3 by default):
  REF vs U5
  REF vs FAST
  REF vs GTFS

Pipeline phases:
  PHASE 1 — Task-1a gravity regression for all 5 TTMs; emit one LaTeX table in
            the style of ARSW Table III plus CSV and a per-TTM JSON summary.
  PHASE 2 — REF calibration (Task 1b ε estimation + Task 1c full inversion) and
            baseline equilibrium solve on REF TTM. Persist REF artifacts.
  PHASE 3 — For each ALT in --alts: load ALT TTM, solve equilibrium with REF
            fundamentals on ALT TTM, compute all Topic-8 deltas, render figures
            and artifacts. Free ALT TTM between iterations.

Usage:
  cd Topic_7_8/arsw_pipeline
  python run_task1_topic8_orchestrator.py                  # Standard REF, default ALTs
  python run_task1_topic8_orchestrator.py --arsw-ttm       # ARSW REF
  python run_task1_topic8_orchestrator.py --alts u5 fast   # restrict ALTs
  python run_task1_topic8_orchestrator.py --no-maps        # skip plotting (faster)
  python run_task1_topic8_orchestrator.py --skip-gravity-table

Memory: ~5 GB peak (REF TTM + ALT TTM + equilibrium kernels). 16 GB system OK.
"""
from __future__ import annotations
import argparse
import json
import time
from collections import OrderedDict
from pathlib import Path

import numpy as np

import config
import dataio
import geo

from orchestrator.plot_style import set_rcparams
from orchestrator.ttm_catalog import (
    TTMSpec, TTM_REGISTRY, load_aligned_ttm, pair_folder_name, pair_label,
)
from orchestrator.overlays import load_overlays
from orchestrator.gravity_all import run_gravity_for_ttm, build_gravity_table
from orchestrator.single_pipeline import (
    RefState, calibrate_and_solve_ref, solve_alt,
)
from orchestrator.comparison import compute_comparison
from orchestrator.render import render_comparison


# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------
def _json_safe(obj):
    if isinstance(obj, dict):
        return {k: _json_safe(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_json_safe(v) for v in obj]
    if isinstance(obj, np.ndarray):
        return obj.tolist()
    if isinstance(obj, np.floating):
        return float(obj)
    if isinstance(obj, np.integer):
        return int(obj)
    if isinstance(obj, np.bool_):
        return bool(obj)
    if isinstance(obj, Path):
        return str(obj)
    return obj


def _print_phase(title: str):
    bar = "=" * 76
    print(f"\n{bar}\n   {title}\n{bar}")


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(
        description="Topic-8 / Task-1 orchestrator: multi-TTM comparison runner")
    ap.add_argument("--arsw-ttm", action="store_true",
                    help="Use ARSW Original TTM as REF (default REF is Standard).")
    ap.add_argument("--alts", nargs="+",
                    default=["u5", "fast", "gtfs"],
                    choices=list(TTM_REGISTRY.keys()),
                    help="Which ALT TTMs to compare against (default: u5 fast gtfs).")
    ap.add_argument("--max-iter", type=int, default=1000,
                    help="Equilibrium solver max iterations (default 1000).")
    ap.add_argument("--no-maps", action="store_true",
                    help="Skip figure rendering (computation only).")
    ap.add_argument("--skip-gravity-table", action="store_true",
                    help="Skip the per-TTM gravity table; still computes REF ε·κ.")
    ap.add_argument("--out-suffix", default=None,
                    help="Override the output folder suffix "
                         "(default: '' for Standard REF, '_arsw' for --arsw-ttm).")
    ap.add_argument("--ttm-hist-sample", type=int, default=2_000_000,
                    help="Sample size for TTM diff histogram (default 2,000,000).")
    ap.add_argument("--save-alt-ucprob", action="store_true",
                    help="Also persist ALT ucprob_block .npz (large; ~500 MB).")
    args = ap.parse_args()

    set_rcparams()
    t_total = time.time()

    # ─── resolve REF and ALTs ─────────────────────────────────────────
    ref_key = "arsw" if args.arsw_ttm else "standard"
    ref_spec: TTMSpec = TTM_REGISTRY[ref_key]
    alt_keys = [k for k in args.alts if k != ref_key]
    if not alt_keys:
        raise SystemExit("No ALTs left after removing REF; nothing to do.")
    alt_specs = [TTM_REGISTRY[k] for k in alt_keys]

    # ─── output directories ──────────────────────────────────────────
    suffix = args.out_suffix if args.out_suffix is not None \
        else ("_arsw" if args.arsw_ttm else "")
    base_out = config.OUT_DIR / f"topic8_orchestrator{suffix}"
    base_fig = config.FIG_DIR / f"topic8_orchestrator{suffix}"
    base_out.mkdir(parents=True, exist_ok=True)
    base_fig.mkdir(parents=True, exist_ok=True)

    _print_phase("Topic 8 / Task 1 — Orchestrator")
    print(f"   REF  : {ref_spec.display_name}")
    print(f"   ALTs : {[s.display_name for s in alt_specs]}")
    print(f"   OUT  : {base_out}")
    print(f"   FIGS : {base_fig}")

    # ─── shared inputs ───────────────────────────────────────────────
    _print_phase("Loading shared inputs")
    d06 = dataio.load_block_data(2006)
    bez = dataio.load_bezirke_matrices()
    var_data, _ = dataio.load_wageworker_var()
    print(f"   1986 Bezirke log-wage variance target: {var_data:.6f}")
    print(f"   loading {config.SHP_BERLIN.name} ({config.NOBS_2006} blocks) ...")
    gdf = geo.load_geometry(config.SHP_BERLIN, config.NOBS_2006)
    overlays = load_overlays(crs_epsg=config.WORK_EPSG,
                             with_streets=not args.no_maps,
                             with_bezirke=True, verbose=True)

    # =================================================================
    # PHASE 1 — gravity for all 5 TTMs (REF retained for Phase 2)
    # =================================================================
    _print_phase("PHASE 1 — gravity regressions (all TTMs)")
    gravity_summary: OrderedDict = OrderedDict()
    gravity_rows = []
    tt_ref_held = None

    catalog_order = ["standard", "u5", "fast", "gtfs", "arsw"]
    for key in catalog_order:
        spec = TTM_REGISTRY[key]
        print(f"  -- {spec.display_name}")
        t0 = time.time()
        tt = load_aligned_ttm(spec, verbose=True)
        res = run_gravity_for_ttm(spec, tt, d06, bez, weight_mode="emp")
        ek_o, se_o, ex_o = res["ols"]
        ek_p, se_p, _    = res["ppml"]
        print(f"     OLS  εκ = {ek_o:.4f} (SE {se_o:.4f}, "
              f"R² {ex_o.get('r2', float('nan')):.3f})  "
              f"PPML εκ = {ek_p:.4f}  ({time.time() - t0:.1f}s)")
        gravity_summary[key] = {
            "label": spec.display_name,
            "ols_eps_kappa": ek_o, "ols_se": se_o, "ols_r2": ex_o.get("r2"),
            "ppml_eps_kappa": ek_p, "ppml_se": se_p,
            "n": ex_o.get("n"),
        }
        gravity_rows.append(res)
        if key == ref_key:
            tt_ref_held = tt
            print(f"     (REF TTM kept in memory for Phase 2)")
        else:
            del tt

    if not args.skip_gravity_table:
        tex_path = base_out / "gravity_table.tex"
        csv_path = base_out / "gravity_table.csv"
        build_gravity_table(
            gravity_rows, tex_path, csv_path,
            include_arsw_benchmark=True, bez=bez,
            caption=(r"Cross-TTM gravity estimates of $\varepsilon\kappa$ "
                     r"(emp-weighted Bezirke aggregation). "
                     r"Origin and destination fixed effects in all specifications."),
            label="tab:gravity_cross_ttm")
        print(f"   LaTeX table → {tex_path}")
        print(f"   CSV          → {csv_path}")

    (base_out / "gravity_summary.json").write_text(
        json.dumps(_json_safe(gravity_summary), indent=2))

    ref_kappaeps = float(gravity_summary[ref_key]["ols_eps_kappa"])
    print(f"\n   REF ε·κ (OLS, emp-weighted) = {ref_kappaeps:.5f}")

    # =================================================================
    # PHASE 2 — REF calibration + baseline equilibrium
    # =================================================================
    _print_phase(
        f"PHASE 2 — REF ({ref_spec.short_name}) calibration + baseline equilibrium")
    if tt_ref_held is None:
        # Defensive reload (shouldn't occur in normal operation)
        tt_ref_held = load_aligned_ttm(ref_spec, verbose=True)

    ref_state = calibrate_and_solve_ref(
        ref_spec, tt_ref_held, d06,
        kappaeps=ref_kappaeps,
        wageworker_var_data=var_data,
        alpha=config.ALPHA, beta=config.BETA,
        max_iter=args.max_iter, solver_weight=0.5,
        verbose=True)

    # Persist REF fundamentals
    fund_path = base_out / f"ref_{ref_spec.key}_fundamentals.npz"
    np.savez_compressed(
        fund_path,
        A=ref_state.cal["A"], B=ref_state.cal["B"],
        wage=ref_state.cal["wage"], CMA=ref_state.cal["CMA"],
        vv=ref_state.cal["vv"], V=ref_state.cal["V"],
        L=ref_state.cal["L"], theta=ref_state.cal["theta"],
        epsilon=np.float64(ref_state.epsilon),
        kappa=np.float64(ref_state.kappa),
        kappaeps=np.float64(ref_state.kappaeps),
        block_id=d06["block_id"])
    print(f"   REF fundamentals → {fund_path}")

    # Persist REF baseline equilibrium
    eq_path = base_out / f"ref_{ref_spec.key}_baseline_equilibrium.npz"
    np.savez_compressed(
        eq_path,
        endog=ref_state.res["endog"],
        ri=ref_state.res["ri"], wi=ref_state.res["wi"],
        Iwpl=ref_state.res["Iwpl"], Irsd=ref_state.res["Irsd"],
        Ubar=np.array(ref_state.res["Ubar"]),
        Phi=np.array(ref_state.res["Phi"]),
        HH=np.array(ref_state.res["HH"]),
        n_iter=np.array(ref_state.res["n_iter"]),
        converged=np.array(ref_state.res["converged"]),
        cpath=np.asarray(ref_state.res["cpath"]))
    print(f"   REF baseline equilibrium → {eq_path}")

    # =================================================================
    # PHASE 3 — per-comparison ALT solves + deltas + rendering
    # =================================================================
    overall: dict = {
        "args": vars(args),
        "ref": {
            "key": ref_spec.key,
            "display_name": ref_spec.display_name,
            "latex_name": ref_spec.latex_name,
        },
        "ref_state": {
            "epsilon": ref_state.epsilon,
            "kappa": ref_state.kappa,
            "kappaeps": ref_state.kappaeps,
            "calibration_converged": bool(ref_state.cal["converged"]),
            "ref_equilibrium_converged": bool(ref_state.res["converged"]),
            "ref_equilibrium_n_iter": int(ref_state.res["n_iter"]),
            "consistency": ref_state.consistency,
            "timings_s": ref_state.timings_s,
        },
        "gravity_summary": gravity_summary,
        "comparisons": OrderedDict(),
    }

    for alt_spec in alt_specs:
        pair = pair_folder_name(ref_spec, alt_spec)
        _print_phase(
            f"PHASE 3.{alt_spec.key} — {pair_label(ref_spec, alt_spec)}")
        comp_out = base_out / pair
        comp_fig = base_fig / pair
        comp_out.mkdir(parents=True, exist_ok=True)
        comp_fig.mkdir(parents=True, exist_ok=True)

        print(f"   loading ALT TTM ({alt_spec.key}) ...")
        tt_alt = load_aligned_ttm(alt_spec, verbose=True)

        print(f"   solving ALT equilibrium ...")
        res_alt = solve_alt(tt_alt, ref_state,
                            alpha=config.ALPHA, beta=config.BETA,
                            max_iter=args.max_iter, solver_weight=0.5,
                            verbose=True)

        print(f"   computing deltas ...")
        cmp = compute_comparison(ref_state, res_alt, tt_ref_held, tt_alt,
                                 beta=config.BETA)
        print(f"     ΔGDP = {cmp['delta_agg']['delta_GDP_pct']:+.3f}%   "
              f"ΔŪ  = {cmp['delta_agg']['delta_Ubar_pct']:+.3f}%   "
              f"ΔWATT = {cmp['watt']['delta_pct']:+.3f}%")

        if not args.no_maps:
            print(f"   rendering figures and artifacts ...")
            written = render_comparison(
                comp_out, comp_fig, cmp,
                ref_spec, alt_spec,
                gdf=gdf, overlays=overlays,
                ref_state=ref_state, res_alt=res_alt,
                tt_ref=tt_ref_held, tt_alt=tt_alt,
                d06=d06,
                ttm_hist_sample=args.ttm_hist_sample,
                save_alt_ucprob=args.save_alt_ucprob)
            print(f"   wrote {len(written['figures'])} figures, "
                  f"{len(written['artifacts'])} artifacts → {comp_out}")
        else:
            # Minimal JSON summary when maps are skipped
            (comp_out / "summary_no_maps.json").write_text(
                json.dumps(_json_safe({
                    "ref": ref_spec.key, "alt": alt_spec.key,
                    "agg_ref": cmp["agg_ref"], "agg_alt": cmp["agg_alt"],
                    "delta_agg": cmp["delta_agg"],
                    "watt": cmp["watt"],
                    "planner": cmp["planner"],
                    "ttm_diff": cmp["ttm_diff"],
                }), indent=2))

        overall["comparisons"][alt_spec.key] = {
            "ref_key": ref_spec.key,
            "alt_key": alt_spec.key,
            "alt_equilibrium_converged": bool(res_alt["converged"]),
            "alt_equilibrium_n_iter": int(res_alt["n_iter"]),
            "delta_agg": cmp["delta_agg"],
            "watt": cmp["watt"],
            "planner": cmp["planner"],
            "ttm_diff": cmp["ttm_diff"],
        }

        del tt_alt, res_alt, cmp

    overall["elapsed_seconds"] = time.time() - t_total
    (base_out / "orchestrator_summary.json").write_text(
        json.dumps(_json_safe(overall), indent=2))

    _print_phase(f"Done.  Elapsed: {overall['elapsed_seconds']:.1f}s")
    print(f"   OUT  → {base_out}")
    print(f"   FIGS → {base_fig}")


if __name__ == "__main__":
    main()
