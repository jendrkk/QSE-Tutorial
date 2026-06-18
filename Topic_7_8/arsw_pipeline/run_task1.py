"""
run_task1.py — Tutorial 7 / Task 1 orchestrator (a–d), end to end.

  (a) gravity regression for ε·κ, using the student's block TTM aggregated to Bezirke
      (simple mean and employment-weighted), with the ARSW bilat_minutes matrix as a
      benchmark;
  (b) estimate ε (and κ) by matching the 1986 West Bezirk log-wage variance;
  (c) recover fundamental productivities A and amenities B (+ wages, income, density)
      for 2006, and map them;
  (d) re-run (c) with ARSW's original block TTM and map A, B side by side for comparison.

Outputs
-------
  fundamentals + estimates : Topic_7/arsw_pipeline/output/
  maps (PNG)               : Topic_7/arsw_pipeline/figs/

Run
---
  cd "<repo>/Topic_7/arsw_pipeline"
  python run_task1.py                 # full a–d with the all-modes user TTM
  python run_task1.py --ttm simple    # use simplified_travel_time_matrix.parquet
  python run_task1.py --skip-arsw     # a–c only (no ARSW comparison; no big .mat/.csv load)
  python run_task1.py --arsw-source csv   # read ARSW TTM from ttfinal_*_ren.csv instead of .mat

Memory: the 2006 block TTM is 12309² ≈ 1.2 GB (float64); the ARSW comparison loads a
second one. A machine with ≥16 GB RAM is recommended; ≥32 GB is comfortable.
"""
from __future__ import annotations
import argparse
import json
from pathlib import Path
import numpy as np
import pandas as pd

import config
import dataio
import geo
import gravity
import estimation
import calibration


def _fmt(res):
    ek, se, extra = res
    tail = "  ".join(f"{k}={v:.3g}" if isinstance(v, float) else f"{k}={v}"
                     for k, v in extra.items())
    return f"ε·κ = {ek:.5f}  (SE {se:.5f})   {tail}"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ttm", choices=["full", "simple"], default="full",
                    help="which user TTM parquet to use")
    ap.add_argument("--skip-arsw", action="store_true",
                    help="skip Task 1d (no ARSW TTM load)")
    ap.add_argument("--arsw-source", choices=["mat", "csv"], default="mat")
    ap.add_argument("--no-maps", action="store_true")
    args = ap.parse_args()

    config.OUT_DIR.mkdir(parents=True, exist_ok=True)
    config.FIG_DIR.mkdir(parents=True, exist_ok=True)
    user_ttm_path = config.USER_TTM_FULL if args.ttm == "full" else config.USER_TTM_SIMPLE
    bez_path = config.SHP_BEZIRKE23 if config.SHP_BEZIRKE23.exists() else None
    summary = {}

    # ----------------------------------------------------------------- data
    print("Loading canonical 2006 block data ...")
    d06 = dataio.load_block_data(2006, west_only=False)
    N = d06["nobs"]
    assert N == config.NOBS_2006, f"expected {config.NOBS_2006} blocks, got {N}"

    print(f"Loading + realigning user TTM ({user_ttm_path.name}) ...")
    user_M = dataio.load_user_ttm(user_ttm_path)
    print(f"  raw user TTM: {user_M.shape[0]}×{user_M.shape[1]}")
    tt_user = geo.realign_user_ttm(user_M, config.SHP_BERLIN, N)
    del user_M
    print(f"  aligned to canonical {tt_user.shape[0]}×{tt_user.shape[1]} (no NaN: "
          f"{not np.isnan(tt_user).any()})")

    # =========================================================== Task 1(a)
    print("\n=== Task 1(a): commuting gravity for ε·κ ===")
    bez = dataio.load_bezirke_matrices()
    act = d06["obsvar"][:, 1] + d06["obsvar"][:, 2]            # total employment weight
    tau_simple = dataio.aggregate_ttm_to_bezirke(tt_user, d06["bzk_mod"], weight=None)
    tau_empw = dataio.aggregate_ttm_to_bezirke(tt_user, d06["bzk_mod"], weight=act)

    g_simple = gravity.run_gravity(bez["prob"], tau_simple, bez["counts"])
    g_empw = gravity.run_gravity(bez["prob"], tau_empw, bez["counts"])
    g_arsw = gravity.run_gravity(bez["prob"], bez["minutes"], bez["counts"])
    print("  own TTM, simple mean   :", _fmt(g_simple["ols"]))
    print("  own TTM, emp-weighted  :", _fmt(g_empw["ols"]))
    print("  ARSW bilat_minutes     :", _fmt(g_arsw["ols"]), "  [benchmark]")
    print("  (PPML, own emp-weighted:", _fmt(g_empw["ppml"]), ")")

    kappaeps = float(g_empw["ols"][0])                        # primary: own TTM, emp-weighted
    summary["task1a"] = {
        "kappaeps_used": kappaeps,
        "own_simple_ols": g_simple["ols"][0], "own_empw_ols": g_empw["ols"][0],
        "own_empw_ppml": g_empw["ppml"][0], "arsw_minutes_ols": g_arsw["ols"][0],
    }
    pd.DataFrame({
        "spec": ["own_simple", "own_empweighted", "arsw_minutes"],
        "epskappa_ols": [g_simple["ols"][0], g_empw["ols"][0], g_arsw["ols"][0]],
        "se_ols": [g_simple["ols"][1], g_empw["ols"][1], g_arsw["ols"][1]],
        "epskappa_ppml": [g_simple["ppml"][0], g_empw["ppml"][0], g_arsw["ppml"][0]],
    }).to_csv(config.OUT_DIR / "task1a_gravity.csv", index=False)

    # =========================================================== Task 1(b)
    print("\n=== Task 1(b): estimate ε and κ (1986 West) ===")
    d86w = dataio.load_block_data(1986, west_only=True)
    west_idx = d06["west_idx"]
    tt_west = tt_user[np.ix_(west_idx, west_idx)]             # 2006 TTM trimmed to West
    var_data, _ = dataio.load_wageworker_var()
    est = estimation.estimate_epsilon_kappa(
        d86w["obsvar"], tt_west, d86w["bzk1937"], kappaeps, var_data,
        alpha=config.ALPHA, beta=config.BETA)
    epsilon, kappa = est["epsilon"], est["kappa"]
    summary["task1b"] = est
    del tt_west

    # =========================================================== Task 1(c)
    print("\n=== Task 1(c): recover fundamentals (2006, own TTM) ===")
    cu = calibration.calibrate(d06["obsvar"], tt_user,
                               epsilon=epsilon, kappa=kappa,
                               alpha=config.ALPHA, beta=config.BETA)
    np.savez_compressed(config.OUT_DIR / "fundamentals_userTTM.npz",
                        A=cu["A"], B=cu["B"], wage=cu["wage"], V=cu["V"],
                        theta=cu["theta"], vv=cu["vv"], block_id=d06["block_id"])
    pd.DataFrame({"block_id": d06["block_id"], "A": cu["A"], "B": cu["B"],
                  "wage": cu["wage"], "V": cu["V"], "theta": cu["theta"]}
                 ).to_csv(config.OUT_DIR / "fundamentals_userTTM.csv", index=False)
    summary["task1c"] = {"converged": bool(cu["converged"]), "gap": cu["gap"],
                         "A_gt0": int((cu["A"] > 0).sum()), "B_gt0": int((cu["B"] > 0).sum())}

    if not args.no_maps:
        print("  rendering A and B maps ...")
        gdf = geo.load_geometry(config.SHP_BERLIN, N)
        geo.plot_block_map(gdf, cu["A"], "Fundamental productivity A (2006, own TTM)",
                           config.FIG_DIR / "A_userTTM.png", bezirke_path=bez_path)
        geo.plot_block_map(gdf, cu["B"], "Fundamental amenity B (2006, own TTM)",
                           config.FIG_DIR / "B_userTTM.png", bezirke_path=bez_path)

    # =========================================================== Task 1(d)
    if not args.skip_arsw:
        print("\n=== Task 1(d): compare with ARSW original TTM ===")
        del tt_user                                          # free ~1.2 GB before second load
        print(f"  loading ARSW block TTM (source={args.arsw_source}) ...")
        tt_arsw = dataio.load_arsw_block_ttm(2006, prefer=args.arsw_source)
        if tt_arsw.shape[0] != N:
            raise ValueError(f"ARSW TTM is {tt_arsw.shape[0]}×{tt_arsw.shape[1]}, expected {N}")
        np.fill_diagonal(tt_arsw, 0.0)
        ca = calibration.calibrate(d06["obsvar"], tt_arsw,
                                   epsilon=epsilon, kappa=kappa,
                                   alpha=config.ALPHA, beta=config.BETA)
        del tt_arsw
        np.savez_compressed(config.OUT_DIR / "fundamentals_arswTTM.npz",
                            A=ca["A"], B=ca["B"], wage=ca["wage"], V=ca["V"],
                            theta=ca["theta"], block_id=d06["block_id"])
        # correlations between the two recoveries (workplace/residence blocks only)
        def _logcorr(x, y, mask):
            m = mask & (x > 0) & (y > 0)
            return float(np.corrcoef(np.log(x[m]), np.log(y[m]))[0, 1])
        rA = _logcorr(cu["A"], ca["A"], cu["Iwpl"])
        rB = _logcorr(cu["B"], ca["B"], cu["Irsd"])
        print(f"  corr(log A_own, log A_ARSW) = {rA:.4f}   "
              f"corr(log B_own, log B_ARSW) = {rB:.4f}")
        summary["task1d"] = {"corr_logA": rA, "corr_logB": rB,
                             "arsw_converged": bool(ca["converged"])}
        if not args.no_maps:
            print("  rendering comparison maps ...")
            gdf = geo.load_geometry(config.SHP_BERLIN, N)
            geo.plot_comparison(gdf, cu["A"], ca["A"], "Own TTM", "ARSW TTM",
                                "Fundamental productivity A — own vs ARSW travel times",
                                config.FIG_DIR / "compare_A.png", bezirke_path=bez_path)
            geo.plot_comparison(gdf, cu["B"], ca["B"], "Own TTM", "ARSW TTM",
                                "Fundamental amenity B — own vs ARSW travel times",
                                config.FIG_DIR / "compare_B.png", bezirke_path=bez_path)

    # ----------------------------------------------------------------- save
    with open(config.OUT_DIR / "summary.json", "w") as f:
        json.dump(summary, f, indent=2, default=float)
    print("\nDone. Estimates:")
    print(f"  ε·κ (Task 1a) = {kappaeps:.5f}")
    print(f"  ε   (Task 1b) = {epsilon:.2f}")
    print(f"  κ   (Task 1b) = {kappa:.5f}")
    print(f"Outputs -> {config.OUT_DIR}")
    print(f"Figures -> {config.FIG_DIR}")


if __name__ == "__main__":
    main()
