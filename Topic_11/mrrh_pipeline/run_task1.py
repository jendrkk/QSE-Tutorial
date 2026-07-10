"""
run_task1.py — Topic 11, Task 1 orchestrator.

Pipeline:
  load -> (a) invert productivity + trade shares + P_n
        -> (b) invert quality-of-life residual b_n
        -> maps + border scatters of the inverted fundamentals
        -> estimate West-East log gaps
        -> (c) three counterfactuals: remove productivity gap / QoL gap / both
        -> effect maps, border scatters, aggregate bars
        -> summary tables (CSV + LaTeX) + npz dump

Run:  python -m mrrh_pipeline.run_task1
Every written file is printed with its full path.
"""
from __future__ import annotations
import numpy as np
import pandas as pd

from . import config as C
from . import dataio, quantify, gaps, counterfac, mapping, plots

OUTCOMES = ["wage", "workL", "resR", "housePr", "tradePr", "output"]
OUT_LABEL = {"wage": "Wage $w_n$", "workL": "Workplace emp. $L_n$",
             "resR": "Residence emp. $R_n$", "housePr": "Floor price $Q_n$",
             "tradePr": "Tradable price $P_n$", "output": "Output $w_nL_n$"}
_written = []


def _log(path):
    _written.append(str(path))
    print(f"  wrote {path}")


def _cf_outcome_series(res):
    return {"wage": res["w"], "workL": res["l"], "resR": res["r"],
            "housePr": res["q"], "tradePr": res["p"],
            "output": res["w"] * res["l"]}


def main():
    C.ensure_dirs()
    d = dataio.load()
    east = d.east
    print(f"[load] N={d.N} counties (400 shared AGS); East={int(east.sum())} "
          f"West={int((~east).sum())}")

    # ---- (a) productivity, trade shares, tradable price index --------------
    A_n, tradesh, tradeshOwn, P_n, it_a, gap_a = quantify.solve_product_trade(
        d.L_n, d.R_n, d.w_n, d.v_n, d.dni)
    print(f"[a] productivity solve: iters={it_a} max|inc-exp|={gap_a:.2e} "
          f"A_n range=[{A_n.min():.3f},{A_n.max():.3f}] "
          f"P_n range=[{P_n.min():.3f},{P_n.max():.3f}]")

    # ---- (b) quality of life -----------------------------------------------
    b_n, CMA, CPI = quantify.qol_residual(d.R_n, d.L, P_n, d.Q_n, d.tau, d.w_n)
    print(f"[b] QoL residual: b_n range=[{b_n.min():.3f},{b_n.max():.3f}]")

    sA = gaps.summary(A_n, east)
    sB = gaps.summary(b_n, east)
    print(f"[gaps] log A: West={sA['west']:+.4f} East={sA['east']:+.4f} "
          f"gap={sA['gap']:+.4f} (exp={np.exp(sA['gap']):.3f})")
    print(f"[gaps] log b: West={sB['west']:+.4f} East={sB['east']:+.4f} "
          f"gap={sB['gap']:+.4f} (exp={np.exp(sB['gap']):.3f})")

    # ---- level maps + scatters of inverted fundamentals --------------------
    _log(mapping.choropleth(np.log(A_n), d.keep,
         "Inverted log productivity  $\\log A_n$", "MAP_logA_productivity",
         label="$\\log A_n$"))
    _log(mapping.choropleth(np.log(b_n), d.keep,
         "Inverted log quality of life  $\\log b_n$", "MAP_logB_qualityoflife",
         label="$\\log b_n$"))
    _log(plots.border_scatter(d.borderdist,
         {"log $A_n$": A_n, "log $b_n$": b_n},
         "Inverted fundamentals across the former border",
         "scatter_fundamentals", ylabel="Log level (mean-normalised)"))

    # ---- (c) counterfactuals ----------------------------------------------
    gapA, gapB = sA["gap"], sB["gap"]
    cf_specs = [("prod", "i_remove_productivity_gap",  "Remove productivity gap"),
                ("qol",  "ii_remove_qol_gap",          "Remove quality-of-life gap"),
                ("both", "iii_remove_both_gaps",       "Remove both gaps")]
    results = {}
    table_rows = []

    for which, tag, nice in cf_specs:
        aC, bC, kapC, dC = gaps.build_forcings(gapA, gapB, east, d.N, which)
        res = counterfac.counter_facts(aC, bC, kapC, dC,
                                       d.w_n, d.v_n, d.uncondCom, d.L_n, d.R_n, tradesh)
        results[which] = res
        popL = (res["l"] * d.L_n).sum() / d.L_n.sum()
        popR = (res["r"] * d.R_n).sum() / d.R_n.sum()
        print(f"[c:{which}] iters={res['iters']} welfare={100*(res['welf']-1):+.3f}% "
              f"welf_mat_sd={res['welf_mat_sd']:.1e}  SL'/SL={popL:.4f} SR'/SR={popR:.4f}")

        series = _cf_outcome_series(res)

        # effect maps (log change), diverging about 0
        for oc in OUTCOMES:
            _log(mapping.choropleth(
                np.log(series[oc]), d.keep,
                f"{nice}: $\\Delta\\log$ {OUT_LABEL[oc]}",
                f"MAP_CF_{tag}_{oc}", diverging=True,
                label="$\\Delta\\log$"))

        # border scatters: labour + prices
        _log(plots.border_scatter(d.borderdist,
             {"Wage": series["wage"], "Workplace emp.": series["workL"]},
             f"{nice}: labour market", f"scatter_CF_{tag}_labour"))
        _log(plots.border_scatter(d.borderdist,
             {"Floor price": series["housePr"], "Tradable price": series["tradePr"]},
             f"{nice}: prices", f"scatter_CF_{tag}_prices"))

        # aggregate West/East bars
        wv = [np.log(series[oc])[~east].mean() for oc in OUTCOMES]
        ev = [np.log(series[oc])[east].mean() for oc in OUTCOMES]
        _log(plots.effect_bars([OUT_LABEL[o] for o in OUTCOMES], wv, ev,
             f"{nice}: mean log change by region", f"bars_CF_{tag}"))

        for oc, w, e in zip(OUTCOMES, wv, ev):
            table_rows.append({"cf": which, "outcome": oc,
                               "west_mean_dlog": w, "east_mean_dlog": e,
                               "welfare_pct": 100 * (res["welf"] - 1)})

    # ---- tables ------------------------------------------------------------
    # gap summary
    gap_df = pd.DataFrame([
        {"fundamental": "log A_n (productivity)", **sA, "exp_gap": np.exp(sA["gap"])},
        {"fundamental": "log b_n (quality of life)", **sB, "exp_gap": np.exp(sB["gap"])},
    ])[["fundamental", "west", "east", "gap", "exp_gap", "lo", "hi"]]
    p = C.OUTPUT_DIR / "gaps_summary.csv"; gap_df.to_csv(p, index=False); _log(p)
    p = C.OUTPUT_DIR / "gaps_summary.tex"
    p.write_text(gap_df.to_latex(index=False, float_format="%.4f",
                 caption="West$-$East mean log differences in inverted fundamentals.",
                 label="tab:gaps")); _log(p)

    # counterfactual outcomes (wide: outcome x cf, stacked West/East)
    cf_df = pd.DataFrame(table_rows)
    wide = cf_df.pivot_table(index="outcome",
                             columns="cf",
                             values=["west_mean_dlog", "east_mean_dlog"])
    wide = wide.reindex(OUTCOMES)
    p = C.OUTPUT_DIR / "cf_outcomes.csv"; wide.to_csv(p); _log(p)
    welf = {w: 100 * (results[w]["welf"] - 1) for w in results}
    welf_df = pd.DataFrame([welf], index=["welfare_pct"])
    p = C.OUTPUT_DIR / "cf_welfare.csv"; welf_df.to_csv(p); _log(p)
    p = C.OUTPUT_DIR / "cf_outcomes.tex"
    p.write_text(wide.to_latex(float_format="%.4f",
                 caption="Counterfactual mean log changes by region "
                         "(i: productivity, ii: quality of life, iii: both).",
                 label="tab:cf")); _log(p)

    # ---- npz dump ----------------------------------------------------------
    dump = dict(keep=np.array(d.keep), east=east, A_n=A_n, P_n=P_n, tradeshOwn=tradeshOwn,
                b_n=b_n, CMA=CMA, CPI=CPI, Q_n=d.Q_n, L_n=d.L_n, R_n=d.R_n,
                w_n=d.w_n, v_n=d.v_n, borderdist=d.borderdist,
                gapA=gapA, gapB=gapB)
    for which, res in results.items():
        for key in ("w", "v", "q", "pi", "lam", "p", "r", "l"):
            dump[f"{which}_{key}"] = res[key]
        dump[f"{which}_welf"] = res["welf"]
    p = C.OUTPUT_DIR / "task1_results.npz"; np.savez_compressed(p, **dump); _log(p)

    print(f"\n[done] {len(_written)} files written under {C.OUTPUT_DIR.parent}")
    return results


if __name__ == "__main__":
    main()