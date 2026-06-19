"""
render.py — write all per-comparison artifacts and figures to disk.

`render_comparison(...)` is the single public entry point. It writes:

  Figures (under fig_dir):
    delta_cma_pct.png                      single Δ% CMA full-equilibrium map
    delta_cma_decomposition.png            paired CMA: full vs TTM-only (shared cbar)
    delta_HMHR_pct.png                     paired HM and HR Δ% (shared cbar)
    delta_wage_Crent_pct.png               paired wage and Crent Δ% (shared cbar)
    delta_block_outcomes_grid.png          2x2 grid: HM, HR, wage, Crent
    baseline_HM_HR_jenks.png               baseline HM and HR Jenks paired
    ref_A_B_jenks.png                      REF fundamentals A and B Jenks paired
    convergence_ref.png                    log-iter REF convergence
    convergence_alt.png                    log-iter ALT convergence
    ttm_diff_histogram.png                 Δτ histogram, REF − ALT (random sample)
    aggregate_delta_bar.png                ΔGDP%, ΔŪ%, ΔWATT% bar plot
    watt_bar.png                           WATT before/after
    welfare_comparison_bar.png             Model ΔGDP vs planner monetised savings

  Artifacts (under out_dir):
    block_pcc.csv                          one row per block, all Δ% columns
    summary.json                           agg_ref, agg_alt, delta_agg, watt,
                                           planner, ttm_diff, ttm_paths, sizes
    alt_equilibrium.npz                    endog, Ubar, Phi, HH, n_iter, converged,
                                           cpath, ri, wi, Iwpl, Irsd (no ucprob_block
                                           by default)

Dynamic titles are built from the (ref_spec.latex_name, alt_spec.latex_name)
pair so each comparison's plots are self-labelling.
"""
from __future__ import annotations
import json
from pathlib import Path

import numpy as np
import pandas as pd

from .ttm_catalog import TTMSpec, pair_label
from .overlays import Overlays
from .single_pipeline import RefState
from .plot_style import (
    CMAP_DIVERGING, CMAP_SEQ_PRODUCTIVITY, CMAP_SEQ_AMENITY,
    DEFAULT_ALPHA_GREEN, DEFAULT_ALPHA_WATER, DEFAULT_ALPHA_STREETS,
)
from . import map_helpers as mh


def _json_safe(obj):
    """Convert NumPy scalars/arrays/bools to JSON-native types recursively."""
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
    return obj


def _suptitle(varname_tex: str, ref_spec: TTMSpec, alt_spec: TTMSpec) -> str:
    return (rf"$\Delta\%$ {varname_tex} --- "
            rf"{pair_label(ref_spec, alt_spec, latex=True)}")


def render_comparison(out_dir: Path, fig_dir: Path,
                      cmp: dict,
                      ref_spec: TTMSpec, alt_spec: TTMSpec,
                      *, gdf, overlays: Overlays,
                      ref_state: RefState, res_alt: dict,
                      tt_ref: np.ndarray, tt_alt: np.ndarray,
                      d06: dict,
                      truncate: tuple = (-50.0, 100.0),
                      ttm_hist_sample: int = 2_000_000,
                      save_alt_ucprob: bool = False) -> dict:
    """Write all figures and artifacts for one (REF, ALT) comparison.

    Returns a dict of paths written (for the orchestrator's overall summary).
    """
    out_dir = Path(out_dir); fig_dir = Path(fig_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    fig_dir.mkdir(parents=True, exist_ok=True)
    written: dict = {"figures": {}, "artifacts": {}}

    pcc = cmp["pcc"]
    res_ref = ref_state.res
    cal = ref_state.cal
    ref_lbl = ref_spec.short_name
    alt_lbl = alt_spec.short_name

    # ───────────── Δ% CMA: single + paired decomposition ──────────────
    written["figures"]["delta_cma"] = str(mh.plot_pcc_single(
        fig_dir / "delta_cma_pct.png", gdf, pcc["cma"],
        title=_suptitle(r"residence CMA (full equilibrium)", ref_spec, alt_spec),
        overlays=overlays, truncate=truncate, cmap=CMAP_DIVERGING,
        cbar_label=r"$\Delta$ CMA (\%)"))

    written["figures"]["delta_cma_decomp"] = str(mh.plot_pcc_paired(
        fig_dir / "delta_cma_decomposition.png", gdf,
        pcc["cma"], pcc["cma_ttm_only"],
        title_a=r"Full equilibrium ($\tau$ \& wages re-solve)",
        title_b=r"TTM only (wages held at REF)",
        suptitle=_suptitle(r"residence CMA --- decomposition", ref_spec, alt_spec),
        overlays=overlays, truncate=truncate, cmap=CMAP_DIVERGING,
        cbar_label=r"$\Delta$ CMA (\%)"))

    # ───────────── Δ% paired HM+HR, wage+Crent ────────────────────────
    written["figures"]["delta_HMHR"] = str(mh.plot_pcc_paired(
        fig_dir / "delta_HMHR_pct.png", gdf, pcc["HM"], pcc["HR"],
        title_a=r"$\Delta\%\, H_M$ (workplace employment)",
        title_b=r"$\Delta\%\, H_R$ (residence employment)",
        suptitle=_suptitle(r"employment", ref_spec, alt_spec),
        overlays=overlays, truncate=truncate, cmap=CMAP_DIVERGING))

    written["figures"]["delta_wage_Crent"] = str(mh.plot_pcc_paired(
        fig_dir / "delta_wage_Crent_pct.png", gdf,
        pcc["wage"], pcc["Crent"],
        title_a=r"$\Delta\%$ wage $w$",
        title_b=r"$\Delta\%$ floor price (Crent)",
        suptitle=_suptitle(r"prices", ref_spec, alt_spec),
        overlays=overlays, truncate=truncate, cmap=CMAP_DIVERGING))

    # ───────────── 2x2 grid of the four headline block outcomes ───────
    written["figures"]["delta_grid"] = str(mh.plot_pcc_grid_2x2(
        fig_dir / "delta_block_outcomes_grid.png", gdf,
        pccs={"HM":    pcc["HM"],
              "HR":    pcc["HR"],
              "wage":  pcc["wage"],
              "Crent": pcc["Crent"]},
        titles={"HM":    r"$\Delta\%\, H_M$ (workplace empl.)",
                "HR":    r"$\Delta\%\, H_R$ (residence empl.)",
                "wage":  r"$\Delta\%$ wage $w$",
                "Crent": r"$\Delta\%$ floor price (Crent)"},
        suptitle=_suptitle(r"block outcomes", ref_spec, alt_spec),
        overlays=overlays, truncate=truncate, cmap=CMAP_DIVERGING))

    # ───────────── REF descriptive (Jenks) baseline maps ──────────────
    endog_ref = res_ref["endog"]
    HM_ref = endog_ref[:, 6]; HR_ref = endog_ref[:, 7]
    written["figures"]["baseline_HM_HR"] = str(mh.plot_jenks_paired(
        fig_dir / "baseline_HM_HR_jenks.png", gdf, HM_ref, HR_ref,
        title_a=rf"$H_M$ baseline ({ref_spec.latex_name})",
        title_b=rf"$H_R$ baseline ({ref_spec.latex_name})",
        suptitle=rf"Baseline employment under {ref_spec.latex_name}",
        overlays=overlays, cmap=CMAP_SEQ_PRODUCTIVITY,
        cbar_label="workers per block", k=10, positive_only=True))

    written["figures"]["ref_A_B"] = str(mh.plot_jenks_paired(
        fig_dir / "ref_A_B_jenks.png", gdf, cal["A"], cal["B"],
        title_a=rf"$A$ productivity ({ref_spec.latex_name})",
        title_b=rf"$B$ amenity ({ref_spec.latex_name})",
        suptitle=rf"REF fundamentals --- {ref_spec.latex_name}",
        overlays=overlays, cmap=CMAP_SEQ_PRODUCTIVITY,
        k=10, positive_only=True, alpha_green=0.20))

    # ───────────── Solver convergence ─────────────────────────────────
    written["figures"]["convergence_ref"] = str(mh.plot_convergence(
        fig_dir / "convergence_ref.png", res_ref["cpath"],
        title=rf"Convergence --- REF equilibrium ({ref_spec.latex_name})"))

    written["figures"]["convergence_alt"] = str(mh.plot_convergence(
        fig_dir / "convergence_alt.png", res_alt["cpath"],
        title=rf"Convergence --- ALT equilibrium ({alt_spec.latex_name})"))

    # ───────────── TTM diff histogram ─────────────────────────────────
    written["figures"]["ttm_diff_hist"] = str(mh.plot_ttm_diff_histogram(
        fig_dir / "ttm_diff_histogram.png", tt_ref, tt_alt,
        title=rf"TTM time differences: {ref_spec.latex_name} vs {alt_spec.latex_name}",
        ref_label=ref_lbl, alt_label=alt_lbl,
        sample_size=ttm_hist_sample))

    # ───────────── Aggregate bar + WATT bar + welfare comparison ─────
    written["figures"]["aggregate_delta_bar"] = str(mh.plot_aggregate_bar(
        fig_dir / "aggregate_delta_bar.png",
        labels=[r"$\Delta$GDP", r"$\Delta\bar U$", r"$\Delta$WATT",
                r"$\Delta$wage bill", r"$\Delta\bar w$"],
        values=[cmp["delta_agg"]["delta_GDP_pct"],
                cmp["delta_agg"]["delta_Ubar_pct"],
                cmp["watt"]["delta_pct"],
                cmp["delta_agg"]["delta_wage_bill_pct"],
                cmp["delta_agg"]["delta_meanwage_pct"]],
        ylabel=r"\% change", value_fmt="{:+.2f}\\%",
        title=rf"Aggregate changes --- {pair_label(ref_spec, alt_spec, latex=True)}"))

    written["figures"]["watt_bar"] = str(mh.plot_watt_bar(
        fig_dir / "watt_bar.png",
        watt_ref_min=cmp["watt"]["ref_min"],
        watt_alt_min=cmp["watt"]["alt_min"],
        ref_label=ref_spec.latex_name, alt_label=alt_spec.latex_name,
        title=r"Population-weighted average travel time (WATT)"))

    written["figures"]["welfare"] = str(mh.plot_welfare_comparison(
        fig_dir / "welfare_comparison_bar.png",
        model_delta_gdp_eur=cmp["delta_agg"]["delta_GDP_eur"],
        planner_monetised=cmp["planner"]["monetised_value_eur_per_year"],
        title=(rf"Welfare --- model $\Delta$GDP vs transport-planner monetised savings "
               rf"({pair_label(ref_spec, alt_spec, latex=True)})")))

    # ───────────── Block-level PCC CSV ────────────────────────────────
    csv_path = out_dir / "block_pcc.csv"
    df = pd.DataFrame({"block_id": d06["block_id"],
                       **{k: v for k, v in pcc.items()}})
    df.to_csv(csv_path, index=False)
    written["artifacts"]["block_pcc_csv"] = str(csv_path)

    # ───────────── JSON summary ───────────────────────────────────────
    json_path = out_dir / "summary.json"
    json_payload = {
        "ref": {
            "key": ref_spec.key,
            "display_name": ref_spec.display_name,
            "latex_name": ref_spec.latex_name,
        },
        "alt": {
            "key": alt_spec.key,
            "display_name": alt_spec.display_name,
            "latex_name": alt_spec.latex_name,
        },
        "ref_state": {
            "epsilon": ref_state.epsilon,
            "kappa": ref_state.kappa,
            "kappaeps": ref_state.kappaeps,
            "calibration_converged": bool(cal["converged"]),
            "ref_equilibrium_converged": bool(res_ref["converged"]),
            "ref_equilibrium_n_iter": int(res_ref["n_iter"]),
            "consistency": ref_state.consistency,
            "timings_s": ref_state.timings_s,
        },
        "alt_equilibrium": {
            "converged": bool(res_alt["converged"]),
            "n_iter": int(res_alt["n_iter"]),
        },
        "agg_ref":   cmp["agg_ref"],
        "agg_alt":   cmp["agg_alt"],
        "delta_agg": cmp["delta_agg"],
        "watt":      cmp["watt"],
        "planner":   cmp["planner"],
        "ttm_diff":  cmp["ttm_diff"],
    }
    json_path.write_text(json.dumps(_json_safe(json_payload), indent=2))
    written["artifacts"]["summary_json"] = str(json_path)

    # ───────────── ALT equilibrium .npz (compact) ─────────────────────
    npz_path = out_dir / "alt_equilibrium.npz"
    payload = {
        "endog":     res_alt["endog"],
        "ri":        res_alt["ri"],
        "wi":        res_alt["wi"],
        "Iwpl":      res_alt["Iwpl"],
        "Irsd":      res_alt["Irsd"],
        "Ubar":      np.array(res_alt["Ubar"]),
        "Phi":       np.array(res_alt["Phi"]),
        "HH":        np.array(res_alt["HH"]),
        "n_iter":    np.array(res_alt["n_iter"]),
        "converged": np.array(res_alt["converged"]),
        "cpath":     np.asarray(res_alt["cpath"]),
    }
    if save_alt_ucprob:
        payload["ucprob_block"] = res_alt["ucprob_block"].astype(np.float32)
    np.savez_compressed(npz_path, **payload)
    written["artifacts"]["alt_equilibrium_npz"] = str(npz_path)

    return written
