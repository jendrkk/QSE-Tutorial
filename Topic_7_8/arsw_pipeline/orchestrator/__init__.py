"""Topic 8 orchestrator package — multi-TTM Task-1 driver.

Submodules:
    ttm_catalog      — TTMSpec dataclass, TTM_REGISTRY, load_aligned_ttm
    plot_style       — set_rcparams() and figsize constants (LaTeX/Palatino, transparent)
    overlays         — load_overlays(), add_overlays_to_ax()
    map_helpers      — paired/single Δ% and Jenks choropleths (added in step 2)
    gravity_all      — per-TTM Task 1a + LaTeX table (added in step 2)
    single_pipeline  — REF Tasks 1b/1c + baseline equilibrium (added in step 3)
    comparison       — Topic 8 deltas for one (REF, ALT) pair (added in step 3)
    render           — write all artifacts for one comparison (added in step 3)

Importable from the parent package:
    from orchestrator.ttm_catalog import TTM_REGISTRY, load_aligned_ttm
"""
