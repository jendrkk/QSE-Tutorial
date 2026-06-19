"""
ttm_catalog.py — registry of the five travel-time matrices the orchestrator
compares, plus a unified loader that always returns a canonical (N×N) float64
array with no NaN and zero diagonal.

The registry is keyed by short strings the orchestrator CLI accepts:

    "standard"  Our Standard TTM   (Bus+Tram+S-Bahn+U-Bahn, U-Bahn 25 km/h, no U5)
    "u5"        Our U5 TTM         (Standard + U5 Alexanderpl↔Hbf)
    "fast"      Our FAST TTM       (Standard but U-Bahn 35 km/h)
    "gtfs"      Our GTFS TTM       (2024 VBB GTFS feed; all modes 2024)
    "arsw"      ARSW Original TTM  (prepdata_big_TD.mat variable tt06)

Non-ARSW parquets are 12308×12308 (Final.py drops one block in geometry cleanup);
`load_aligned_ttm` calls `geo.realign_user_ttm` to scatter back into canonical
12309×12309 order. The ARSW .mat is already canonical; only the diagonal is
zeroed.
"""
from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import numpy as np

import config
import dataio
import geo


REPO_ROOT = config.REPO_ROOT
TTM_STASH = REPO_ROOT / "Topic_7_8" / "TTM_stash"


@dataclass(frozen=True)
class TTMSpec:
    """Specification of one travel-time matrix in the catalog.

    Attributes
    ----------
    key            short CLI-safe identifier (used in folder names and JSON keys)
    display_name   long human-readable string (logs, progress)
    latex_name     LaTeX-safe label (plot titles, table rows); must compile with
                   matplotlib's text.usetex = True (escape & with \\&, $ with \\$)
    short_name     2–8 character label for compact contexts (axis labels, legends)
    parquet_path   path to the parquet on disk; None for ARSW (uses .mat instead)
    is_arsw        True only for the "arsw" entry
    desc           one-line description for tables / summary files
    """
    key: str
    display_name: str
    latex_name: str
    short_name: str
    parquet_path: Optional[Path]
    is_arsw: bool
    desc: str


# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------
TTM_REGISTRY: dict[str, TTMSpec] = {
    "standard": TTMSpec(
        key="standard",
        display_name="Our Standard TTM (full PT, U-Bahn 25 km/h, no U5)",
        latex_name=r"Standard TTM",
        short_name="Standard",
        parquet_path=TTM_STASH / "TTM standard" / "sample_travel_time_matrix.parquet",
        is_arsw=False,
        desc="Bus+Tram+S-Bahn+U-Bahn (U-Bahn 25 km/h), 2006 network, no U5 extension",
    ),
    "u5": TTMSpec(
        key="u5",
        display_name="Our U5 TTM (full PT, with U5 extension)",
        latex_name=r"U5 TTM",
        short_name="U5",
        parquet_path=TTM_STASH / "TTM U5" / "sample_travel_time_matrix.parquet",
        is_arsw=False,
        desc="Standard network plus U5 extension (Alexanderplatz to Hauptbahnhof)",
    ),
    "fast": TTMSpec(
        key="fast",
        display_name="Our FAST TTM (U-Bahn 35 km/h)",
        latex_name=r"FAST TTM",
        short_name="FAST",
        parquet_path=TTM_STASH / "TTM FAST" / "sample_travel_time_matrix.parquet",
        is_arsw=False,
        desc="Standard network with U-Bahn speed lifted from 25 to 35 km/h",
    ),
    "gtfs": TTMSpec(
        key="gtfs",
        display_name="Our GTFS TTM (2024 VBB feed)",
        latex_name=r"GTFS TTM (2024 VBB)",
        short_name="GTFS",
        parquet_path=TTM_STASH / "TTM GTFS" / "updated_u5_travel_time_matrix.parquet",
        is_arsw=False,
        desc="Computed from 2024 VBB GTFS feed, all modes as of 2024",
    ),
    "arsw": TTMSpec(
        key="arsw",
        display_name="ARSW Original TTM (Ahlfeldt et al. 2015)",
        latex_name=r"ARSW Original",
        short_name="ARSW",
        parquet_path=None,
        is_arsw=True,
        desc="Original ARSW TTM (tt06 from prepdata_big_TD.mat)",
    ),
}


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------
def load_aligned_ttm(spec: TTMSpec, n_canon: int = config.NOBS_2006,
                     *, verbose: bool = False) -> np.ndarray:
    """Return a canonical (n_canon × n_canon) float64 TTM, NaN-free, diag=0.

    * ARSW: read from prepdata_big_TD.mat via `dataio.load_arsw_block_ttm`.
      The .mat is already in canonical order; only the diagonal is forced to 0.
    * Others: read parquet via `dataio.load_user_ttm`, then scatter through
      `geo.realign_user_ttm`. The cleaning-and-reset_index bug in Final.py drops
      one canonical block; realign nearest-neighbour-fills that block and any
      disconnected pairs (per-row worst-case fill).

    Raises
    ------
    FileNotFoundError if the parquet (non-ARSW) is missing.
    ValueError if shape post-alignment is not (n_canon, n_canon).
    """
    if spec.is_arsw:
        tt = dataio.load_arsw_block_ttm(2006, prefer="mat")
        if tt.shape != (n_canon, n_canon):
            raise ValueError(
                f"ARSW TTM is {tt.shape}, expected ({n_canon},{n_canon})")
        tt = np.ascontiguousarray(tt, dtype=np.float64)
        np.fill_diagonal(tt, 0.0)
        if verbose:
            print(f"  loaded ARSW TTM from prepdata_big_TD.mat: {tt.shape}")
        return tt
    if spec.parquet_path is None or not spec.parquet_path.exists():
        raise FileNotFoundError(
            f"TTM '{spec.key}': parquet not found at {spec.parquet_path}")
    if verbose:
        print(f"  loading {spec.key} from {spec.parquet_path.name} ...")
    M = dataio.load_user_ttm(spec.parquet_path)
    if verbose:
        print(f"    raw shape {M.shape}; realigning to canonical {n_canon}×{n_canon} ...")
    tt = geo.realign_user_ttm(M, config.SHP_BERLIN, n_canon, verbose=verbose)
    del M
    if tt.shape != (n_canon, n_canon):
        raise ValueError(
            f"TTM '{spec.key}' post-alignment is {tt.shape}, "
            f"expected ({n_canon},{n_canon})")
    return tt


# ---------------------------------------------------------------------------
# Convenience pretty-printers
# ---------------------------------------------------------------------------
def pair_folder_name(ref_spec: TTMSpec, alt_spec: TTMSpec) -> str:
    """Folder name for a (REF, ALT) comparison: 'standard_vs_u5', 'arsw_vs_fast', …"""
    return f"{ref_spec.key}_vs_{alt_spec.key}"


def pair_label(ref_spec: TTMSpec, alt_spec: TTMSpec, *, latex: bool = False) -> str:
    """Human-readable label for the pair, e.g. 'Standard vs U5'."""
    if latex:
        return rf"{ref_spec.latex_name} vs.\ {alt_spec.latex_name}"
    return f"{ref_spec.short_name} vs {alt_spec.short_name}"
