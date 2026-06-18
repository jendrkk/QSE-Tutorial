"""
data_loaders.py
Loaders for ARSW2015 MATLAB data files and supplementary CSVs.

All public functions return plain numpy arrays (float64) or dicts thereof.
Variable names mirror MATLAB originals exactly (prepdata_big_TD86 / prepdata_big_TD
conventions) for side-by-side auditability.
"""
from __future__ import annotations
from pathlib import Path
import numpy as np
import pandas as pd
import scipy.io as sio


# ─────────────────────────────────────────────────────────────────────────────
# Internal helpers
# ─────────────────────────────────────────────────────────────────────────────

def _squeeze(arr: np.ndarray) -> np.ndarray:
    """Strip length-1 MATLAB dimensions."""
    return np.squeeze(arr)


def _col(arr: np.ndarray) -> np.ndarray:
    """1-D float64 column vector."""
    return _squeeze(arr).astype(np.float64)


def _mat(arr: np.ndarray) -> np.ndarray:
    """2-D float64 matrix; promotes 1-D to row vector."""
    a = _squeeze(arr).astype(np.float64)
    if a.ndim == 1:
        a = a.reshape(1, -1)
    return a


# ─────────────────────────────────────────────────────────────────────────────
# 1986 West Berlin — Task 1b
# ─────────────────────────────────────────────────────────────────────────────

def load_prepdata_TD86(mat_path: Path) -> dict:
    """
    Load prepdata_big_TD86.mat.

    Returns
    -------
    dict with keys:
        floor86rw   (nobs86rw,) float64  — floor space prices
        empwpl86rw  (nobs86rw,) float64  — workplace employment
        emprsd86rw  (nobs86rw,) float64  — residence employment
        tt86rw      (nobs86rw, nobs86rw) float64  — travel time matrix
        nobs86rw    int
        bzk86rw     (nobs86rw,) int  — Bezirk identifiers (1-based, 1-23)
        dummywestr  (nobs86rw,) bool  — West Berlin indicator (if present)
        fwestr      (nobs86rw,) bool  — West Berlin indicator (if present)
    """
    raw = sio.loadmat(str(mat_path), squeeze_me=True, struct_as_record=False)

    out: dict = {}
    out["floor86rw"]  = _col(raw["floor86rw"])
    out["empwpl86rw"] = _col(raw["empwpl86rw"])
    out["emprsd86rw"] = _col(raw["emprsd86rw"])
    out["tt86rw"]     = _mat(raw["tt86rw"])
    out["nobs86rw"]   = int(np.squeeze(raw["nobs86rw"]))
    out["bzk86rw"]    = _col(raw["bzk86rw"]).astype(int)

    for key in ("dummywestr", "fwestr"):
        if key in raw:
            out[key] = _col(raw[key]).astype(bool)

    expected = (out["nobs86rw"], out["nobs86rw"])
    if out["tt86rw"].shape != expected:
        raise ValueError(
            f"tt86rw shape {out['tt86rw'].shape} != {expected}"
        )
    return out


# ─────────────────────────────────────────────────────────────────────────────
# 2006 Full Berlin — Task 1c
# ─────────────────────────────────────────────────────────────────────────────

def load_prepdata_TD(mat_path: Path) -> dict:
    """
    Load prepdata_big_TD.mat.

    Returns
    -------
    dict with keys:
        floor06   (nobs06,) float64
        empwpl06  (nobs06,) float64
        emprsd06  (nobs06,) float64
        area06    (nobs06,) float64
        tt06      (nobs06, nobs06) float64
        nobs06    int
        bzk06     (nobs06,) int
        fwestr    (nobs06,) bool  (if present)
        fwestd    (nobs06,) bool  (if present)
    """
    raw = sio.loadmat(str(mat_path), squeeze_me=True, struct_as_record=False)

    out: dict = {}
    out["floor06"]  = _col(raw["floor06"])
    out["empwpl06"] = _col(raw["empwpl06"])
    out["emprsd06"] = _col(raw["emprsd06"])
    out["area06"]   = _col(raw["area06"])
    out["tt06"]     = _mat(raw["tt06"])
    out["nobs06"]   = int(np.squeeze(raw["nobs06"]))
    out["bzk06"]    = _col(raw["bzk06"]).astype(int)

    for key in ("fwestr", "fwestd"):
        if key in raw:
            out[key] = _col(raw[key]).astype(bool)

    expected = (out["nobs06"], out["nobs06"])
    if out["tt06"].shape != expected:
        raise ValueError(
            f"tt06 shape {out['tt06'].shape} != {expected}"
        )
    return out


# ─────────────────────────────────────────────────────────────────────────────
# Bezirke wage data — Task 1b target moment
# ─────────────────────────────────────────────────────────────────────────────

def load_bezirke_wages_1986(csv_path: Path) -> tuple[np.ndarray, float]:
    """
    Load wageworker1986.csv and compute target moment for GMM.

    Replicates MATLAB:
        bzkwge = csvread(...);  bzkwge = bzkwge(:,2);
        lbwdata = log(bzkwge) - mean(log(bzkwge));
        varlwdata = var(lbwdata);          % ddof=1

    Returns
    -------
    lbwdata   : (K,) demeaned log Bezirke wages
    varlwdata : scalar variance (ddof=1, matching MATLAB var())
    """
    df = pd.read_csv(csv_path, header=None)
    wages = df.iloc[:, 1].values.astype(np.float64)
    lbw = np.log(wages)
    lbw -= lbw.mean()
    return lbw, float(np.var(lbw, ddof=1))


# ─────────────────────────────────────────────────────────────────────────────
# User travel time matrix injection — Task 1c
# ─────────────────────────────────────────────────────────────────────────────

def load_user_ttm_mat(mat_path: Path, var_name: str = "tt06") -> np.ndarray:
    """
    Load user-generated travel time matrix from a .mat file.

    Parameters
    ----------
    mat_path : path to .mat file
    var_name : MATLAB variable name containing the (N×N) matrix

    Returns
    -------
    (N, N) float64 ndarray
    """
    raw = sio.loadmat(str(mat_path), squeeze_me=True, struct_as_record=False)
    if var_name not in raw:
        available = [k for k in raw.keys() if not k.startswith("_")]
        raise KeyError(
            f"'{var_name}' not found in {mat_path}. Available: {available}"
        )
    return _mat(raw[var_name])


def load_user_ttm_parquet(parquet_path: Path) -> np.ndarray:
    """
    Load user-generated travel time matrix from a parquet file.
    DataFrame order is preserved as-is; caller is responsible for alignment
    with the block ordering in prepdata_big_TD.mat.

    Returns
    -------
    (N, N) float64 ndarray
    """
    df = pd.read_parquet(parquet_path)
    return df.values.astype(np.float64)
