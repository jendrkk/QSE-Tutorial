"""
dataio.py — load canonical block data, Bezirke commuting matrices, and travel-time
matrices (the student's own, and ARSW's original), all keyed to canonical block order.

Canonical order == caldata{year}_ren.csv row order (verified == Berlin4matlab.shp
STAT_BLOCK order, 1:1). empadjust scales workplace employment so its total equals
residence employment, applied on the SAME subset that is used downstream (full for 2006,
West-only for 1986), matching prepdata_TD.m / prepdata_TD86.m.
"""
from __future__ import annotations
from pathlib import Path
import numpy as np
import pandas as pd

import config
from core import mod_bezirk


# ---------------------------------------------------------------------------
# Block data
# ---------------------------------------------------------------------------
def _read_caldata(path):
    df = pd.read_csv(path, header=None)
    df.columns = config.CALDATA_COLS[:df.shape[1]]
    return df


def _empadjust(empwpl, emprsd):
    """empadjust.m : rescale workplace employment so Σ empwpl == Σ emprsd."""
    return empwpl * (emprsd.sum() / empwpl.sum())


def load_block_data(year=2006, west_only=False):
    """
    Return a dict for the requested cross-section:
      obsvar   : (N,4) array [floor_price Q, empwpl (adjusted), emprsd, area K]
      bzk1937  : historic Bezirke (1-23)
      bzk_mod  : modern Bezirke (1-12)
      dummywest, coords (x,y), block_id, west_idx (canonical indices kept), nobs.
    The row order is canonical (full data) when west_only=False, or the West subset in
    canonical order when west_only=True.
    """
    path = config.CALDATA_2006 if year == 2006 else config.CALDATA_1986
    df = _read_caldata(path)
    full_west_idx = np.where(df["dummywest"].values == 1)[0]
    if west_only:
        df = df.iloc[full_west_idx].reset_index(drop=True)

    empwpl = _empadjust(df["emp_wpl"].values.astype(float), df["emp_rsd"].values.astype(float))
    obsvar = np.column_stack([
        df["floor"].values.astype(float),      # Q  (floor-space price)
        empwpl,                                 # empwpl (adjusted)
        df["emp_rsd"].values.astype(float),     # emprsd
        df["finhalt"].values.astype(float),     # K  (block area)
    ])
    bzk1937 = df["bezirk1937"].values.astype(int)
    coords = np.column_stack([df["x_coord"].values, df["y_coord"].values]).astype(float)
    return {
        "obsvar": obsvar,
        "bzk1937": bzk1937,
        "bzk_mod": mod_bezirk(bzk1937),
        "dummywest": df["dummywest"].values.astype(int),
        "coords": coords,
        "block_id": df["block_id"].values,
        "west_idx": full_west_idx,           # canonical indices of West blocks (full order)
        "nobs": len(df),
    }


# ---------------------------------------------------------------------------
# Bezirke commuting matrices (12×12, rows=origin I, cols=destination J)
# ---------------------------------------------------------------------------
def load_bezirke_matrices():
    prob    = np.loadtxt(config.BILAT_PROB,    delimiter=",")
    minutes = np.loadtxt(config.BILAT_MINUTES, delimiter=",")
    counts  = np.loadtxt(config.BILAT_COMMUTE, delimiter=",")
    return {"prob": prob, "minutes": minutes, "counts": counts}


def load_wageworker_var(path=None):
    """Target moment: Var(log Bezirke wages, ddof=1) from the 1986 West wage data."""
    path = path or config.WAGEWORKER_1986
    wg = pd.read_csv(path, header=None)
    lb = np.log(wg.iloc[:, 1].values.astype(float))
    lb -= lb.mean()
    return float(np.var(lb, ddof=1)), wg.iloc[:, 0].values.astype(int)


# ---------------------------------------------------------------------------
# Travel-time matrices
# ---------------------------------------------------------------------------
def load_user_ttm(path=None):
    """Load the student's block travel-time matrix (parquet, index centroid_0..centroid_M-1)
    as an (M×M) float array in centroid order (== Final.py surviving-block order)."""
    path = path or config.USER_TTM_FULL
    df = pd.read_parquet(path)
    # order rows/cols by the integer centroid suffix, to be robust to storage order
    def _key(lbls):
        s = pd.Index(lbls).astype(str)
        if s.str.contains("_").all():
            return s.str.rsplit("_", n=1).str[-1].astype(int)
        return np.arange(len(lbls))
    df = df.iloc[np.argsort(_key(df.index).values)]
    df = df[df.columns[np.argsort(_key(df.columns).values)]]
    return df.values.astype(float)


def load_arsw_block_ttm(year=2006, prefer="mat"):
    """ARSW's original block travel-time matrix in canonical order.
    prefer='mat' reads tt06 / tt86rw from prepdata_big_TD(.86).mat (already canonical);
    prefer='csv' reads ttfinal_{year}_ren.csv and drops the first column (the MATLAB
    `tt(:,1)=[]`). Returns a square float array. NOTE: 2006 is ~12309² — large."""
    if prefer == "mat":
        var = "tt06" if year == 2006 else "tt86rw"
        mat = config.PREPDATA_TD if year == 2006 else config.PREPDATA_TD86
        if not Path(mat).exists():
            raise FileNotFoundError(f"{mat} not found; pass prefer='csv'")
        try:
            from scipy.io import loadmat
            d = loadmat(mat, variable_names=[var])
            if var in d:
                return np.asarray(d[var], float)
            raise KeyError(var)
        except (NotImplementedError, KeyError, ValueError):
            try:                                            # v7.3 (HDF5)
                import h5py
                with h5py.File(mat, "r") as f:
                    arr = np.array(f[var])
                return arr.T.astype(float) if arr.ndim == 2 else arr.astype(float)
            except Exception as e:
                raise RuntimeError(
                    f"could not read {var} from {mat} ({e}); pass prefer='csv'")
    # CSV fallback (drop first column == MATLAB tt(:,1)=[])
    csv = config.TTFINAL_2006 if year == 2006 else config.TTFINAL_1986
    arr = pd.read_csv(csv, header=None).values.astype(float)
    if arr.shape[1] == arr.shape[0] + 1:
        arr = arr[:, 1:]
    return arr


# ---------------------------------------------------------------------------
# Block → Bezirke aggregation of a travel-time matrix (Task 1a)
# ---------------------------------------------------------------------------
def aggregate_ttm_to_bezirke(tt_block, bzk_mod, weight=None, n_bezirke=12):
    """
    Aggregate an (N×N) block travel-time matrix to a (12×12) Bezirke matrix.
    weight=None       -> simple mean of block-pair times in each Bezirk pair;
    weight=array (N,) -> weighted mean with pair weight w_i·w_j (e.g. employment),
                          i.e. the expected commute time of a representative commuter.
    Rows = origin Bezirk I, cols = destination Bezirk J (same orientation as tt_block).
    """
    N = tt_block.shape[0]
    out = np.full((n_bezirke, n_bezirke), np.nan)
    idx = [np.where(bzk_mod == b)[0] for b in range(1, n_bezirke + 1)]
    w = None if weight is None else np.asarray(weight, float)
    for a in range(n_bezirke):
        ia = idx[a]
        if len(ia) == 0:
            continue
        for b in range(n_bezirke):
            jb = idx[b]
            if len(jb) == 0:
                continue
            sub = tt_block[np.ix_(ia, jb)]
            if w is None:
                out[a, b] = np.nanmean(sub)
            else:
                W = np.outer(w[ia], w[jb])
                m = np.isfinite(sub)
                out[a, b] = (sub[m] * W[m]).sum() / W[m].sum() if m.any() else np.nan
    return out
