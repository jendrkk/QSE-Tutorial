"""
dataio.py — load the SW2020 toolkit data and the Tutorial-9 time-cost matrices,
align everything to the 400 counties shared by both (AGS ascending), and build the
model observables exactly as scripts/ReadData.m does.

Alignment facts (verified empirically):
  * All toolkit tables (commuting_wide columns & rows, labor_tidy.region_id,
    house_prices.county_id, CountyArea, CountyBorderDist) are in identical order:
    AGS ascending, 401 counties.
  * Tutorial-9 matrices carry 400 counties: exactly one AGS is missing — 16056
    Eisenach (merged into Wartburgkreis post-2021, absent from the newer road
    network). We restrict the whole analysis to the 400 shared AGS.
  * commuting_wide raw orientation is [work x res]; ReadData transposes to
    [res x work]. uncondCom = comMat/L sums to 1. L_n (workplace) = colsum*L/mean;
    R_n (residence) = rowsum*L/mean. Both scaled by the same factor so the closed
    economy is internally consistent (sum L_n = sum R_n = N).
  * Shapefile VG250_KRS_clean_final has 401 features, EPSG:25832; join on county_id.
"""
from __future__ import annotations
from dataclasses import dataclass
import numpy as np
import pandas as pd

from . import config as C


@dataclass
class Data:
    keep: list          # 400 AGS codes, ascending
    N: int
    east: np.ndarray    # bool mask, True = East (incl. Berlin)
    # observables
    comMat: np.ndarray      # [res, work] bilateral flows
    uncondCom: np.ndarray   # [res, work], sums to 1  (= lamObs)
    condCom: np.ndarray     # [res, work], rows sum to 1
    L: float                # total flows (scalar)
    w_n: np.ndarray         # workplace wage, mean 1
    v_n: np.ndarray         # expected residential wage
    L_n: np.ndarray         # workplace employment, mean 1  (sum = N)
    R_n: np.ndarray         # residence employment, mean 1  (sum = N)
    dni: np.ndarray         # bilateral trade cost [n, i]  (Tutorial-9 tcmatr)
    tau: np.ndarray         # bilateral travel time [n, i], minutes, diagonal imputed
    Q_n: np.ndarray         # observed floor-space price (rentindex)
    area: np.ndarray        # county area, km^2
    borderdist: np.ndarray  # signed distance to inner-German border (km), East > 0


def _read_matrix(path) -> tuple[list, np.ndarray]:
    df = pd.read_csv(path)
    # first column may be an index of AGS; tcmatr/ttmatr are square with AGS headers
    if df.shape[1] == df.shape[0] + 1:
        cols = [int(c) for c in df.columns[1:]]
        M = df.iloc[:, 1:].values.astype(float)
    else:
        cols = [int(c) for c in df.columns]
        M = df.values.astype(float)
    return cols, M


def load() -> Data:
    # --- Tutorial-9 matrices define the 400-county universe -----------------
    ags_tc, tc = _read_matrix(C.TUT9_DIR / C.F_TCMATR)   # trade cost dni, diag=1
    ags_tt, tt = _read_matrix(C.TUT9_DIR / C.F_TTMATR)   # travel time, diag=0
    assert ags_tc == ags_tt, "tcmatr/ttmatr AGS order mismatch"
    keep = ags_tc                                        # 400, ascending
    keep_set = set(keep)
    N = len(keep)

    # --- commuting flows (401) -> align to 400 ------------------------------
    cw = pd.read_csv(C.DATA_DIR / C.F_COMMUTING)
    ags401 = [int(c) for c in cw.columns[1:]]
    idx = np.array([ags401.index(a) for a in keep])      # positions of kept AGS
    comMat_full = cw.iloc[:, 1:].values.astype(float)    # [work401, res401]
    comMat = comMat_full[np.ix_(idx, idx)].T             # [res, work]  (ReadData: comMat')

    L = comMat.sum()
    uncondCom = comMat / L                               # sums to 1
    condCom = uncondCom / uncondCom.sum(axis=1, keepdims=True)

    # --- wages --------------------------------------------------------------
    lt = pd.read_csv(C.DATA_DIR / C.F_LABOR).set_index("region_id").loc[keep]
    w_n = lt["median_income_workplace"].values.astype(float)
    w_n = w_n / w_n.mean()
    v_n = condCom @ w_n

    # --- employment ---------------------------------------------------------
    L_n = uncondCom.sum(axis=0) * L; L_n = L_n / L_n.mean()   # workplace
    R_n = uncondCom.sum(axis=1) * L; R_n = R_n / R_n.mean()   # residence

    # --- prices, area, border ----------------------------------------------
    Q_n = (pd.read_csv(C.DATA_DIR / C.F_HOUSE)
             .set_index("county_id").loc[keep, "rentindex"].values.astype(float))
    area = (pd.read_csv(C.DATA_DIR / C.F_AREA)
              .set_index("county_id").loc[keep, "Area"].values.astype(float))

    state = np.array([a // 1000 for a in keep])
    east = state >= C.EAST_STATE_MIN

    bd = (pd.read_csv(C.DATA_DIR / C.F_BORDER)
            .set_index("county_id").loc[keep, "BorderDist"].values.astype(float))
    bd = bd + 10.0                                       # BorderData.m: +10 for visibility
    bd[~east] = -np.abs(bd[~east])                       # West negative, East positive

    # --- travel-time diagonal imputation -----------------------------------
    tau = tt.copy()
    own = C.OWN_TIME_C * np.sqrt(area / np.pi) / C.OWN_TIME_SPEED * 60.0
    own = np.maximum(own, C.OWN_TIME_FLOOR)
    np.fill_diagonal(tau, own)

    dni = tc.copy()

    return Data(keep=keep, N=N, east=east, comMat=comMat, uncondCom=uncondCom,
                condCom=condCom, L=L, w_n=w_n, v_n=v_n, L_n=L_n, R_n=R_n,
                dni=dni, tau=tau, Q_n=Q_n, area=area, borderdist=bd)