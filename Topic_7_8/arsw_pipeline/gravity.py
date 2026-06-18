"""
gravity.py — Task 1a: commuting gravity regression for ε·κ.

Model: ln π_IJ = ϑ_I + ς_J − (ε·κ) τ_IJ + e_IJ  (ARSW Eq. 25 / Table III), origin and
destination Bezirke fixed effects. The coefficient on bilateral travel time identifies
ε·κ. Estimated by OLS on ln π and by PPML (Poisson) on π / on the raw counts.

Validated on the ARSW bilat_* data: OLS ε·κ = 0.0697 (SE 0.0056, R²=0.83),
matching the toolkit's reduced-form value 0.07; PPML = 0.0797.
"""
from __future__ import annotations
import warnings
import numpy as np
import pandas as pd
import statsmodels.api as sm
from statsmodels.genmod.generalized_linear_model import GLM
from statsmodels.genmod import families


def matrix_to_long(M):
    df = pd.DataFrame(M)
    df.index = np.arange(1, M.shape[0] + 1)
    df.columns = np.arange(1, M.shape[1] + 1)
    s = df.stack().reset_index()
    s.columns = ["I", "J", "val"]
    return s


def build_gravity_df(prob, tau, counts=None):
    g = matrix_to_long(prob).rename(columns={"val": "pi"})
    g = g.merge(matrix_to_long(tau).rename(columns={"val": "tau"}), on=["I", "J"])
    if counts is not None:
        g = g.merge(matrix_to_long(counts).rename(columns={"val": "n"}), on=["I", "J"])
    g = g[np.isfinite(g["tau"]) & (g["pi"] > 0)].copy()
    g["ln_pi"] = np.log(g["pi"])
    g["I"] = g["I"].astype(int).astype(str)
    g["J"] = g["J"].astype(int).astype(str)
    return g


def _X(df):
    Id = pd.get_dummies(df["I"], prefix="o", drop_first=True)
    Jd = pd.get_dummies(df["J"], prefix="d", drop_first=True)
    X = pd.concat([df[["tau"]], Id, Jd], axis=1).astype(float)
    return sm.add_constant(X)


def run_gravity(prob, tau, counts=None):
    """Return {'ols','ppml','ppml_counts'} each = (eps_kappa, se, extra)."""
    g = build_gravity_df(prob, tau, counts)
    X = _X(g)
    out = {}
    o = sm.OLS(g["ln_pi"].astype(float), X).fit(cov_type="HC1")
    out["ols"] = (-o.params["tau"], o.bse["tau"], {"r2": o.rsquared, "n": int(o.nobs)})
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        p = GLM(g["pi"].astype(float), X, family=families.Poisson()).fit(
            cov_type="HC1", maxiter=300)
        out["ppml"] = (-p.params["tau"], p.bse["tau"], {"n": int(p.nobs)})
        if counts is not None:
            pc = GLM(g["n"].astype(float), X, family=families.Poisson()).fit(
                cov_type="HC1", maxiter=300)
            out["ppml_counts"] = (-pc.params["tau"], pc.bse["tau"], {"n": int(pc.nobs)})
    return out
