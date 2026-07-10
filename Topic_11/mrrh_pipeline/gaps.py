"""
gaps.py — estimate the systematic West-East gaps and build the counterfactual
forcing variables.

gap(x) = mean_West(log x) - mean_East(log x).
  * A_n gap is typically POSITIVE (West more productive).
  * b_n gap is typically NEGATIVE (East higher residual amenity).

"Eliminate the systematic gap" = move East fundamentals to the West mean, i.e.
multiply the East entries by exp(gap):
  * aChange:  Nx1,  aChange[east] = exp(gapA),           else 1.
  * bChange:  NxN,  bChange[n in east, :] = exp(gapB),   else 1   (residence-row wise,
              since QoL is residence-specific b_n, constant across workplace i).

kapChange = dChange = ones (commuting and trade costs unchanged).
"""
from __future__ import annotations
import numpy as np


def log_gap(x, east):
    lx = np.log(x)
    return float(lx[~east].mean() - lx[east].mean())


def summary(x, east):
    lx = np.log(x)
    return dict(west=float(lx[~east].mean()), east=float(lx[east].mean()),
                gap=float(lx[~east].mean() - lx[east].mean()),
                lo=float(lx.min()), hi=float(lx.max()))


def build_forcings(gapA, gapB, east, N, which):
    """which in {'prod', 'qol', 'both'}. Returns (aChange, bChange, kapChange, dChange)."""
    aChange = np.ones(N)
    bChange = np.ones((N, N))
    kapChange = np.ones((N, N))
    dChange = np.ones((N, N))
    if which in ("prod", "both"):
        aChange[east] = np.exp(gapA)
    if which in ("qol", "both"):
        bChange[east, :] = np.exp(gapB)
    return aChange, bChange, kapChange, dChange