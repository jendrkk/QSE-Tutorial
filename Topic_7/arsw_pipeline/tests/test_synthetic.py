"""
tests/test_synthetic.py — validate the inversion numerics on a synthetic economy with
KNOWN fundamentals, independently of the (large) real data.

Run:  cd Topic_7/arsw_pipeline && python -m tests.test_synthetic
Expects: wage & amenity recovery correlation ≈ 1.0 (max |Δlog| < 1e-5); exact ε recovery.
"""
from __future__ import annotations
import sys
from pathlib import Path
import numpy as np
from scipy.stats import gmean

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import core


def _make_economy(N=60, alpha=0.80, beta=0.75, epsilon=6.83, kappa=0.07 / 6.83, seed=0):
    rng = np.random.default_rng(seed)
    xy = rng.uniform(0, 20, (N, 2))
    tau = np.sqrt(((xy[:, None, :] - xy[None, :, :]) ** 2).sum(-1))
    np.fill_diagonal(tau, 0.5)
    w_true = np.exp(rng.normal(0, 0.3, N)); w_true /= gmean(w_true)
    B_true = np.exp(rng.normal(0, 0.4, N))
    Q = np.exp(rng.normal(0, 0.3, N))
    K = rng.uniform(0.5, 3, N)
    H = 100_000.0
    d = np.exp(-epsilon * kappa * tau)
    phi = (d * Q[:, None] ** (-(1 - beta) * epsilon)
             * B_true[:, None] ** epsilon * w_true[None, :] ** epsilon)
    Phi = phi.sum()
    HR = phi.sum(axis=1) / Phi * H
    HM = phi.sum(axis=0) / Phi * H
    obs = np.column_stack([Q, HM, HR, K])
    return dict(obs=obs, tau=tau, w_true=w_true, B_true=B_true,
                alpha=alpha, beta=beta, epsilon=epsilon, kappa=kappa, N=N, H=H)


def test_inversion():
    e = _make_economy()
    N, a, b, eps, k = e["N"], e["alpha"], e["beta"], e["epsilon"], e["kappa"]
    obs, tau = e["obs"], e["tau"]
    init = np.zeros(N)
    Iw = obs[:, 1] != 0
    init[Iw] = ((1 - a) / obs[Iw, 0]) ** ((1 - a) / a) * a
    w, A, conv, HMC, gap = core.solve_omega(obs, tau, N, init, mode="C",
                                            alpha=a, beta=b, epsilon=eps, kappa=k)
    assert conv and gap == 0.0
    assert np.corrcoef(np.log(w), np.log(e["w_true"]))[0, 1] > 0.999999
    assert np.abs(np.log(w) - np.log(e["w_true"])).max() < 1e-5
    B, CMA, HRS = core.recover_amenities(obs, tau, N, w, alpha=a, beta=b, kappa=k, epsilon=eps)
    lb, lt = np.log(B), np.log(e["B_true"])
    lb -= lb.mean(); lt -= lt.mean()
    assert np.corrcoef(lb, lt)[0, 1] > 0.999999
    assert np.abs(lb - lt).max() < 1e-5
    vv = core.compute_expected_income(obs, tau, N, w, B, alpha=a, beta=b, kappa=k, epsilon=eps)
    A2, B2, w2 = core.adjust_levels(obs, tau, N, A, B, alpha=a, beta=b, kappa=k, epsilon=eps)
    V, L, th = core.compute_density(obs, A2, w2, vv, N, alpha=a, beta=b)
    assert np.all(vv > 0) and np.all((th >= 0) & (th <= 1))
    assert abs(gmean(A2[A2 > 0]) - 1) < 1e-9
    print("test_inversion PASSED (wages & amenities recovered to machine precision)")


def test_epsilon():
    rng = np.random.default_rng(2)
    Nn = 400
    omega = np.exp(rng.normal(0, 0.8, Nn)); omega /= gmean(omega)
    HM = rng.uniform(1, 500, Nn)
    bzk = rng.integers(1, 13, Nn)
    for eps0 in (4.0, 6.83, 11.5):
        vd = core.bezirk_logwage_var(omega.copy(), HM, bzk, eps0)
        eh = core.estimate_epsilon(omega, HM, bzk, vd)
        assert abs(eh - eps0) <= 0.01, (eps0, eh)
    vs = [core.bezirk_logwage_var(omega.copy(), HM, bzk, ee) for ee in (3, 5, 8, 12, 20)]
    assert all(np.diff(vs) < 0)
    print("test_epsilon PASSED (exact recovery; moment monotone in ε)")


if __name__ == "__main__":
    test_inversion()
    test_epsilon()
    print("ALL SELF-TESTS PASSED")
