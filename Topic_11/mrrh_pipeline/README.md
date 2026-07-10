# MRRH2018 / SW2020 pipeline — Topic 11, Task 1

Python translation of the `MRRH2018-toolkit` MATLAB code that produces all
results for **Task 1**: illustrating the general-equilibrium effects of
eliminating the systematic **West–East Germany gap in productivity and quality of
life**, on the Seidel & Wickerath (2020) county data, using the **Tutorial-9
time-cost matrices**.

## Run

```bash
# from the parent of this folder (…/Topic_11)
python -m mrrh_pipeline.run_task1
```

Outputs go to `mrrh_pipeline/figs/` (PNG) and `mrrh_pipeline/output/`
(CSV / LaTeX / NPZ). Every written file is printed with its full path.

Paths default to the repository layout (`config.py`); override with
`MRRH_DATA_DIR`, `MRRH_TUT9_DIR`, `MRRH_SHAPE_DIR`, `MRRH_OUT_DIR`, `MRRH_FIG_DIR`
to run outside the repo.

## Modules

| file | role |
|------|------|
| `config.py` | structural parameters, paths, analysis knobs |
| `dataio.py` | load + align all inputs to the 400 shared AGS counties |
| `quantify.py` | (a) productivity/trade-share inversion; (b) quality-of-life residual |
| `counterfac.py` | (c) exact-hat-algebra solver (all `updateXxxTK` + `counterFactsTK`) |
| `gaps.py` | West–East gap estimation + counterfactual forcing matrices |
| `mapping.py` | Beamer-styled county choropleths (Jenks, state overlay) |
| `plots.py` | border-distance scatters + West/East aggregate bars |
| `run_task1.py` | orchestrator |

## Method notes

- **Sample.** The Tutorial-9 matrices cover 400 counties; the toolkit data cover
  401. The single missing AGS is **16056 Eisenach** (merged into Wartburgkreis
  post-2021, absent from the newer road network). The whole analysis is restricted
  to the 400 shared counties, keyed by AGS. Eisenach appears as one grey county on
  the maps.
- **East definition.** `East ≡ AGS state code ≥ 11` (Berlin included). On the full
  401-sample this reproduces the toolkit's positional `East(325:end)` split exactly
  and is robust to dropping counties.
- **Trade cost.** `dni` = Tutorial-9 `tcmatr_de.csv` (diagonal 1), used directly in
  place of the toolkit's distance-based trade cost.
- **(a) Productivity.** `solveProductTradeTK` — iterate `A_n` to the
  income = expenditure condition (SW2020 eq. 12); recover trade shares (eq. 10) and
  the tradable price index `P_n`.
- **(b) Quality of life.** ARSW2015-style residual from the unconditional
  residence-choice probability:
  `b_n ∝ (R_n/L)·(P_n^α Q_n^(1-α))^ε / CMA_n`, with commuter market access
  `CMA_n = Σ_i τ_ni^(-εμ) w_i^ε`. `Q_n` is the observed `rentindex`; `τ` is the
  Tutorial-9 travel time. Its zero diagonal is imputed with an area-based own-time
  `τ_nn = (2/3)·√(Area_n/π)/speed·60` min (default 30 km/h, floor 3 min); this
  convention is a swap-able knob in `config.py`. `b_n` is geomean-normalised.
- **(c) Counterfactuals.** "Remove the gap" = move **East** fundamentals to the
  **West** mean, i.e. multiply East entries by `exp(gap)`, with
  `gap = mean_West(log x) − mean_East(log x)`. Productivity is an N-vector change;
  quality of life is applied residence-row-wise to the NxN amenity change matrix.
  Commuting and trade costs are unchanged. Three experiments: (i) productivity,
  (ii) quality of life, (iii) both.

## Baseline findings (Tutorial-9 matrices, default knobs)

- West−East mean **log productivity** gap ≈ **+0.270** (West ≈ 31 % more productive).
- West−East mean **log quality of life** gap ≈ **−1.09** (East higher residual
  amenity — the model rationalises population persistence in the less-productive
  East via amenities; magnitude is sensitive to the own-time convention).
- Welfare change (common scalar Ū): (i) **+5.5 %**, (ii) **−2.8 %**, (iii) **−0.9 %**.
  Note the asymmetry: equalising to the West *raises* East productivity (welfare up)
  but *lowers* East amenity (welfare down). Population is conserved by construction
  (`ΣL′/ΣL = ΣR′/ΣR = 1`).

The welfare-planner scalar treats the amenity experiment as a pure destruction of
East amenity; read (ii) as the GE cost of removing the *measured* amenity advantage,
not as a policy recommendation.