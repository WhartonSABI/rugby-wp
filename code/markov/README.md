# Markov manuscript revision analysis

Run from the `rugby-wp` repository root:

```sh
Rscript --vanilla tests/test-markov.R
Rscript --vanilla code/markov/run_sim.R
```

The runner retains its historical filename but now computes predictions by
solving absorption equations. Its default output is
`output/markov-revision-20261113/`; pass another directory as the first argument.
It rebuilds the model from raw data instead of reading the legacy transition
cache. The runner does not overwrite the paper source or its figures.
The scripts under `code/ml/` are historical experiments; the revision's common
comparison is implemented in `code/helpers/comparators.R`.

## Model and corrected bookkeeping

`Points_Difference` is already relative to the possession team. For consecutive
observations in each match (ordered by ID), convert it to the home perspective
before differencing. This identifies the signed points scored between phases.
The final record's signed next-score label supplies any score after that last
observation and before full time. Replaying the reconstructed transitions must
reproduce every observed score margin and final match result.

Each empirical transition is a JOINT outcome: signed points relative to the
current possession team, whether possession switches, next time bucket, next
field zone, and half/full-time indicators. Sampling these fields together
preserves their dependence. In particular, scoring does not automatically
award points to the current possession team or automatically flip possession.

The estimation key has 336 possible time/field/score-bucket combinations.
The complete evolving state additionally retains exact score margin and
possession identity. The score bucket is always recomputed from that state.
There is no independently sampled successor score bucket.

Observed conditioning rows use empirical frequencies. For an unobserved row,
back off to the training distribution at the same time and field position,
then to the same time bucket if needed. This is an explicit modeling choice,
not evidence of reliable estimation in sparse cells. `state_support.csv`
records support and fallback level for every key. No holdout observations
contribute to the fitted kernel or fallback distributions.

## Direct probabilities and optional simulation

For the expanded state space, Q contains transient-to-transient probabilities
and R contains probabilities of termination in a home win, draw, or away win.
Solve `(I - Q) B = R`. Final-margin distributions use the same system with
terminal margin categories. Next-score probabilities use a separate absorbing
system that stops at the first score or the selected half/match boundary.
Include all signed outcomes `-7, -5, -3, 0, 3, 5, 7` when calculating EP.

Because the outer score buckets are unbounded, the numerical calculation uses
an expanding exact-margin grid. Exits from that grid are a separate unresolved
outcome, never clamped to a win or loss. The runner enlarges the grid until the
largest unresolved probability at the evaluation queries is below `1e-8`.
This bounds each reported absorption probability's grid error. Linear-system
residuals and probability sums are checked as well. Numerical precision is
conditional on the estimated model; it does not measure statistical uncertainty.

Simulation remains available for example paths and numerical cross-checks.
Hitting its safety cap raises an error; it cannot create a completed-match
result. The direct probability calculations have no trajectory-length cutoff.

## Evaluation

- Split the first 105 matches in recorded ID order for training and the last
  27 for evaluation. This reproduces the original split rule; there are no
  match-date fields in the raw CSV to independently establish calendar order.
- Estimate the kernel using all training phases. Select first phases only
  AFTER reconstructing transitions. Do not remove evaluation rows based on
  their future terminal event.
- Evaluate every model on the same 2,479 first phases, with both team
  perspectives (4,958 rows). The outcome is 1 for wins, 0.5 for draws, and 0
  for losses. Markov predictions are `P(win) + 0.5 P(draw)`. Outright win/draw/
  loss probabilities are also saved separately.
- Logistic regression uses a logit mean model; the forest uses `ranger` with
  fixed settings. XGBoost stopping rounds are chosen on the last portion of
  training games, then the model is refitted on all 105 training games.
  The 27 evaluation games are never used for early stopping.
- The constant home/away baseline is estimated from training rows only.
- Resample the 27 held-out games jointly across models for 2,000 bootstrap
  replicates. The resulting intervals quantify test-sample uncertainty with
  fitted models held fixed, not uncertainty from refitting the transition model.

All metrics, predictions with identifiers, split assignments, calibration
summaries, numerical diagnostics, examples, figures, and R package versions
are written to the output directory.

## Source discrepancy and modeling limitations

The score-change direction at raw ID 28406 disagrees with its next-score
outcome label. For nonterminal transitions the implementation uses the observed
scoreboard change and exports the discrepancy for review. The raw CSV is not
modified. Small within-game clock reversals do not cross these coarse time
buckets; transitions are kept in ID order rather than reordered by the clock.

There are 281 observed conditioning rows in training: 35 have fewer than five
transitions. The other 55 use fallback distributions. Some near-boundary tied
states produce extreme EP estimates from one or two observations. These need
explicit discussion and a pooling/smoothing sensitivity analysis before making
strong claims about state-specific EP. No smoothing parameter has been selected
using the held-out results.

The 36-minute middle-half buckets still discard exact clock time. This is an
approximate state representation, and direct computation does not cure that
approximation. The current model does not estimate clock-time-to-next-score,
team-strength effects, or path-dependent momentum effects.

## Published EP reference and manuscript work

Use the version of record of Watts and Pipping-Gamón (2026), *Journal of
Quantitative Analysis in Sports*, 22(4), 407-421:
https://doi.org/10.1515/jqas-2025-0183
Publisher PDF: https://www.degruyterbrill.com/document/doi/10.1515/jqas-2025-0183/pdf

The published paper's next-score EP model conditions on lineout-start
possessions and includes all seven signed outcomes. This Markov kernel pools
phase types. Cite the publication for related methodology; numerical agreement
requires aligned starting conditions and scoring horizons. The separate
`rugby-ep` working tree is not a reference for this revision.

The revised manuscript, *A Markov Chain Approach to Win Probability in Rugby
Union*, is in `paper/main.tex` with its bibliography and six figure panels.
The revision was submitted to [arXiv](https://arxiv.org/abs/2610.04813) on
November 13, 2026. To build the manuscript with a LaTeX installation:

```sh
cd paper
latexmk -pdf main.tex
```

After running the analysis, refresh the approved paper figures with:

```sh
Rscript --vanilla code/markov/export-paper-figures.R
```

This updates the two calibration panels, two final-margin panels, next-score
probabilities, and EP figures, and saves training calibration data. The EP
illustration uses a tied score in the first-half middle bucket and stops at the
next score or half-time.
