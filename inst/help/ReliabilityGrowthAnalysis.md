# Reliability Growth Analysis

Fit Duane, Crow–AMSAA (power-law NHPP), log-linear NHPP, or piecewise power-law NHPP models using ReliaGrowR.

## Data

Each row describes one observation interval. Assign **Time**, and choose cumulative interval endpoints or interval durations. Each independent sequence starts at zero. Keep rows in observation order. Assign **Failure Count** for counts within each interval; leaving it empty means one failure per interval. Counts are not cumulative.

The current ReliaGrowR engine requires positive counts in every interval. Zero-count intervals contain exposure information and must not be dropped to bypass this limitation. Missing interval records are not omitted automatically.

Use **Subgroup** for independent test sequences. A subgroup fit is not covariate regression. Do not split one sequence by changing operating conditions.

## Models and estimation

Duane fits log cumulative MTBF against log time. Crow–AMSAA fits a power-law cumulative mean, M(t) = lambda × t^beta. Log-linear NHPP fits intensity m(t) = exp(a + b × t). Piecewise NHPP fits a segmented log–log regression. Starting change points initialize estimated locations; they are not fixed boundaries.

Maximum likelihood uses grouped interval counts, not an exact-event-time likelihood. MLE time is scaled internally and results are reported in the original units. Fits reaching known numerical bounds are rejected. ReliaGrowR does not return optimizer convergence status. Regression and NHPP likelihood fit statistics should not be compared directly.

## Predictions

Choose **Outcome**, evaluation times under **Time**, and plot/table **Output**. No predictor assignments are needed. These controls follow the parametric survival Predictions layout.

**Equal intervals** spans the first observed endpoint to maximum follow-up. **Sequence** provides From, Step and To fields; extend To beyond observed follow-up to forecast. **Custom (relative)** accepts percentages of each subgroup's maximum follow-up, including percentages above 100 for forecasting. **Custom (absolute)** accepts times in the original units; empty input uses 25%, 50% and 75% of follow-up. Evaluation times must be positive. A dashed vertical line marks the observed endpoint when forecasting; dotted lines mark estimated change points.

- **Survival probability:** event-free survival exp(-M(t)), the probability of no events between time zero and t under an NHPP.
- **Failure probability:** 1 - exp(-M(t)), the probability of at least one event between time zero and t under an NHPP.
- **Cumulative failures:** expected cumulative failures M(t).
- **Failure intensity:** instantaneous failure rate m(t).
- **Cumulative MTBF:** t / M(t).
- **Instantaneous MTBF:** 1 / m(t), a local rate-based measure rather than expected waiting time to the next failure.

Failure intensity and cumulative failures are the recurrent-event analogues of hazard and cumulative hazard in parametric survival analysis. The latter is an expected count, not a failure probability. Neither MTBF outcome equals mean survival time.

The probability outcomes transform ReliaGrowR's fitted cumulative mean; no additional model is fitted. Survival and failure probabilities sum to one. Their confidence intervals transform the cumulative-mean bounds, reversing their order for survival. For Duane, this interpretation additionally assumes its fitted growth curve is the cumulative mean of an NHPP. These are probabilities since time zero, not conditional probabilities of the next event after an arbitrary starting time.

In medical applications, survival probability means remaining event-free for the specified event. These outputs do not add patient-level exposure or censoring support. The interval-count input requirements and restriction on zero-count intervals still apply; use a suitable patient-level survival analysis for individually censored time-to-event data.

**Display** controls confidence intervals, legend, theme, observed data and colors. Axis controls select linear or logarithmic scales. **Merge series across: Subgroups** overlays independent subgroup curves.

Tables support **Long** (times in rows) and **Wide** (times in column groups) formats. **Merge tables across measures** places selected outcomes together. Optional **Round time table values** rounds evaluation times before calculating table predictions; rounded times must remain positive. Plot evaluation times remain unrounded.

Confidence intervals describe uncertainty in fitted quantities, not future count variability. Rate-based intervals are unavailable for piecewise models. Uncertainty is unavailable when covariance estimation is unreliable.

## Diagnostics and export

Least-squares residuals use the regression's logarithmic scale. MLE residuals are Pearson residuals for interval counts. Exports preserve the original dataset row order; failed subgroup fits produce missing exported values.

## Reference

[ReliaGrowR: Reliability Growth Analysis and Repairable Systems Modeling](https://doi.org/10.32614/CRAN.package.ReliaGrowR).

[NIST: NHPP power-law model and count probabilities](https://itl.nist.gov/div898/handbook/apr/section1/apr172.htm).
