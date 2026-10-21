# Recurrent Event Analysis

Fit Duane, Crow–AMSAA (power-law NHPP), log-linear NHPP, or piecewise power-law NHPP models using ReliaGrowR.

## Data

Each row describes one observation interval. Assign **Time**, and choose cumulative interval endpoints or interval durations. Each independent sequence starts at zero. Keep rows in observation order. Assign **Event Count** for counts within each interval; leaving it empty means one event per interval. Counts are not cumulative.

The current ReliaGrowR engine requires positive counts in every interval. Zero-count intervals contain exposure information and must not be dropped to bypass this limitation. Missing interval records are not omitted automatically.

Use **Subgroup** for independent test sequences. A subgroup fit is not covariate regression. Do not split one sequence by changing operating conditions.

## Models and estimation

Duane fits log cumulative mean time between events against log time. Crow–AMSAA fits a power-law cumulative mean, M(t) = lambda × t^beta. Log-linear NHPP fits intensity m(t) = exp(a + b × t). Piecewise NHPP fits a segmented log–log regression. Starting change points initialize estimated locations; they are not fixed boundaries.

Maximum likelihood uses grouped interval counts, not an exact-event-time likelihood. MLE time is scaled internally and results are reported in the original units. Fits reaching known numerical bounds are rejected. Fit warnings and errors appear as notes to **Model summary**.

The **Estimation** selector is enabled for Crow–AMSAA, which offers maximum likelihood and least squares. Other models display their single available method in a disabled selector.

Under **Advanced**, **Model Options** appears for the piecewise model and contains **Starting change points** with its nested **Times** field. **Reproducibility** contains **Set seed**, enabled for piecewise fitting or selected randomized quantile diagnostics and exports. These groups follow the two-column Advanced layout used in parametric survival.

## Statistics

Statistics uses the same interface component as parametric survival. **Model summary** and **Data summary** appear on the left; **Coefficients**, their nested confidence-interval control, and **Coefficients covariance matrix** appear on the right. **Fitted model equation** retains the same decimal-place control. Model ranking, information-criterion weights and sequential comparisons are omitted because this analysis fits one selected model independently to each subgroup.

Output follows the parametric order: data summary, model summary, coefficient estimates, covariance matrices and fitted equations. Coefficient tables show estimates, standard errors and optional two-sided confidence intervals, without predictor Wald tests. Repeated subgroup labels are suppressed within each coefficient or equation block. Covariance has one table per subgroup, with a single table when only one fit is available.

**Model summary** shows log-likelihood, parameter count, AIC and BIC. The model and estimation method are specified in the settings. Fit notices identify the affected subgroup; failed fits have blank statistics. If a fit notice needs reporting, the summary is shown even when its checkbox is unchecked. AIC and BIC use the same three-decimal formatting as parametric survival. Covariance matrices use the reported coefficient scale, as stated in their footnote; the parametric survival covariance convention can differ.

Fitted equations identify the selected model and its cumulative mean, intensity or cumulative mean time between events. The shared equation formatter uses scientific notation when a nonzero coefficient would otherwise round to zero. Decimal-place settings affect equation display only, not fitted estimates or predictions.

For piecewise models, **Segment estimates** reports ReliaGrowR's returned segment slopes (beta), their standard errors, growth rates and scales (lambda). Growth rates are the package's reported values, 1 - beta. **Change points** shows estimated times in the original **Time** units. Its standard-error column is explicitly on the native log-time scale; it is not a standard error in time units. These tables do not add confidence intervals or refit the model. Standard errors are omitted when fit inference is unreliable. The regression coefficients and covariance matrix retain their existing parameterization.

## Predictions

Under **Advanced**, **Confidence interval > Bounds** selects **Two-sided**, **Lower**, or **Upper**, as in parametric survival. This applies to coefficient intervals, prediction intervals and probability-plot reference bounds. One-sided tables show the selected bound, and plots show it as a dashed curve. Two-sided intervals remain the default. Each output retains its own confidence level.

Choose **Outcome**, evaluation times under **Time**, and plot/table **Output**. No predictor assignments are needed. These controls follow the parametric survival Predictions layout.

**Equal intervals** spans the first observed endpoint to maximum follow-up. **Sequence** provides From, Step and To fields; extend To beyond observed follow-up to forecast. **Custom (relative)** accepts percentages of each subgroup's maximum follow-up, including percentages above 100 for forecasting. **Custom (absolute)** accepts times in the original units; empty input uses 25%, 50% and 75% of follow-up. Evaluation times must be positive. A dashed vertical line marks the observed endpoint when forecasting; dotted lines mark estimated change points.

- **Survival probability:** event-free survival exp(-M(t)), the probability of no events between time zero and t under an NHPP.
- **Failure probability:** 1 - exp(-M(t)), the probability of at least one event between time zero and t under an NHPP.
- **Cumulative events:** expected cumulative events M(t).
- **Event intensity:** instantaneous event rate m(t).
- **Cumulative mean time between events:** t / M(t).
- **Instantaneous mean time between events:** 1 / m(t), a local rate-based measure rather than expected waiting time to the next event.

Event intensity and cumulative events are the recurrent-event analogues of hazard and cumulative hazard in parametric survival analysis. The latter is an expected count, not a failure probability. These time-between-events outcomes should not generally be interpreted as mean survival time.

The probability outcomes transform ReliaGrowR's fitted cumulative mean; no additional model is fitted. Survival and failure probabilities sum to one. Their confidence intervals transform the cumulative-mean bounds, reversing their order for survival. For Duane, this interpretation additionally assumes its fitted growth curve is the cumulative mean of an NHPP. These are probabilities since time zero, not conditional probabilities of the next event after an arbitrary starting time.

In medical applications, survival probability means remaining event-free for the specified event. These outputs do not add patient-level exposure or censoring support. The interval-count input requirements and restriction on zero-count intervals still apply; use a suitable patient-level survival analysis for individually censored time-to-event data.

**Display** controls confidence intervals, legend, theme, observed data and colors. Axis controls select linear or logarithmic scales. **Merge series across: Subgroups** overlays independent subgroup curves.

Tables support **Long** (times in rows) and **Wide** (times in column groups) formats. **Merge tables across measures** places selected outcomes together. Optional **Round time table values** rounds evaluation times before calculating table predictions; rounded times must remain positive. Plot evaluation times remain unrounded.

Confidence intervals describe uncertainty in fitted quantities, not future event-count variability. Rate-based intervals are unavailable for piecewise models. Uncertainty is unavailable when covariance estimation is unreliable.

### Nonparametric estimate (MCF): prerequisite

The proposed **Nonparametric estimate** display option is deferred. `ReliaGrowR::mcf()` requires unit identifiers, exact event/censoring times with binary event indicators, and observation endpoints for each unit. Current **Time** and **Event Count** describe observation intervals; even a count of one does not establish an exact event time. **Subgroup** selects separate fitted sequences and does not supply unit histories within a subgroup.

Before adding the overlay, the input interface must support exact-event records, unit IDs distinct from analysis subgroups, censoring or explicit follow-up endpoints, and units with no events. Observation windows must match the native function's common time-origin assumption, and the fitted curve and MCF must refer to the same per-unit scale. Those records can then be passed directly to `mcf()` and its returned estimates plotted. Grouped counts must never be expanded into invented event times, nor interval endpoints silently treated as event times. The existing **Observed data** overlay remains a cumulative count, not a nonparametric MCF.

## Diagnostics

The Diagnostics layout follows parametric survival: **Residual Plots** on the left and **Probability plot** on the right. Numerical fitted values and residuals are available under **Export**, rather than as a diagnostic table.

Choose residuals versus **time**, residuals versus **predicted**, or a residual **histogram**, with one plot per fitted subgroup. The **Type** selector offers:

- **Regression:** the package's least-squares regression residuals, on the log cumulative mean time between events scale for Duane and log cumulative events scale for Crow–AMSAA and piecewise fits. Available for least squares only.
- **Response:** observed minus fitted interval events.
- **Pearson:** response residual divided by the square root of the fitted interval mean.
- **Deviance:** signed square root of the Poisson interval deviance.
- **Randomized quantile:** standard normal quantiles obtained by randomizing within each observed count's Poisson probability jump.

Count residuals assume an NHPP, including an NHPP interpretation of the Duane growth curve. Their fitted interval means are differences of the package's fitted cumulative events. Residuals versus predicted uses fitted log regression values for Regression, and fitted interval counts for the other types. Residual plots use the same JASP construction as parametric survival, independently of prediction-plot settings.

### Probability plots

For grouped interval counts, **Normal (Q-Q)** compares ordered randomized quantile residuals with standard normal quantiles; **Uniform (P-P)** compares their cumulative probabilities with uniform plotting positions. Under the fitted NHPP they should approximately follow the identity line. One point represents one observation interval; counts are not expanded into invented event times. These plots apply to all four models under their NHPP interpretation.

These are count-model diagnostics, not Weibull or other lifetime probability-paper plots. The native ReliaGrowR Q-Q/P-P helpers do not provide the required grouped-count diagnostic, so these plots wrap the package's fitted interval means with the discrete probability-integral transform of Dunn and Smyth (1996).

Native `gof()` statistics are not offered: their event-time transformation is not a goodness-of-fit test for the grouped interval-count input used here. The grouped-count residual and probability-plot diagnostics are retained.

Controls mirror the applicable parametric probability-plot controls: empirical points and coordinates, reference line, subgroup overlays, plotting positions, grid, legend, palette and theme. A **Reference envelope** shows exploratory pointwise order-statistic bounds for the reference distribution, excluding uncertainty in the fitted parameters.

Randomized quantile residuals are cached and shared by residual plots, probability plots and exports. Visual changes do not redraw the randomization. Use **Set seed** under **Advanced** to reproduce it across runs.

## Export

The column prefix appears above **Residuals** and **Fitted Values**, as in parametric survival. Select each desired residual type separately. Fitted cumulative events and fitted interval events can also be exported. For example, prefix `RecurrentEvents` produces `RecurrentEvents_PearsonResidual`, `RecurrentEvents_QuantileResidual`, `RecurrentEvents_CumulativeEvents` and `RecurrentEvents_IntervalEvents`.

Exports preserve the original dataset row order, including interleaved subgroups. Failed subgroup fits or unavailable values produce missing entries; export calculation problems are reported. Regression residuals are unavailable for maximum likelihood. Count diagnostics require finite positive fitted interval means. Existing columns owned by another analysis or supplied by the user are not overwritten.

## Reference

[ReliaGrowR: Reliability Growth Analysis and Repairable Systems Modeling](https://doi.org/10.32614/CRAN.package.ReliaGrowR).

[NIST: NHPP power-law model and count probabilities](https://itl.nist.gov/div898/handbook/apr/section1/apr172.htm).

[Dunn and Smyth (1996): Randomized Quantile Residuals](https://gksmyth.github.io/pubs/residual.pdf).
