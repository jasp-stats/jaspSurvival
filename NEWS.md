# jaspSurvival Changelog

# jaspSurvival 0.98.1
## Added
* Recurrent event analysis

## Fixed
* Parametric models retain standard errors and confidence intervals when only one parameter is estimated.
* Prediction figures precede tables; outcomes follow the interface order.

# jaspSurvival 0.98.0
## Added
* Flexible prediction plots, including contours and heatmaps, and Long or Wide prediction tables.
* Population-averaged predictions and predictions from manual input, CSV files and dataset rows, with export.
* Time ratios, acceleration factors and prediction intervals for survival time.
* Predictor transformations, temperature models, factor contrasts and reference levels.
* Ancillary parameter regressions, fixed parameters and fitted model equations.
* Density diagnostics, stratified probability plots and one-sided confidence intervals.

## Changed
* Improved estimation and uncertainty estimates for parametric and mixture survival models.
* Mixture component plots show each component's survival measures; density curves show weighted contributions.

## Fixed
* Counting-process response residuals use the exit time consistently in plots and exports.
* Missing values in unused predictors no longer exclude observations.
* Saved analyses retain Kaplan-Meier overlays and censoring marks when upgraded.
* Faster frequency-weighted analyses and population-averaged survival percentiles.

# jaspSurvival 0.97.0
## Added
* Added the Parametric Mixture Survival Analysis
* Parametric: subgroup analysis with multiple factors.
* Prediction and Probability plots offer 'Merge plots across' 'Factors' and 'Subgroups'
* Added per-observation exports of residuals and fitted values for Cox and parametric survival models, and posterior component probabilities and classifications for mixture models.

## Changed
* Increased the default number of samples for CI calculation to 10,000 (and added the corresponding option)
* Non-Parametric: life-table 'Quantiles' steps use empirical quantiles.

## Fixed
* Parametric survival models consistently include the intercept.
* Stability of the prediction plots
* Semi-Parametric: degrees of freedom of the t frailty distribution.
* Semi-Parametric: Schoenfeld and scaled Schoenfeld residual plots use the correct observation order.
* Semi-Parametric: exact matching of strata in model terms.
* Non-Parametric: test degrees of freedom when groups have no expected events.
* Censoring summary updates when predictors change.
* Parametric: empty model-label footnotes.

# jaspSurvival 0.96.7
## Fixed
* Fixed confidence intervals for failure-probability predictions so lower and upper bounds are correctly swapped when transforming from survival probabilities.
* Fixed undefined `nullPredictors` references in Cox estimates and hazard-ratio table error handling; align null-model footnotes with the summary table.
* Fixed undefined `tempTable` in parametric life-time table merge error handling by stopping with the intended message (caught by the existing `try()`).

# jaspSurvival 0.96.6
## Fixed
* Improved y-axis label and grid spacing for the exponential canvas in parametric survival probability plots.

# jaspSurvival 0.96.5
## Added
* Added an exponential canvas to the parametric survival probability plot.
* Added optional censoring marks to the parametric survival probability plot.

## Changed
* Enabled prediction and probability-plot legend/color-palette controls only when the plot can display multiple fitted curves, while keeping theme controls available for selected plots.
* Increased probability plot width when a side legend is shown.
* Reduced probability plot axis text and title sizing by 10% for the JASP theme to avoid label overlap.

# jaspSurvival 0.96.4
## Features
* Added probability plot to parametric survival

# jaspSurvival 0.96.3
## Changed
* Added unit tests
* Updated README
