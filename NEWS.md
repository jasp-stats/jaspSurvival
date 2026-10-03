# jaspSurvival Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---
# jaspSurvival 0.97.0
## Added
* Parametric mixture survival analysis: constrained maximum likelihood fitting enforces a minimum standard deviation of log survival time in each component. The default Relative bound is 1% of the unconstrained one-component reference model's log-time SD; Absolute offers an editable 0.1 preset.
* Parametric mixture survival analysis: added an estimation-diagnostics table summarizing starting values, distinct solutions, degeneracy, component sizes, optimizer convergence, and Hessian status.
* Added per-observation exports of residuals and fitted values for Cox and parametric survival models, and posterior component probabilities and classifications for mixture models.
* Added the Parametric Mixture Survival Analysis: finite mixtures of up to four components from the same parametric family estimated with the EM algorithm followed by a direct maximization of the likelihood, with the selection of the distribution and the number of components by AIC/BIC, the mixing probabilities of all components in the coefficients summary, component mean and median and classification tables, and a mixture components plot.
* Parametric mixture survival analysis: models with coinciding, collapsed, or degenerated components are kept for the model comparison and reported with a warning; models with a lower log-likelihood than a nested model with fewer components are reported as local optima; left-truncated (counting) data are estimated by a direct maximization of the likelihood started from the EM solution of the untruncated data.

## Changed
* Parametric prediction and probability plots use adaptive, unrounded time grids; Round steps affects prediction tables. Parametric life-time steps now use the Equal spacing label.
* Nonparametric life-table Quantiles now uses empirical quantiles of observed times, including censored times and frequency-expanded observations, in place of equally spaced times; these are not estimated-survival quantiles.
* Probability plots color empirical points and censoring marks by factor level and retain fitted-curve tails beyond the 0.1%–99.9% display range instead of clamping them to it.

## Fixed
* Survival summaries with case weights sum the weights of censored observations.
* Residual diagnostics handle assigned factors absent from the fitted model.
* Mixture component initialization handles neutral observations right-censored at time zero.
* Parametric survival models consistently include the intercept; incorrect no-intercept formula handling was removed. Previously saved no-intercept analyses can recompute differently.
* Parametric prediction plots: Kaplan–Meier overlays use case weights and the selected prediction confidence level, with survival starting at (0, 1).
* Survival-time prediction sequences no longer drop their last probability when it is below 1; life-time sequences accept From = 0 or an empty From field.
* Survival-data validation rejects infinite non-interval times or weights, reversed intervals, and equal counting-process endpoints; interval bounds allow only correctly signed infinities.
* Kaplan–Meier tests count nonempty groups for degrees of freedom; Cox models match strata by exact variable name, align Schoenfeld residuals with event order, and pass the selected t-frailty degrees of freedom.
* Parametric survival analysis: the best fitting distribution is selected within each subgroup regardless of the "Compare models across distributions" option, and the best fitting model is selected within each distribution when all distributions are displayed.
* Parametric survival analysis: the sequential model comparison compares models only within the same distribution.
* Parametric survival analysis: the coefficients covariance matrix displays the covariances of interaction terms.
* Parametric survival analysis: the subgroup variable accepts nominal variables (the allowed column type was misspelled) and the covariate and factor descriptions no longer refer to the Cox regression model.

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
