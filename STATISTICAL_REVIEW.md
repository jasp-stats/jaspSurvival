# Statistical review of AFT improvements

Reviewed 12 October 2026: `AFT-improvements`, code at `a17d4ca`.
Comparisons: split point `236ee6c` and [Mixture PR #25](https://github.com/jasp-stats/jaspSurvival/pull/25),
head `c9731d4269f3a77cbc0ea97c0a6afaa54775f52e`.
Checks used R 4.5.2, flexsurv 2.3.2 and survival 3.8.6. Linked online documentation
may describe newer package versions; numerical comparisons used the installed versions.

The checked model definitions and main formulas for independent observations are
sound. This is not a blanket claim about convergence, identification or interval
coverage for every dataset.
The confidence-draw follow-up below corrects missing evaluation handling; several deliberate conventions
need to be understood when comparing results with other software. This review changes
no estimator or default; the subsequent confidence-draw correction is documented below.

User follow-up, 12 October 2026: retain interval-tail behavior as an upstream
flexsurv limitation and leave the minimum-spread policy unchanged. These are
excluded from follow-up implementation. Prediction intervals and candidate
filtering are also retained. Frequency expansion for both parametric analyses
was explicitly requested. The subsequent user concern about selective omission
also prompted the confidence-draw correction; other items remain discussion points.

## Findings and follow-up decisions

| Issue | Implementation and evidence | Recommended action |
| --- | --- | --- |
| Failed simulation draws — omission corrected after review | Previously `.sapDrawBounds()` discarded missing evaluations. It now retains infinite draws and withholds bounds for each prediction with any unresolved draw. Mixture percentiles exceeding the largest representable time return `Inf` after a survival-probability comparison at that limit. Prediction notes report failed evaluation counts and infinite bounds, including relative-lifetime outputs. | Implemented following the user's bias concern. Unresolved draws are not reclassified as infinite without numerical evidence. Ordinary finite type-7 bounds are unchanged. This prevents selective omission; it does not guarantee nominal coverage of asymptotic mixture inference. |
| Tail interval probabilities can round to zero | Inherited `.sapmComponentLogLikelihood()` subtracts ordinary CDFs. For an exponential rate of 1 and interval (40, 41], the code gives `-Inf`; the correct log probability is `-40.4586751453871`. Native flexsurv 2.3.2 also fails at these fixed parameters; a fixed equal-rate mixture returns `-Inf`. This is an inherited/upstream numerical limitation affecting fitting, initialization and classification. | User decision: retain as an upstream limitation; no implementation change requested. A future upstream fix would need stable probabilities in fitting and membership calculations. |
| Minimum spread is only a baseline bound | The inherited bound is combined with new ancillary regression. `.sapmConstraintBounds()` bounds baseline shape/scale coefficients, not their covariate-dependent values. A log-normal example passes a baseline bound of 0.5 with SDs 0.76315 at x=0 and 0.083417 at x=1. The results note correctly says baseline; GUI help is broader. | User decision: leave this policy unchanged; no follow-up implementation requested. |
| Frequency expansion — implemented after review | Earlier automatic bases used compressed rows. For x=0,...,9 and weights (8,5,1,...,1), spline knots were (3,6), versus approximately (0.29167,2.33333) for repeated rows. Both parametric analyses now expand frequency units before bases, fitting and diagnostics. | Implemented per user direction. Raw frequency values remain available as predictors/subgroups, while internal fitting weights are one. Exports collapse replicas back to original rows. Automatic bases and fixed transformed coefficients now use the literal repeated-unit convention. |
| Counting input needs a clear scope | Inherited row-wise mixture likelihood supports independent delayed entry. It differs from a persistent latent-class likelihood for multiple intervals from one subject when predictors change. The independently checked two-interval example is detailed below. | State the supported delayed-entry scope. Longitudinal mixture support requires subject IDs and class-specific covariate histories. |

Locations: [bounds](R/parametricsurvival-bounds.R),
[component likelihood](R/parametricmixturesurvival-analysis.R),
[constraints](R/parametricmixturesurvival-constraints.R),
[transformations](R/parametricsurvival-transformations.R).

Before the correction, a smaller native-function example evaluated the 95th percentile of two equally
weighted Weibull components, with scales 10 and 20. Five handpicked parameter
draws use common shapes 0.8, 1, 1.2, 0.001 and 1.3. Returned times are approximately
61.40, 47.80, 40.75, NA and 38.40. The very small shape puts the requested percentile
beyond the representable time range. The quantile function warns generically;
the bound function discards the NA and returns 38.575 to 60.379. This illustrates
why reporting the rejected fraction matters. These are handpicked parameters,
not a measured failure rate from a fitted model's normal simulation.
Tracing the actual native call clarified provenance: flexsurv's qgeneric returns
a finite value at the representable time boundary for that extreme case. Our
custom mixture accuracy check rejects it and returns NA; our bound helper then
drops it. A native single-Weibull quantile returns Inf in the corresponding
out-of-range case. The interval-filtering behavior was our implementation.

The follow-up correction retains that fourth draw as `Inf`: an independent
80-digit log-time inversion gives log(t)=1099.83798, or approximately
4.50 x 10^477. This is a mathematically finite percentile beyond double range.
Its Weibull expectation is also mathematically finite, despite numerical
overflow; an infinite expectation does not itself make an interior percentile
infinite. With all five draws retained, the illustrative 95% upper confidence
bound is infinite. Any unresolved `NA`/`NaN` draw now withholds that prediction's
bounds instead of selecting only successful evaluations. Counts reach output notes.

The counting limitation is not an incorrectly connected flexsurv argument.
Our custom mixture is fitted through flexsurvreg's per-row distribution likelihood.
A persistent-class longitudinal model requires a different joint likelihood and
subject histories; the current custom distribution supplies neither. This does
not invalidate independent delayed-entry observations or ordinary single-component
counting-process likelihoods.

## Implemented changes and conventions

“New” below means added or materially extended since the split. “Inherited” means
already present in the Mixture PR, though the new interface may expose it more widely.

| Change | Mathematical meaning and software comparison | Assessment / action |
| --- | --- | --- |
| **Parameter-specific and component-specific regression — new** | Each native distribution parameter has its own linked equation. Positive parameters use log links; unrestricted parameters use identity links. This follows flexsurv's parameter regression. Ancillary predictors generally remove a single constant AFT ratio or PH ratio. | Keep. Explain effects by parameter and show percentile-dependent ratios when shapes vary. Do not interpret every exponentiated coefficient as a hazard ratio. |
| **Native parameterization retained — inherited, extended** | Weibull uses native shape and scale: survreg's residual scale equals 1/shape and its intercept equals log(scale). Exponential and gamma model log(rate), so the coefficient direction is opposite to a log-lifetime equation. Gompertz is PH with constant shape, not generally AFT. | Keep native names and links. Provide a parameter conversion example for comparisons with survreg and other packages. |
| **Original generalized gamma/F available — new** | These are legitimate native distribution families, not aliases of the stable parameterizations. Coefficients and some model restrictions differ. flexsurv recommends its stable versions for routine modeling. | Keep original forms for compatibility or specific equations. Prefer stable forms in examples; warn about weak identification near limiting distributions. |
| **Predictor transformations and factor coding — new** | Scaling, powers, logarithms, roots, orthogonal polynomials and natural splines transform predictors within parameter equations. Splines are not Royston–Parmar baseline-hazard splines. Polynomial factor contrasts use category order with equally spaced scores, not measured distances between category labels. | Keep. Preserve fitted bases for prediction, as implemented. State what a fixed transformed coefficient constrains; document knot/basis choices. |
| **Arrhenius in eV — new** | Temperature converts to Kelvin; predictor is 1/(k_B T). With fixed ancillary parameters and no temperature interactions, the log-lifetime slope is activation energy in eV; for log(rate), energy has the opposite sign. k_B uses the exact SI constants. | Sound. Keep the sign/unit explanation. Ancillary temperature effects or interactions require interpreting the complete model, not one slope as a universal activation energy. |
| **Restricted Eyring preset — new** | Log lifetime includes `-log(T) + beta/T`; log(rate) includes `+log(T) + beta/T`. Thus the lifetime prefactor is T^-1. Unlike Arrhenius, the reciprocal term is 1/T and multiplying its slope by k_B gives energy in eV, with the appropriate rate/lifetime sign. Interactions are prohibited, Eyring is location-only and Gompertz is excluded. NIST's general reliability Eyring form permits an estimated temperature exponent and stress-temperature interactions. | Sound restricted model. Describe it as a preset, not the entire general Eyring family. Add a free log-temperature exponent and supported interactions only if that broader model is wanted. Ancillary regression can still change the overall acceleration pattern. |
| **Analytic GenF-original information — new** | One-component canonical Eyring GenF-original fits with entirely exact events in right-censoring mode replace numerical information with analytic observed density information at the same estimates. It is not an expected-information calculation or a refit. Other cases retain native information. | Independent derivative and regression-mapping checks passed. Keep scope and provenance explicit. Extend to censored contributions only after deriving and checking their derivatives. Positive information alone does not establish a global optimum. |
| **Fixed baselines and regression coefficients — new** | Fixed coefficients contribute to the model but not the free parameter count. Inference is conditional on those values being known. Fully fixed finite predictions have zero estimation uncertainty; they still have event-time variability. | Sound. Fixed values estimated elsewhere do not become known constants statistically; sensitivity analysis or joint uncertainty is needed in that situation. |
| **Average reference profile — new** | Unused numeric predictors use frequency-weighted means. Unused factors have equal weights over their level combinations. The design is averaged on each parameter's link scale before evaluating the distribution. This is the same style as link-scale reference-grid averaging in emmeans, and differs from response-scale population standardization in `standsurv`. | Valid reference-profile estimand. Keep notes explicit; a separately named population-standardized option is optional. Example: two exponential groups with means 10 and 40 give reference-profile mean 20, versus equally weighted population mean 25. |
| **Density and probability diagnostics use different references — new/extended** | Density averages fitted densities over actual weighted predictor profiles. Probability plots map each observation to a stratum's frequency-weighted mean design, preserving its individual fitted survival probability. Their reference is independent of the Predictions reference selector. | Both are coherent, but answer different questions. Explain the distinction. Adjusted probability points depend on the fitted model; model confidence bands do not constitute a calibrated goodness-of-fit test. |
| **Reference/current relative lifetime — new** | Ratios divide reference lifetime by current lifetime, using paired draws from the same fit. Standard location-only AFT coefficients in survreg describe current/reference lifetime through exp(beta), so that comparison may require taking a reciprocal. Ratios can depend on percentile when ancillary parameters vary. | Sound chosen direction. Retain the direction in titles/help and avoid presenting it as an invariant time ratio for arbitrary parameter regression. This is not population-relative survival based on expected mortality. |
| **Pointwise normal-simulation confidence bounds — extended** | Joint transformed coefficients are drawn from their asymptotic multivariate normal distribution. Each derived quantity is evaluated within a draw, then bounded by its quantiles. At 95%, two-sided bounds use 2.5/97.5%; lower or upper bounds use 5% or 95%. This agrees with native flexsurv's pointwise simulation approach. | Sound under regular likelihood assumptions. These are not simultaneous bands, data-resampling bootstrap intervals or uncertainty after model selection. Missing-draw handling is corrected above; Monte Carlo adequacy remains relevant. Shared draws correctly retain within-fit dependence for ratios and averages. |
| **Individual event-time prediction range — new** | Endpoints are fitted distribution quantiles. They include individual variability, exclude parameter-estimation uncertainty and can be asymmetric around a mean or median. The GUI says “Prediction interval”; output notes/table headings say “prediction range.” | Legitimate plug-in range. Prefer one consistent label identifying this convention. Add parameter uncertainty only with a clearly defined predictive procedure, not by simply adding confidence limits to quantile endpoints. |
| **Additive component plots — new** | Survival/failure/density/RMST curves are weighted component contributions. Hazard contributions are pi_k f_k(t)/S_mix(t); cumulative hazard contributions integrate these terms. They add to the mixture quantities. The Mixture PR showed standalone component survival/failure/hazard curves instead. | Correct changed interpretation. Label them as contributions, not standalone component hazards or survival probabilities. A latent mixture does not by itself identify competing-risk causes. Component quantiles are not averaged to obtain mixture quantiles. |
| **Tail-safe mixture quantiles and Gompertz means — new** | Quantiles invert log-tail probabilities on log time and check accuracy. Negative Gompertz shape leaves a positive mass at infinity; sufficiently high quantiles and unrestricted means are therefore infinite. Zero-weight components contribute nothing. This follows unrestricted flexsurv Gompertz, while implementations restricting positive shape cannot represent that mass. | Sound. Keep infinite outcomes distinct from numerical failure. Recommend RMST for a finite horizon. Do not silently replace an infinite mean by a finite integration result. |
| **Weighted observed histogram — inherited, extended** | Bin masses are Kaplan–Meier survival drops, not proportions of observed failures. Total identified mass may be below 1 when the tail is censored. On a natural-log time axis both fitted and empirical density use the Jacobian t f(t). | Sound censoring-aware diagnostic. Keep the unidentified tail unnormalized. Label log-time density clearly; histogram bin choices remain presentation choices. |
| **Common fitting cohort and new-observation inputs — new/extended** | Selected models/families share the cohort remaining after missing-value filtering. Manual/CSV/dataset predictions reuse fitted transformations and bases; dataset mode targets rows with missing outcomes. Extrapolation does not refit the model. | Keep common rows for meaningful IC comparisons. Explain when adding a model reduces sample size and when predictions extrapolate beyond the fitting range. |
| **Adaptive grids and plot limits — extended** | Curves refine a finite grid around features. Some diagnostics trim extreme CI values when choosing displayed y limits; data values remain intact. Heatmap limits round outward and saturate colours beyond their range. Contours use interpolated finite surfaces, without confidence surfaces. | Numerical estimates are unchanged; appearance can conceal extreme uncertainty or variation. Offer full-band display/clipping indications and retain precise values in tables. Finite grid resolution is not an accuracy guarantee for every extreme model. |

Main source groups: [equations](R/parametricsurvival-equations.R),
[temperature](R/parametricsurvival-temperature.R),
[information](R/parametricsurvival-information.R),
[reference conditioning](R/parametricsurvival-conditioning.R),
[prediction views](R/parametricsurvival-views.R),
[component distributions](R/parametricmixturesurvival-distributions.R),
[component plots](R/parametricmixturesurvival-plots.R).

The parameter/link conventions match [flexsurv regression](https://chjackson.github.io/flexsurv/reference/flexsurvreg.html)
and the documented conversion to [survreg](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survreg.html).
See [original GenF](https://chjackson.github.io/flexsurv/reference/GenF.orig.html),
[original generalized gamma](https://chjackson.github.io/flexsurv/reference/GenGamma.orig.html),
[NIST Arrhenius](https://www.itl.nist.gov/div898/handbook/apr/section1/apr151.htm),
[NIST Eyring](https://itl.nist.gov/div898/handbook/apr/section1/apr152.htm),
[population standardization](https://chjackson.github.io/flexsurv/reference/standsurv.html),
[pointwise summaries](https://chjackson.github.io/flexsurv/reference/summary.flexsurvreg.html),
[Gompertz](https://chjackson.github.io/flexsurv/reference/Gompertz.html),
[natural splines](https://stat.ethz.ch/R-manual/R-devel/library/splines/html/ns.html)
and [orthogonal polynomials](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/poly.html).
For the order of averaging and back-transformation, see
[emmeans transformations](https://rvlenth.github.io/emmeans/articles/transformations.html).

## Inherited limitations relevant to the new features

- **Candidate selection is heuristic.** Without an explicit spread constraint,
  `.sapmSelectCandidate()` prefers candidates not flagged as degenerate, even if
  another converged candidate has greater likelihood. Checks include effective
  membership below 3, log-IQR ratio below 0.01 and extreme transformed parameters.
  The new ancillary equations extend these diagnostics to conditional parameters.
  A helper example selects log likelihood -100 over flagged -90; constrained
  selection chooses -90. This is an eligible local solution, not a guaranteed global
  unrestricted MLE. Document the thresholds; consider an explicit constraint or
  penalty instead of hidden candidate filtering. If every candidate is flagged,
  the unconstrained path can still report the best flagged candidate with warnings.
- **Mixture uncertainty requires identification.** Wald/delta-method intervals and
  ordinary chi-square LRTs require a regular, identifiable interior fit. The code
  checks nested parameter spaces, suppresses LRTs involving an active bound, and
  does not turn different component counts into ordinary nested-regression tests.
  Coincident components or nearly zero weights can nevertheless invalidate usual
  inference. Suppress unsupported inference or offer a carefully calibrated
  refitting bootstrap. AIC/BIC remain useful selection criteria but are not exact
  evidence probabilities; ordinary BIC's regular marginal-likelihood derivation
  does not automatically apply at mixture singularities.
  See [Drton and Plummer](https://arxiv.org/abs/1309.0911) and
  [Brazzale and Mameli](https://arxiv.org/abs/2206.15178).
- **Start approximations do not redefine the final likelihood.** Short EM runs,
  soft membership starts and approximate handling of delayed entry provide starting
  values. The final native optimizer uses the full survival likelihood. More starts
  improve search evidence but do not prove global optimality. Retain convergence,
  replication and boundary diagnostics.
- **Frequency weights are replication counts.** Positive integers enter the
  parametric analyses through literal row expansion. They are not survey weights
  or inverse-censoring weights. The former spline/polynomial exception is resolved
  by constructing their ordinary R bases on the expanded dataset. Missing-outcome
  predictions use the original input rows. Fit-cache mapping detects old compressed
  fits and refreshes retained outputs; standalone KM/Cox behavior is unchanged.
- **Response residuals are naive.** The difference between observed follow-up and
  fitted mean treats censored follow-up as if it were a realized lifetime. It matches
  [flexsurv's response residual](https://chjackson.github.io/flexsurv/reference/residuals.flexsurvreg.html),
  but is not an unbiased event-time error. Prefer censoring-aware Cox–Snell checks
  where supported; the interval-censoring diagnostic is explicitly unavailable.
- **Predictions start at time zero.** Delayed-entry fitting conditions the likelihood
  on survival to entry; the current GUI predictions use start=0. They describe the
  fitted underlying distribution, not automatically an individual's remaining life
  after entry. This agrees with flexsurv's default. A future conditional prediction
  option must define total versus remaining lifetime consistently for every summary.
- **Counting mixtures do not implement longitudinal class histories.** Independent
  delayed-entry observations have the correct row-wise likelihood. Multiple rows
  from one subject with changing predictors require class-specific accumulated
  survival over that subject's covariate history if class membership stays fixed.
  The present interface has no subject-history linkage and its per-row mixture
  normalization does not calculate that likelihood. Document independent delayed
  entry as the supported scope, or add IDs and a history-aware mixture likelihood.
  A two-interval exponential example gives row-wise log likelihood -2.220045 versus
  -1.697837 for a persistent class; identical constant covariates telescope correctly.
- **Probability-paper positions are display conventions.** Johnson/KM adjusted
  ranks, selected tie handling and endpoint nudges are not the unchanged raw KM
  estimate. They keep probability-paper coordinates finite; they should not be used
  as a formal test or reported as exact nonparametric event probabilities.
- **Nonparametric and Cox calculations were not replaced.** The refactored survival
  comparison calls still use `survdiff(rho=0)`, `rho=1` or user rho. The option labelled
  Flemming-Harrington represents the G-rho special case (gamma=0), not the entire
  two-parameter FH family. Cox CI refactoring retains the selected robust/model
  standard error. See [survdiff](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/survdiff.html).

## Independent verification

The reference likelihood is the usual independent-observation survival likelihood:
an exact event contributes f(t), right censoring S(t), interval censoring
F(upper)-F(lower), and delayed entry divides by S(entry). Frequency weights multiply
log contributions. For mixtures, f and S are sums of weighted component values.
The mixture entry denominator cancels in posterior membership ratios; dividing
each component numerator by its own entry survival would be wrong unless the prior
component weights were adjusted correspondingly. Checked against direct formulas
and [Jackson's full likelihood](https://www.jstatsoft.org/article/view/v070i08).

For GenF-original, the independent log density was

```text
z = log(s1) - log(s2) + (log(t) - mu)/sigma
log f(t) = -log(sigma) - log(t) + s1*z
           - (s1+s2)*log(1+exp(z)) - log B(s1,s2)
```

High-precision differentiation used coordinates
`(mu, log(sigma), log(s1), log(s2))`, independently of the R derivative implementation.
Regression information follows `-sum_i w_i J_i' H_i J_i`; the parameter predictors
are affine in the regression coefficients, so no additional predictor-curvature
term is needed. Fixed coordinates are excluded from that matrix.

| Check | Actual result |
| --- | --- |
| GenF-original score and Hessian, 432 points, shape range 1e-8 to 1e14, standardized log-time offsets -80 to 80 | 90-digit mpmath reference; maximum error divided by 1+absolute reference: score 5.75e-15, Hessian 3.22e-14; tolerance 1e-9 |
| Full regression information, 7 free coefficients, 12 frequency-weighted rows and one fixed slope | Independent 75-digit full-log-likelihood Hessian; maximum scaled discrepancy 4.33e-16; tolerance 1e-9 |
| Mixture density/CDF/RMST and quantile inversion in all 10 families; conditional RMST | Weighted identities and direct survival quadrature passed; quantile/quadrature tolerance 1e-7 |
| Delayed-entry mixture likelihood and posterior memberships | Direct exponential-mixture formulas passed at 1e-10 tolerance |
| Counting with within-subject covariate changes | Confirmed a different likelihood from persistent class histories; documented supported independent delayed-entry scope |
| Tail interval and cure edges | Reproduced native/helper interval failure; correct finite likelihood computed independently. Negative-Gompertz mixture correctly gives infinite upper quantile and mean. Actual Arrhenius expressions match independent SI/Kelvin calculations in all three units. |
| Eyring structural signs and acceleration in 9 eligible families | Native quantiles reproduce T2/T1 times exp[E/k_B*(1/T1-1/T2)] at 1e-10 tolerance; Kelvin/Celsius/Fahrenheit conversions passed |
| Four log-time spread identities, gamma inverse bound | Independent quadrature passed at 1e-8; inverse bound passed at 1e-10 |
| Frequency SDs/quantiles, 100 samples | Expanded-row comparisons passed at 1e-10 |
| Fixed-design frequency fitting | Coefficients, log likelihood, covariance and BIC match expanded rows at 1e-6 or tighter; native BIC uses the total frequency count |
| Native fits with ancillary regression, transformed interactions, factor coding and polynomial terms | Independent explicit designs match likelihood/predictions at 1e-6 or tighter; fixed baseline/slope and free-parameter penalties passed |
| Pointwise simulation bounds | Matched native flexsurv using identical 2,000 joint draws at 1e-12 |
| Six component contribution types | Contributions sum to the mixture values at 1e-7, including integrated hazard |
| Weighted KM histogram, quantile reference mapping, mixing-probability delta method | Mass/Jacobian and mapping checks passed; probability SEs match an explicit stick-breaking Jacobian at 1e-9 |
| Full repository suite on protected HEAD archive | 26 passes, 37 failures, 6 errors, 1 warning, 0 skips; exact per-test outcomes unchanged from the established baseline |

Numerical agreement is evidence, not a proof of coverage or exhaustive edge-case
correctness. The full suite is not green: existing saved-option/imported-default
and Windows figure mismatches remain. No human tests or snapshots were edited or
accepted by this work. Full scripts, logs, result files and reproduced limitations are retained
locally under `.work/statistical-review/`; initial harness errors were corrected
before the successful final runs. The initial review changed no production code;
the subsequent user-authorized frequency expansion is documented above.

Follow-up implementation and explanation checks are retained in
`.work/frequency-unroll/`. Direct native experiments confirm that ordinary
single-component exponential counting data reproduce a piecewise-rate likelihood;
the persistent-class mixture issue does not apply to that single-component case.
Standsurv successfully evaluated our custom mixture survival and quantiles, but
its public type selector does not offer unrestricted mean. Its population-averaged
estimand differs from our current link-averaged reference profile. It is a suitable
candidate for a separate population-standardized prediction mode.

For Eyring interactions, the existing equation compiler already forms reciprocal
temperature times another stress while keeping the fixed log-temperature term
separate. Both QML equation forms already permit interaction terms; the current
R validation blocks them when Eyring is present. Relaxing that policy would still
need fit/inference/label verification. For example, log lifetime
`a - log(T) + b/T + c*V + d*V/T` gives voltage effect `c + d/T`;
the current guard is the obstacle, not the equation compiler. No interaction
policy change was requested or implemented by this follow-up.
