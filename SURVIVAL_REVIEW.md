# Survival module review

Reviewed 25–26 September 2026, starting at `a6839b9` on `mixture`. Work prepared on `codex/survival-review`, then restored from the retained GitHub Desktop stash onto local `mixture` for the user-requested commit series on 27 September. The follow-up prioritizes native library implementations and warnings/missing values for numerical limitations.

Scope: all R backends, four analysis interfaces, shared QML, registration, dependencies, CI, and all eight saved-example test scenarios. Five independent reviewers covered classical survival, parametric models, mixture estimation, mixture outputs, and integration. A second pass cross-reviewed the changes and investigated mixture numerical edge cases. No remote changes.

## Commit review guide

The local `mixture` history is rebuilt from upstream `d58a700`. The original history, including the diagnostics/export changes at `65bc1eb`, is retained on `codex/mixture-history-backup-20260930`. No remote history is changed.

| Order | Commit subject | Review scope |
| --- | --- | --- |
| 1 | Split parametric survival into focused files | Mechanical moves only; original definitions unchanged |
| 2 | Align survival dependencies and test coverage | Dependencies, CI, package exclusions, existing fixture |
| 3 | Validate survival data and preserve classical inference | Shared validation, empirical summaries, Cox errors/residuals |
| 4 | Prepare shared parametric fits and outputs for mixtures | Fit indexing, shared output selection, native warnings/errors |
| 5 | Add mixture distributions and component spread bounds | Distribution adapters, posterior probabilities, bound policy |
| 6 | Fit survival mixtures from multiple starting values | Initialization, EM, native maximization, candidate diagnostics |
| 7 | Report mixture parameters classification and diagnostics | Tables, component curves, identification warnings |
| 8 | Register parametric mixture survival analysis | QML interface, analysis registration, user documentation |
| 9 | Refine mixture diagnostics and add observation exports | Nested coefficients, observed histogram, per-observation exports |

The first commit is verified by comparing parsed definitions with upstream before and after the file moves. Intermediate versions are checked for R syntax, and the final source tree is checked against the retained original tree. Validation counts describe the tested final implementation, not a separate full-suite run at every intermediate commit.

Final implementation: 99 passing expectations, two warnings, no errors, and the same pre-existing ovarian plot snapshot mismatch. Independent mixture prediction, posterior, histogram, subgroup/missing-row, and counting/interval export checks pass. QML syntax checks pass. Dataset column writes require JASP Desktop validation; snapshot artifacts remain for manual review.

User-approved local commit series preserves the documented snapshot exceptions. It does not approve snapshot replacements, editing human-owned tests, pushing, or merging. Local configuration and `.gitignore` changes remain excluded.

## Corrections

### Shared data and classical analyses

- Reject zero-length counting intervals, reversed finite intervals, invalid infinity directions, infinite event times/weights, and datasets with no usable observations. Retain legitimate one-sided interval censoring; count completely missing intervals as omitted observations.
- Avoid covariance-matrix checks on a scalar covariate and variance checks on categorical subgroup labels.
- Invalidate Cox/parametric fits when interval columns change. Invalidate censoring summaries when variables affecting complete-case selection change.
- Keep the required baseline parameter in parametric formulas. `flexsurv` removes the first design column as its intercept: requesting `~ x - 1` silently discarded `x`. Remove the unsupported intercept checkbox from both parametric interfaces. Existing saved false settings no longer discard a predictor.
- Match stratification variables exactly: `jaspColumn1` previously corrupted `jaspColumn10`. Pass the selected Student-t frailty degrees of freedom.
- Handle incomplete specifications, failed fits, empty Cox models, failed null models, and unsupported diagnostics without secondary crashes. Remove the misplaced Vovk–Sellke calculation from the hazard-ratio table.
- Align Schoenfeld residuals with event rows ordered by time within strata. Use interval ends for counting-process residual time plots.
- Report log-rank degrees of freedom from strata with positive expected events. Compute actual empirical life-table quantiles.
- Correct plot dependency/error handling, failed Kaplan–Meier fit handling, and single-stratum labels.

### Parametric fitting and output

- Correct failure-probability intervals: `[1 - survival_upper, 1 - survival_lower]`. Reversed bounds affected tables, ribbons, and Kaplan–Meier overlays.
- Apply frequency weights and the requested confidence level to Kaplan–Meier overlays; include their initial survival point at time zero.
- Accept zero/blank sequence starts, reject empty/nonfinite custom steps, and retain the last requested quantile when it is below one.
- Refresh cached model titles and stored model specifications; compare the current specification when considering fit reuse.
- Keep total/partial model failures visible in requested output when model summaries are disabled. Contain unsupported residual and merged-factor prediction errors within their output sections.
- Correct merged failure-table and covariance-table dependencies, APA legend selection, and unstable complementary-log-log inverse arithmetic. Forward the requested transforms through both supported `jaspGraphs` argument conventions; some labeled logarithmic axes were silently linear.
- Explain that covariance matrices use estimation-scale parameters and mixture stick-breaking coordinates.

### Mixture estimation and reporting

- Fit every candidate with native `flexsurvreg`, then construct covariance/intervals at the selected estimates with its native Hessian-only call (`maxit = 0`). Remove the separate optimizer objective, transformed optimization-vector machinery, second optimization, and custom Newton-decrement calculation. Native optimizer convergence and Hessian diagnostics replace the latter.
- Retain only necessary mixture-distribution/initialization/labeling adapters around native family functions. Remove the review's bespoke stable interval-probability calculation and alternate likelihood comparison. Check native CDF differences for nonfinite/zero probabilities and severe subtractive cancellation; exclude unreliable candidates and explain unavailable output. This diagnostic identifies numerical risk; it does not certify every finite result.
- Delegate mixture quantiles to exported `flexsurv::qgeneric`, with positive-time transformation and boundary/cure handling. Verify the returned CDF inversion; show warnings and missing values when native calculation is unreliable. No homemade bisection or alternate numerical solver remains.
- Reorder native parameter estimates before covariance/CI construction. Remove the custom covariance/Hessian relabeling Jacobian; only the necessary component/weight reexpression remains, without clipping selected weights.
- Remove time-unit-dependent location thresholds from collapse diagnostics. An infinite Gompertz quartile alone does not establish degeneracy.
- Count exact, left-censored, and interval-censored events consistently. Adding an exact observation previously caused interval events to disappear from the diagnostic count.
- Allow a one-component model without mixture starting methods.
- Include the one-component fit when detecting a worse likelihood in a larger nested mixture. Identify model cells by model ID, not editable titles.
- Surface failed fits and identification/convergence warnings in classification/diagnostic output. Explain the all-degenerate fallback accurately; nonconvergence does not prove component collapse. Describe classification entropy as assignment uncertainty: low entropy alone does not establish separated components.
- Capture native prediction warnings in table footnotes/plot captions; keep unavailable values missing and contain all-unavailable plots. Honor the requested CI option and retain native prediction calculations.

### Constrained maximum likelihood

- Add optional **Advanced > Mixture > Constrain component spread**, with a positive minimum SD of natural log time. The option is off by default; 0.1 is an editable preset, explicitly requiring application-specific justification and sensitivity checks.
- Support log-normal, Weibull, log-logistic, and gamma families through native transformed parameter bounds. Apply the same restriction to K = 1, larger mixtures, and hidden split-start fits. Unsupported families are deselected; backend errors remain explicit.
- Use native bounded optimization and select the highest converged feasible likelihood. Retain low effective sample size and identification diagnostics as warnings, rather than additional undisclosed restrictions.
- Reorder estimates and stick weights before native covariance construction. Finalize with unbounded BFGS at `maxit = 0`, verifying unchanged estimates and likelihood. Native L-BFGS-B can move even with `maxit = 0` and is therefore used only for actual optimization.
- At an active spread bound, retain point estimates and fit criteria while showing missing standard errors, covariance, confidence intervals, Wald tests, and likelihood-ratio comparisons, with explanatory warnings. Skip the ordinary native Hessian at that boundary.
- Keep the native distribution/likelihood implementation. Feasible initializer repairs handle tied observations; native `survreg` provides bounded-spread EM starts for its supported AFT families. No alternate likelihood or covariance relabeling engine was introduced.
- Preserve native single-component warnings and missing covariance dimensions. Keep selected-candidate diagnostic warnings visible under likelihood-based selection.
- Preserve mixture dimensions for empty density inputs. Native fits can request zero density rows when every observation is censored; this previously caused a component-index error.

The statistical restriction, family mappings, inference policy, and literature limits are documented in `MIXTURE_REGULARIZATION.md`.

### Integration

- Declare the already-used, already-locked `ggplot2` and `scales` imports.
- Run relevant CI on QML changes; align coverage triggers with lockfile/workflow changes.
- Exclude local agent configuration and research workspaces from source packages.
- Correct misleading prediction/censoring help while preserving existing option values and reusable QML components.

## Verification

No human-owned tests or reference snapshots edited or accepted. The test runner removed unused snapshots after early setup errors; those references were restored from the unchanged starting commit.

The first review's controlled full suite after reinstall returned **94 passes, 4 snapshot failures, 0 errors, 0 warnings** (144.81 seconds). Numerical table assertions passed. Results and runtime versions are preserved in `.claude/review/final-results.json` and `.claude/review/session-info.txt`. Follow-up native-backend validation is recorded separately below. The four original changed snapshots correspond to the corrected outputs:

| Fixture | Snapshot change |
| --- | --- |
| Ovarian, analysis 1 | Survival overlay includes time zero |
| Full output, analysis 1, subgroup 1 | Survival overlay includes time zero |
| Full output, analysis 1, subgroup 2 | Survival overlay includes time zero |
| Full output, analysis 2 | Failure-probability confidence bounds ordered correctly |

New `.new.svg`/`.new.rds` candidates are available for human inspection. They are not replacements for the reference files.

The first-pass scratch checks below established numerical reference cases. The custom stable likelihood/quantile implementations used in that pass were subsequently replaced as described above; these exact tail values must not be read as a claim that native `flexsurv` can fit every such case:

| Check | Evidence |
| --- | --- |
| Exponential mixture at time 1000, rates 1 and 2, equal weights | Negative log-likelihood `1000.693147`; old result `1e10` |
| Same tail observation | Posterior approximately `(1, 0)`; old result `(0.5, 0.5)` |
| Truncated exponential observation, entry 999 and event 1000 | Negative log-likelihood `1`; old result `1e10` |
| Exponential interval `(40, 41]` | Probability `2.685472e-18`; old result zero |
| Defective Gompertz mixture, target probability 0.7 | Quantile approximately `1.693918`; old result infinity |
| Right/interval/counting mixtures and continuous/factor regressions | Five full fits; wrapped/direct likelihood difference at most `1.71e-13` |
| Posterior normalization and ordinary-scale interval masses | Row sums within `1e-12`; six families agree within `1e-14` |
| Component means/medians and delta-method errors | Analytic exponential checks within `1e-7` |
| Relabeling near boundary weights | Two-component swap Jacobian `-1` instead of zero; finite covariance/Hessian; four-component probability permutation preserved |
| Adapter consistency safeguard | Rejected a finite wrapped/stable log-likelihood disagreement of `0.4150218`; normal fits retained |
| Component scales separated by 200 orders of magnitude | Exponential mixture quantile `6.931472e-101`, agreeing with `log(2) / 1e100` |
| Time units and cure distributions | Equivalent times scaled by `1e8`/`1e-8` retain classification; valid cure component retained with effective membership `130.6533` |
| Classical diagnostics | Independent Schoenfeld row pairing and log-rank degrees of freedom; readiness and error-path checks |

QML/R option contracts were checked using main files, imported components, and generated controls. Package metadata/workflow checks and `git diff --check` passed. `qmllint` was unavailable; native Qt parsing and interactive reactive transitions still require Desktop verification.

The native-backend follow-up passed six fitted-model cases (right censoring, continuous/factor predictors, interval censoring, counting/delayed entry, and weights), with the same likelihoods as direct native reference calls. Frequency weights matched row expansion. Ordering parameters before native Hessian construction preserved likelihood and predictions; covariance agreed with an independent two-component mapping within `4.67e-12`. Public `qgeneric` roots matched an independent `uniroot` reference within `1e-9`, handled a 200-order time-scale range, and returned warning/NA for unrepresentable quantities while retaining valid rows. Returned roots are checked against the requested smaller-tail probability rather than an absolute CDF tolerance. Warning footnotes and unavailable-plot errors were verified headlessly. These checks are in `.claude/review/native-core-check.log` and `.claude/review/native-output-check.log`.

Final follow-up suite: **94 passes, the same 4 snapshot mismatches, 0 errors, 0 warnings** (140.11 seconds), after the final smaller-tail check. Numerical assertions and targeted native-output checks passed. Records: `.claude/review/native-confirmed-tests.json` and `.claude/review/native-confirmed-run.log`. Tests and reference snapshots remain unchanged.

Other previous fixes were audited for duplication. KM estimates remain from `survival::survfit`, prediction intervals from native `summary.flexsurvreg`, and residuals from native residual methods. CI complements, row alignment, caches, labels, and missing-value presentation are application adapters. `ggsurvfit_build` now uses its exported API. Component moment/probability delta-method output transformations were preexisting; changing their inference method was not part of the cleanup.

Constrained-ML follow-up checks covered tied observations in all four families at K = 1 and K = 2; K = 3 with two regressors; gamma inverse bounds from `1e-6` to 100; interval censoring; delayed entry; split starts; and constraint-off behavior. Independent analytic tied-event likelihoods agreed within `1.9e-12`. Integer frequency weights matched row expansion within `5.2e-11` in likelihood and `8.3e-7` in relative parameters. All six K = 3 relabelings agreed with an independent covariance Jacobian reference within `8.64e-7` relative matrix error; likelihood was unchanged and predictions differed by at most `2.22e-16`. Records: `.claude/review/constrained-validation.txt`, `.claude/review/constrained-final-check.log`, and `.claude/review/constrained-reference-check.log`.

Headless constrained output checks verified active-bound K = 1/K = 2 regression fits at 90% confidence, missing covariance/SE/CI/Wald/LRT output, native point predictions, component quantities, and 14 plots without model confidence ribbons. Interior fits retained native covariance and intervals. The four-family batch, local unsupported-family errors, positive-event validation, and zero-time right censoring were checked. An unconstrained exponential fit with an exact zero time retained native `flexsurv`'s existing rejection. Scratch harness errors from partial field matching and an incorrect zero-time expectation were investigated against exact result fields and direct native calls; no source behavior was changed to satisfy those expectations.

Visual checks caught clipped warning captions. A shared caption helper now wraps to plot width and reserves vertical space; warning-free plots retain their prior objects and dimensions. Rendered 400- and 550-pixel plots show the full text. A fresh headless probability plot verified the native height setter at 520 by 532 pixels, with finite point predictions and no invalid model ribbon. Evidence: `.claude/review/constrained-caption-check.log` and `.claude/review/constrained-off-native-check.log`. Native Qt parsing and interactive option transitions remain unverified because `qmllint` and native app control were unavailable.

After reinstalling the final constrained implementation, the full suite returned **94 passes, 4 snapshot mismatches, 0 errors, 0 warnings** (138.91 seconds). The restored source was reinstalled and checked again before partitioning on 27 September, with the same results in 139.60 seconds. All four failure records exactly match the preceding native-backend run. Records: `.claude/review/constrained-final-tests.json`, `.claude/review/constrained-final-suite.log`, and `.claude/review/commit-validation.log`. `git diff --check` passed; tracked tests/reference snapshots remain unchanged. The user explicitly approved local commits with these four failures; snapshot acceptance remains pending human inspection.

R setup required access to the external renv cache and JASP resources. Restricted bootstraps stalled before tests; replacement runs completed. Repeated `setupJaspTools()` calls remove shared resources, so validation runs were serialized. Early setup-error counts are not a reliable scientific baseline.

## Recommended next work

**Native-library numerical limit:** `flexsurvreg` subtracts ordinary CDFs for censoring/truncation probabilities. Fixed parameters for an exponential mixture with rates `(1, 2)`, probabilities `(0.5, 0.5)`, and intervals `(1, 2]`, `(40, 41]` give native log-likelihood `-Inf` instead of approximately `-42.89604`. The module now keeps native fitting and diagnoses poorly conditioned differences rather than maintaining another likelihood engine. Such fits may remain unavailable until the upstream implementation improves. Do not clip probabilities to manufacture finite answers.

1. **Make mixture regression coverage a release requirement.** There are no permanent mixture tests. Add kernel identities, all censoring modes, frequency-weight replication, factor/continuous effects, K=1 equivalence, relabel invariance, extreme tails, cure fractions, seed reproducibility, subgroup failures, and saved-state option transitions. Promote the scratch reproductions into human-owned tests after review.

2. **Validate the chosen constraint scientifically.** Optional constrained ML now bounds component log-time spread for four families; see `MIXTURE_REGULARIZATION.md`. Establish application-specific sensitivity guidance and, if needed, boundary-aware inference. The unconstrained mode still uses its existing post-fit ESS/IQR selection policy and can have unbounded exact-event likelihoods. Constraints do not establish identifiability or guarantee a global optimum; censored regression and delayed entry require their own justification.

3. **Resolve the Cox frailty uncertainty convention.** Coefficient intervals use `se2`, hazard-ratio intervals use `fit$var`, and `se2` is labeled robust SE. One saved example gives coefficient CI `[-0.05259374, 0.17723040]`, while the log of the hazard-ratio CI is `[-0.05336416, 0.17800082]`. Choose/document the variance convention and update the corresponding human-owned expectation together.

4. **Qualify cluster-robust inference.** Ordinary likelihood-ratio and score tests assume independent observations; disclose that assumption or select the appropriate robust score statistic. Keep this a deliberate statistical/output change with reviewed expectations.

5. **Separate fitting, prediction uncertainty, and rendering caches.** Changing coefficient CI level currently refits models. Cosmetic plot changes can regenerate Monte Carlo confidence bands. Cache native parameter draws and predictions by fit/grid/CI, and make rendering changes reuse them. Requested point-only predictions now avoid CI computation.

6. **Expose the search audit and computational cost.** Distinguish optimizer failure, low component support, collapse, coincident components, and unreliable curvature. Show attempted and failed starts, exclusion reasons, and the selected candidate. Progress currently advances at family/component granularity despite many optimizations per cell. Make the remaining covariate-effect collapse warning invariant to predictor centering: the current absolute `X beta` threshold can change under an equivalent reparameterization.

7. **Clarify conditional inference and undefined quantities.** State that intervals condition on the selected family, component count, and regression model. Distinguish infinite/undefined component means from computational failure. Place mean/median CI settings alongside their output, rather than under an unrelated coefficient checkbox. Disclose Cox reference covariates and restricted-mean truncation time.

8. **Reduce memory and maintenance costs.** Replace explicit frequency-weight row expansion where native weighted algorithms preserve the same statistical meaning. Split the mixture backend into numerical estimator, distribution adapter, and output builders. Consolidate duplicated QML sections without flattening reusable controls for test tooling.

9. **Repair infrastructure intentionally.** Verify the translation workflow's template targets (`jasptestmodule-qml`, `jasttestmodule-r`) before replacing them with real service identifiers. Make headless setup reuse installed resources and support concurrent processes safely. Add a native QML parsing check in CI.

No statistical convention changes in this recommendation list were made merely to obtain passing snapshots.
