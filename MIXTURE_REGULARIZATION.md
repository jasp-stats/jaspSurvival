# Mixture constrained maximum likelihood

Implemented as an optional setting in **Advanced > Mixture > Constrain component spread**. Fitting remains in `flexsurv::flexsurvreg`. This option adds explicit parameter bounds to the ordinary likelihood; it does not introduce a penalty or a second likelihood implementation.

## Established approaches

**Constrained estimation** restricts the parameter space while maximizing the ordinary likelihood. Hathaway's constraint is a *coupled ratio constraint*, `min(sigma_i / sigma_j) >= c > 0`, for normal mixtures. His existence theorem requires at least K + 1 distinct observations; consistency assumes IID normal-mixture observations and a true parameter satisfying the chosen constraint. The paper's c = 0.1 is one simulation setting, not a universal default. [Hathaway (1985), equations 2.1 and Theorems 2.1/3.3](https://doi.org/10.1214/aos/1176349557).

Absolute scale bounds and appropriate penalties also have support for univariate location-scale mixtures. Their consistency results require explicit regularity, tail, and penalty conditions. They do not automatically establish validity for censored regression or delayed entry. Gaussian mixture-regression research also studies data-adaptive constraints and tuning; choosing the restriction remains consequential. [Tanaka (2009), Sections 2-3](https://arxiv.org/pdf/0710.2183), [Di Mari, Rocci and Gattone (2019), Sections 2-4](https://www.datasciencegroup.unict.it/sites/datasciencegroup.unict.it/files/files/DRG2019.pdf).

**Penalized likelihood** changes the objective. Penalties targeting vanishing Gaussian variances, including suitable inverse-gamma/inverse-Wishart forms, can remove singularities. A penalty only on mixing weights does not stop a positive-weight component concentrating on an exact observation. A generic “regularization strength” would therefore conceal important family-specific choices. [Snoussi and Mohammad-Djafari, Proposition 2](https://arxiv.org/pdf/physics/0111007).

**Common dispersion/shape** is a substantive model restriction. A jointly estimated common gamma shape has consistency support for IID, uncensored mixtures. This does not prove the same result for the module's censored regression cases. Fixing all shapes at a preliminary estimate is not equivalent to jointly estimating a common shape. [He and Chen, Sections 2-4](https://arxiv.org/pdf/2011.04058).

Bayesian inference is a separate modeling choice. A suitable prior can regularize the singular parameter, but an arbitrary proper prior, or a prior on weights alone, is not a general guarantee against an unbounded posterior mode.

## Implemented option

The constraint is an **absolute component-spread floor**, for the four families below. These mappings follow from log-time moments, not a universal prescription from the cited papers.

- **Constrain component spread**: off by default.
- **Minimum log-time SD**: positive epsilon, enabled with the checkbox; editable preset 0.1, maximum 100. Log means natural logarithm.

No universally justified epsilon was identified. The help explicitly identifies 0.1 as a preset, asks users to justify their choice and check sensitivity, and leaves the constraint off by default.

| Family | SD(log T) | Bound implied by epsilon |
| --- | --- | --- |
| Lognormal | sdlog | sdlog >= epsilon |
| Weibull | pi / (sqrt(6) * shape) | shape <= pi / (sqrt(6) * epsilon) |
| Log-logistic | pi / (sqrt(3) * shape) | shape <= pi / (sqrt(3) * epsilon) |
| Gamma | sqrt(trigamma(shape)) | Upper shape bound obtained by inverting trigamma |

These bounds are invariant to changing time units. They are static box bounds, unlike Hathaway's coupled ratio restriction. For positive event times, they bound each event-density contribution even with unrestricted location regression coefficients; ordinary censoring contributions are probabilities bounded by one. This is a boundedness argument, not a proof of existence, consistency, identifiability, or validity under left truncation.

Enabling the constraint restricts the distribution selector and deselects unsupported families. Exponential, generalized families, and Gompertz are excluded from this initial option. Gompertz shape has inverse-time units and can be negative, yielding a cure fraction; the same raw shape cap cannot be reused across time units. [Official Gompertz definition](https://chjackson.github.io/flexsurv/reference/Gompertz.html).

## Compatibility with the selected engine

`flexsurvreg` forwards optimizer arguments and estimates positive parameters on a transformed scale. Static `lower`/`upper` bounds can use this interface, with correctly transformed bounds and parameter ordering. `fixedpars` fixes constants; it does not express a jointly estimated common shape or a coupled scale ratio. No native penalty hook is documented. [Official flexsurvreg API](https://chjackson.github.io/flexsurv/reference/flexsurvreg.html).

The native optimizer uses `method = "L-BFGS-B"`, with component spread bounds transformed to the scale expected by `flexsurvreg`. Mixing weights and regression coefficients remain free. The same floor applies to every component count, including displayed one-component models and hidden one-component fits used to initialize splitting. Changing either constraint option invalidates the fit and dependent output.

Starting values are made feasible before bounded optimization. Native initializers are retained, with a small repair for undefined starts from tied observations. For log-normal, Weibull, and log-logistic EM starts, native `survival::survreg` can estimate the component with its spread fixed at the floor. These EM fits initialize the final native `flexsurvreg` optimization; they do not supply a separate reported estimator.

Constrained candidate selection uses the largest converged, numerically usable likelihood satisfying the declared bounds. Effective sample size, component separation, and other identification checks remain warnings; they no longer silently define additional exclusions in this mode. Multiple starts still do not certify a global maximum. Native precision failures remain failures, without clipping likelihood contributions or substituting another engine.

## Reordering and uncertainty

Component parameters and regression blocks are reordered by baseline median lifetime, and stick weights are reexpressed in that order, **before** native covariance construction. The final call uses unbounded `BFGS` with `maxit = 0`. It must preserve every selected parameter and the log-likelihood; explicit checks reject a changed point. A probe found that `L-BFGS-B` can take an optimization step even with `maxit = 0`, so bounded finalization is deliberately avoided.

For an interior optimum, native `flexsurvreg` computes covariance in the final coordinates. No custom Hessian/covariance relabeling Jacobian is required. Reusing a covariance matrix from the old component order would require such a transformation, particularly for three or more components because stick weights transform nonlinearly.

If any spread bound is active, estimates, log-likelihood, AIC/BIC, classifications, and point predictions remain available. Standard errors, confidence intervals, covariance estimates, Wald tests, and likelihood-ratio tests involving that fit are missing, with explanatory messages. The final native Hessian is skipped at an active bound. R documents that its ordinary returned Hessian is unconstrained even when bounds are active; inverting it would not supply boundary-aware inference. [R optim documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html).

Constraint activation uses distance on the transformed parameter scale (within `1e-6` of the bound). Feasibility allows floating-point roundoff only. Warnings identify the selected floor; the fit metadata also retains the natural parameter bound and the names of active parameters. Empirical Kaplan–Meier intervals remain available because they do not use the constrained model's covariance.

Constraints do not remove label switching, redundant components, weak identification, or model-selection uncertainty. AIC/BIC retain their ordinary formulas and are not a new boundary-corrected selection method. Future penalties must keep ordinary likelihood separate from the penalized objective.

## Numerical validation

Development checks cover K = 1 and K = 2 tied-event fits in all four families; gamma bound inversion from epsilon `1e-6` through 100; K = 3 regression; interval censoring; counting/delayed entry; frequency weights; time-unit changes; and switching constraints off. The gamma inversion's relative SD error stayed below `9e-16`. Scaling time by 1000 changed likelihood by the expected density Jacobian, within `8.3e-8`.

For an interior three-component fit with two covariates, all six label permutations were checked against an independent scratch-only numerical Jacobian applied to the original covariance. Native covariance recomputed in the permuted coordinates agreed within `8.64e-7` relative maximum matrix error; likelihood differences were zero and survival predictions differed by at most `2.22e-16`. The Jacobian is validation code only, not part of the module.

Independent analytic maxima for repeated exact events agreed with constrained K = 1 likelihoods for all four families within `1.9e-12`. Integer frequency weights matched repeated rows for K = 1 in all four families and K = 2 log-normal: likelihood differences at most `5.2e-11`, relative parameter differences below `8.3e-7`. These references are in `.claude/review/constrained-reference-check.log`.

Core evidence is retained in `.claude/review/constrained-validation.txt` and `.claude/review/constrained-final-check.log`. Earlier logs retain intermediate failures and their numerical probes; they must not be mistaken for the final checks. Permanent human-owned mixture tests still need reviewed additions.

Headless output and rendered-caption checks passed, including missing inference at active bounds and retained native inference for interior fits. The final installed-module suite returned 94 passes and the same four preexisting review-related plot snapshot mismatches, with no errors or warnings (138.91 seconds). See `SURVIVAL_REVIEW.md` for the snapshot details and remaining native Qt verification.

## Source readiness

StatsVault search and materialization supplied Hathaway and Tanaka PDFs. Hathaway's full paper and displayed constraint were checked locally; Tanaka's full author manuscript was checked online. Full primary manuscripts were also inspected for He-Chen, Snoussi-Mohammad-Djafari, and Di Mari-Rocci-Gattone. These sources do not establish one regularizer for every supported survival family and censoring scheme.

The canonical-citation tool failed. StatsVault materialized metadata only for [Chen, Li and Tan (2016), gamma-mixture penalized ML](https://doi.org/10.1007/s11425-016-0125-0); publisher full-text retrieval failed. Its abstract confirms a shape-parameter penalty, but its exact penalty and assumptions were not verified. No formula from that paper is proposed here.
