# Minimum component standard deviation on the log-time scale.
# Bounds are supplied to flexsurvreg on its transformed parameter scale.

.sapmConstraintSpec <- function(options, distribution, dataset, modelTerms) {

  if (!options[["mixtureConstrainMinimumSpread"]])
    return(NULL)

  if (!distribution %in% c("lnorm", "weibull", "llogis", "gamma"))
    stop(gettextf("A minimum log-time standard deviation is not available for %1$s. Select log-normal, Weibull, log-logistic, or gamma.", .sapOption2DistributionName(distribution)))

  referenceSd <- NULL
  if (options[["mixtureConstrainMinimumSpreadType"]] == "relative") {
    referenceSd <- .sapmReferenceLogTimeSd(dataset, options, distribution, modelTerms)
    epsilon     <- options[["mixtureMinimumLogTimeSdRelative"]] * referenceSd
  } else {
    epsilon <- options[["mixtureMinimumLogTimeSd"]]
  }

  bound <- switch(distribution,
    "lnorm"   = epsilon,
    "weibull" = pi / (sqrt(6) * epsilon),
    "llogis"  = pi / (sqrt(3) * epsilon),
    "gamma"   = .sapmGammaShapeBound(epsilon)
  )

  return(list(
    family            = distribution,
    spreadType        = options[["mixtureConstrainMinimumSpreadType"]],
    relativePercent   = if (options[["mixtureConstrainMinimumSpreadType"]] == "relative") 100 * options[["mixtureMinimumLogTimeSdRelative"]] else NULL,
    referenceLogTimeSd = referenceSd,
    minimumLogTimeSd   = epsilon,
    parameter         = if (distribution == "lnorm") "sdlog" else "shape",
    direction         = if (distribution == "lnorm") "lower" else "upper",
    naturalBound      = bound
  ))
}
.sapmReferenceLogTimeSd <- function(dataset, options, distribution, modelTerms) {

  # Use the same likelihood and predictors, without the component spread constraint.
  reference <- try(suppressWarnings(flexsurv::flexsurvreg(
    formula = .sapGetFormula(options, modelTerms),
    data    = dataset,
    dist    = distribution,
    weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]],
    hessian = FALSE
  )), silent = TRUE)
  if (jaspBase::isTryError(reference))
    stop(gettext("The one-component reference model could not be fitted. Try a different distribution or use an absolute minimum log-time standard deviation."))

  estimates   <- reference[["res"]][, "est"]
  referenceSd <- switch(distribution,
    "lnorm"   = estimates[["sdlog"]],
    "weibull" = pi / (sqrt(6) * estimates[["shape"]]),
    "llogis"  = pi / (sqrt(3) * estimates[["shape"]]),
    "gamma"   = sqrt(trigamma(estimates[["shape"]]))
  )
  if (reference[["opt"]][["convergence"]] != 0 || !is.finite(reference[["loglik"]]) || !is.finite(referenceSd) || referenceSd <= 0)
    stop(gettext("The one-component reference model did not yield a converged, positive log-time standard deviation. Try a different distribution or use an absolute minimum log-time standard deviation."))

  return(unname(referenceSd))
}
.sapmGammaShapeBound <- function(epsilon) {

  # trigamma(shape) is decreasing. Solve in log-shape for relative precision.
  a     <- -2 * log(epsilon)
  b     <- -log(epsilon)
  lower <- max(a, b)
  upper <- lower + log1p(exp(-abs(a - b)))
  if (lower == upper)
    return(exp(lower))
  root  <- stats::uniroot(function(x) log(trigamma(exp(x))) - 2 * log(epsilon),
                          lower = lower, upper = upper, tol = .Machine$double.eps)$root

  return(exp(root))
}
.sapmConstraintBounds <- function(constraint, family, components, parameterCount) {

  index <- (seq_len(components) - 1L) * length(family[["pars"]]) + match(constraint[["parameter"]], family[["pars"]])
  value <- family[["transforms"]][[match(constraint[["parameter"]], family[["pars"]])]](constraint[["naturalBound"]])
  lower <- rep(-Inf, parameterCount)
  upper <- rep( Inf, parameterCount)
  if (constraint[["direction"]] == "lower")
    lower[index] <- value
  else
    upper[index] <- value

  return(list(lower = lower, upper = upper, index = index, value = value))
}
.sapmFeasibleInits <- function(inits, constraint, family, components) {

  if (is.null(constraint))
    return(inits)

  index <- .sapmConstraintBounds(constraint, family, components, length(inits))[["index"]]
  if (constraint[["direction"]] == "lower")
    inits[index] <- pmax(inits[index], constraint[["naturalBound"]])
  else
    inits[index] <- pmin(inits[index], constraint[["naturalBound"]])

  return(inits)
}
.sapmConstraintPoint <- function(inits, constraint, family, components) {

  bounds <- .sapmConstraintBounds(constraint, family, components, length(inits))
  values <- family[["transforms"]][[match(constraint[["parameter"]], family[["pars"]])]](inits[bounds[["index"]]])
  tolerance <- 64 * .Machine$double.eps * max(1, abs(bounds[["value"]]))
  feasible  <- all(is.finite(values)) && if (constraint[["direction"]] == "lower")
    all(values >= bounds[["value"]] - tolerance) else all(values <= bounds[["value"]] + tolerance)

  return(list(
    feasible = feasible,
    active   = is.finite(values) & abs(values - bounds[["value"]]) <= 1e-6,
    index    = bounds[["index"]]
  ))
}
.sapmConstraintInfo <- function(fit, constraint, family, components) {

  if (is.null(constraint))
    return(NULL)

  point <- .sapmConstraintPoint(fit[["res"]][, "est"], constraint, family, components)
  if (!point[["feasible"]])
    stop(gettext("The fitted parameters do not satisfy the minimum log-time standard deviation. Results cannot be reported reliably."))

  parameters <- rownames(fit[["res"]])[point[["index"]]]
  return(c(constraint, list(
    boundedParameters  = parameters,
    boundaryParameters = parameters[point[["active"]]],
    active             = any(point[["active"]])
  )))
}
.sapmApplyConstraintInference <- function(fit, info) {

  attr(fit, "constraints") <- info
  if (is.null(info))
    return(fit)

  parameters <- rownames(fit[["res.t"]])
  missingCovariance <- !is.matrix(fit[["cov"]]) || !identical(dim(fit[["cov"]]), rep(length(parameters), 2))
  if (info[["active"]] || missingCovariance) {
    # An ordinary inverse Hessian does not describe inference at a constraint boundary.
    fit[["cov"]] <- matrix(NA_real_, length(parameters), length(parameters), dimnames = list(parameters, parameters))
    fit[["res"]][, colnames(fit[["res"]]) != "est"] <- NA_real_
    fit[["res.t"]][, colnames(fit[["res.t"]]) != "est"] <- NA_real_
    if (!is.null(fit[["opt"]][["hessian"]]))
      fit[["opt"]][["hessian"]][] <- NA_real_
  }

  return(fit)
}
.sapmConstraintDistribution <- function(distribution, constraint, family) {

  dlist       <- flexsurv::flexsurv.dists[[distribution]]
  nativeInits <- dlist[["inits"]]
  dlist[["inits"]] <- function(t, mf, mml, aux) {
    arguments <- list(t = t, mf = mf, mml = mml, aux = aux)
    initialize <- function() do.call(nativeInits, arguments[intersect(names(arguments), names(formals(nativeInits)))])
    repair <- function(inits) {
      feasible <- .sapmFeasibleInits(inits, constraint, family, 1L)
      if (distribution == "gamma") {
        # Preserve the native moment initializer's mean when its shape is projected.
        # Exact ties give Inf/Inf, for which the same native mean(t) remains defined.
        meanTime <- if (all(is.finite(inits[1:2]))) inits[1] / inits[2] else mean(t)
        feasible[2] <- feasible[1] / meanTime
      }
      return(feasible)
    }
    inits <- try(repair(initialize()), silent = TRUE)
    if (jaspBase::isTryError(inits) || any(!is.finite(inits))) {
      # Ties can leave native starts undefined. A feasible median-based start
      # initializes the model; flexsurvreg still estimates every parameter.
      medianTime <- stats::median(t[is.finite(t) & t > 0])
      bound      <- constraint[["naturalBound"]]
      inits <- switch(distribution,
        "lnorm"   = c(log(medianTime), bound),
        "weibull" = c(bound, medianTime / stats::qweibull(0.5, shape = bound, scale = 1)),
        "llogis"  = c(bound, medianTime),
        "gamma"   = c(bound, bound / medianTime)
      )
    }
    return(inits)
  }

  return(dlist)
}
.sapmFitSingle <- function(dataset, options, distribution, modelTerms) {

  constraint <- .sapmConstraintSpec(options, distribution, dataset, modelTerms)
  family     <- .sapmFamily(distribution)
  formula    <- .sapGetFormula(options, modelTerms)
  weights    <- if (options[["weights"]] != "") dataset[[options[["weights"]]]] else rep(1, nrow(dataset))
  dlist      <- .sapmConstraintDistribution(distribution, constraint, family)

  # Native initialization with only the feasibility repair above; no preliminary optimization.
  pilotCall <- list(formula = formula, data = dataset, dist = dlist,
                    method = "BFGS", hessian = FALSE, control = list(maxit = 0))
  if (options[["weights"]] != "")
    pilotCall[["weights"]] <- weights
  pilot <- suppressWarnings(do.call(flexsurv::flexsurvreg, pilotCall))
  descriptor <- list(dlist = flexsurv::flexsurv.dists[[distribution]], dfns = NULL)
  native <- .sapmNativeFit(formula, dataset, options, family, 1L, descriptor,
                           unname(pilot[["res"]][, "est"]), weights, constraint = constraint)
  if (jaspBase::isTryError(native[["fit"]]))
    stop(jaspBase::.extractErrorMessage(native[["fit"]]))
  candidate <- native[["fit"]]
  selectedWarnings <- native[["warnings"]]
  if (candidate[["opt"]][["convergence"]] != 0)
    stop(gettext("The constrained optimizer did not converge. Try a different distribution or a simpler model."))

  inits <- unname(candidate[["res"]][, "est"])
  native <- .sapmNativeFit(formula, dataset, options, family, 1L, descriptor,
                           inits, weights, hessian = TRUE, constraint = constraint)
  if (jaspBase::isTryError(native[["fit"]]))
    stop(jaspBase::.extractErrorMessage(native[["fit"]]))
  fit <- native[["fit"]]
  .sapmCheckFinalPoint(fit, inits, candidate[["loglik"]])

  info <- .sapmConstraintInfo(fit, constraint, family, 1L)
  fit  <- .sapmApplyConstraintInference(fit, info)
  attr(fit, "nativeWarnings") <- unique(c(selectedWarnings, native[["warnings"]]))
  attr(fit, "nativeHessianWarning") <- !info[["active"]] &&
    (native[["hessianWarning"]] || !isTRUE(.sapmHessianPositiveDefinite(fit)) || any(!is.finite(fit[["cov"]])))

  return(fit)
}
