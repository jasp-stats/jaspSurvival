# Reference grids are averaged on each native parameter's linear predictor.
.sapPredictionCustomValues <- function(options) {

  variables  <- .sapOptionsVariables(options)
  values     <- stats::setNames(rep("", length(variables)), variables)
  unassigned <- setdiff(variables, .sapPredictionTableVariables(options))
  if (options[["predictionsUnassignedReferenceValues"]] != "custom" || !length(unassigned))
    return(values)

  data <- .sapCustomPredictionManual(options[["predictionsUnassignedReferenceValuesCustomValues"]], unassigned)
  if (nrow(data) != 1L)
    stop(gettext("Custom reference values require exactly one row."))
  values[unassigned] <- vapply(data, function(value) trimws(value[1L]), character(1))
  return(values)
}

.sapPredictionReferenceValue <- function(value, variable, options, weights, referenceLevel) {

  mode <- options[["predictionsUnassignedReferenceValues"]]
  # Population predictions retain full fitting profiles; this only seeds a scenario.
  if (mode == "population")
    return(value[1L])

  text <- .sapPredictionCustomValues(options)[[variable]]
  if (is.factor(value)) {
    level <- if (mode == "custom" && nzchar(text)) text else referenceLevel
    if (!level %in% levels(value))
      stop(gettextf("Reference value for %1$s must be an existing factor level.", jaspBase::decodeColNames(variable)))
    return(factor(level, levels = levels(value), ordered = is.ordered(value)))
  }
  number <- if (mode == "reference") 0 else if (mode == "custom" && nzchar(text)) .sapNumericExpression(text) else .sapWeightedMean(value, weights)
  if (!.sapFiniteNumeric(number, expectedLength = 1L))
    stop(gettextf("Reference value for %1$s must be one finite number or expression.", jaspBase::decodeColNames(variable)))
  return(as.numeric(number))
}

.sapPredictionMarginalFactors <- function(fit, options, used) {

  if (options[["predictionsUnassignedReferenceValues"]] == "reference")
    return(character(0))

  training  <- attr(fit, "dataset")
  variables <- setdiff(.sapModelVariables(attr(fit, "modelTerms")), used)
  factors   <- variables[vapply(training[variables], is.factor, logical(1))]
  if (options[["predictionsUnassignedReferenceValues"]] == "custom") {
    values  <- .sapPredictionCustomValues(options)
    factors <- factors[!nzchar(values[factors])]
  }
  return(factors)
}

.sapPredictionPointCoefficients <- function(fit) {

  return(matrix(fit[["res.t"]][, "est"], nrow = 1L, dimnames = list(NULL, rownames(fit[["res.t"]]))))
}

.sapPredictionPointParameters <- function(fit, newdata = NULL, X = NULL) {

  # Native normboot checks covariance even with supplied point coefficients.
  # This unused local sentinel leaves cached inference metadata unchanged.
  fit[["cov"]] <- matrix(0, 1L, 1L)
  return(withCallingHandlers(flexsurv::normboot.flexsurvreg(fit, B = 1L, newdata = newdata, X = X,
    rawsim = .sapPredictionPointCoefficients(fit), transform = FALSE), warning = function(warning) {
      if (.sapRetainedFactorContrastWarning(warning, fit))
        invokeRestart("muffleWarning")
    }))
}

# Average design rows over unassigned factor levels on the link scale.
.sapPredictionConditionDesign <- function(fit, data, options, used) {

  if (fit[["ncovs"]] == 0L)
    return(matrix(numeric(0), nrow = nrow(data), ncol = 0L))

  factors  <- .sapPredictionMarginalFactors(fit, options, used)
  training <- attr(fit, "dataset")

  # Expand unassigned factors equally, then average their native design rows per scenario.
  grid     <- if (length(factors)) expand.grid(lapply(training[factors], levels), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) else data.frame(.grid = 1L)
  rows     <- rep(seq_len(nrow(data)), each = nrow(grid))
  expanded <- data[rows, , drop = FALSE]
  for (variable in factors)
    expanded[[variable]] <- factor(
      rep(grid[[variable]], nrow(data)),
      levels  = levels(training[[variable]]),
      ordered = is.ordered(training[[variable]])
    )
  for (transformations in .sapModelTransformations(attr(fit, "modelTerms")))
    for (variable in names(transformations)) {
      if (any(.sapTransformationInvalid(as.numeric(as.character(expanded[[variable]])), transformations[[variable]])))
        stop(gettextf(
          "The reference value of %1$s is outside its transformation domain. Choose Average or valid Custom values.",
          jaspBase::decodeColNames(variable)
        ))
    }

  # Transform the expanded profiles with the fitted basis before averaging.
  native   <- .sapPredictionPointParameters(fit, newdata = expanded)
  design   <- attr(native, "X")
  averaged <- rowsum(design, rows, reorder = FALSE) / nrow(grid)
  rownames(averaged) <- NULL
  return(averaged)
}

# Attach either a reference design or weighted observed profiles for prediction.
.sapPredictionConditionedFit <- function(fit, data, options, used) {

  if (options[["predictionsUnassignedReferenceValues"]] == "population") {
    attr(fit, "predictionPopulation") <- .sapPopulationDesigns(fit, data, options, used)
    times <- .saExtractSurvTimes(attr(fit, "dataset"), options)
    times <- times[is.finite(times) & times > 0]
    attr(fit, "populationAnchor") <- if (length(times)) log(stats::median(times)) else 0
    attr(fit, "predictionX") <- NULL
    return(fit)
  }

  # Ordinary reference predictions use a single averaged design per scenario.
  attr(fit, "predictionPopulation") <- NULL
  attr(fit, "predictionX") <- .sapPredictionConditionDesign(fit, data, options, used)
  attr(fit, "predictionLabels") <- NULL
  return(fit)
}

.sapPredictionUsedVariables <- function(options, xVariable = options[["predictionsXAxisVariables"]]) {

  if (options[["predictionsOutputPlotType"]] == "contour")
    return(.sapPredictionTableVariables(options))
  return(setdiff(
    unique(c(xVariable, options[["predictionsSeparateLines"]], options[["predictionsSeparatePlots"]])),
    options[["predictionsLifetimeTimeId"]]
  ))
}

# Evaluate point predictions in scenario-major, then time-major order.
.sapPredictionPointSummary <- function(fit, data, type, at, transform = identity, fn = NULL) {

  parameters <- .sapPredictionPointParameters(fit, X = attr(fit, "predictionX"))
  parameters <- if (is.matrix(parameters)) parameters[rep(1L, nrow(data)), , drop = FALSE] else do.call(rbind, parameters)
  nt         <- length(at)
  expanded   <- parameters[rep(seq_len(nrow(parameters)), each = nt), fit[["dlist"]][["pars"]], drop = FALSE]
  if (is.null(fn))
    fn <- .sapNativePredictionFunction(fit, type)
  estimates <- transform(do.call(fn, c(list(t = rep(at, times = nrow(data)), start = 0), as.list(as.data.frame(expanded)), fit[["aux"]])))
  result    <- lapply(seq_len(nrow(data)), function(i) {
    index <- (i - 1L) * nt + seq_len(nt)
    data.frame(time = at, est = estimates[index], lcl = NA_real_, ucl = NA_real_)
  })
  attr(result, "X") <- attr(fit, "predictionX")
  return(result)
}

# Native link coefficients map to the same parameters for points and draws.
# The equivalent flexsurv 2.3.2 normboot adapter needs an unused covariance-guard
# bypass for supplied draws, list stacking and no-covariate profile replication.
# It was slower at 100 profiles/B=128; retain this short bulk tcrossprod projection.
.sapPredictionParameters <- function(fit, X, rawsim) {

  stopifnot(ncol(X) == fit[["ncoveffs"]])
  parameters <- fit[["dlist"]][["pars"]]
  values     <- matrix(NA_real_, nrow(rawsim) * nrow(X), length(parameters), dimnames = list(NULL, parameters))
  for (i in seq_along(parameters)) {
    parameter <- parameters[i]
    linear    <- matrix(rawsim[, parameter], nrow(rawsim), nrow(X))
    columns   <- fit[["mx"]][[parameter]]
    if (length(columns))
      linear <- linear + tcrossprod(rawsim[, fit[["covpars"]][columns], drop = FALSE], X[, columns, drop = FALSE])
    values[, i] <- fit[["dlist"]][["inv.transforms"]][[i]](as.vector(linear))
  }
  return(values)
}
