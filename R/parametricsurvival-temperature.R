# Boltzmann's constant in eV/K, from the exact SI definitions of k_B and e.
.sapBoltzmannConstantEv <- 1.380649e-23 / 1.602176634e-19

.sapKelvin <- function(value, unit) {

  return(switch(unit, kelvin = value, celsius = value + 273.15, fahrenheit = (value - 32) * 5 / 9 + 273.15))
}

.sapTemperatureExpression <- function(variable, unit) {

  numeric <- paste0("as.numeric(as.character(", variable, "))")
  return(switch(unit, kelvin = numeric, celsius = paste0("(", numeric, " + 273.15)"),
    fahrenheit = paste0("((", numeric, " - 32) * 5/9 + 273.15)")))
}

.sapEyringLogExpression <- function(variable, unit) {

  return(paste(deparse(str2lang(paste0("log(", .sapTemperatureExpression(variable, unit), ")")), width.cutoff = 500L), collapse = ""))
}

.sapTemperatureUnitLabel <- function(unit) {

  return(switch(
    unit,
    kelvin     = gettext("from Kelvin"),
    celsius    = gettext("from Celsius"),
    fahrenheit = gettext("from Fahrenheit")
  ))
}

.sapEquationHasEyring <- function(equation) {

  return(any(vapply(equation[["transformations"]], function(specification) specification[["transformation"]] == "eyring", logical(1))))
}

.sapUsesEyring <- function(options) {

  return(any(vapply(options[["modelTerms"]], .sapEquationHasEyring, logical(1))))
}

.sapCheckEyringModels <- function(models) {

  for (model in models) {
    for (equation in model[["ancillaryEquations"]])
      if (.sapEquationHasEyring(equation))
        .quitAnalysis(gettext("The Eyring preset is available only for the location equation. Choose another transformation for ancillary parameters."))
  }
  return()
}

.sapEyringFixed <- function(dataset, equation, formula, family, parameter) {

  variables <- names(equation[["transformations"]])[vapply(equation[["transformations"]], function(specification)
    specification[["transformation"]] == "eyring", logical(1))]
  if (length(variables) == 0L)
    return(numeric(0))
  if (parameter != family[["location"]] || family[["family"]] == "gompertz")
    stop("The Eyring structural coefficient has no supported native parameter mapping.")
  matrix <- stats::model.matrix(formula, stats::model.frame(formula, dataset))
  labels <- attr(stats::terms(formula), "term.labels")
  fixed  <- numeric(0)
  for (variable in variables) {
    expression <- .sapEyringLogExpression(variable, equation[["transformations"]][[variable]][["temperatureUnit"]])
    index      <- which(attr(matrix, "assign") == match(expression, labels))
    if (length(index) != 1L)
      stop("The Eyring structural term does not match one native design column.")
    fixed[colnames(matrix)[index]] <- if (family[["family"]] %in% c("exp", "gamma")) 1 else -1
  }
  return(fixed)
}

.sapEyringOmissionNote <- function(options) {

  if (.sapUsesEyring(options) && options[["distribution"]] %in% c("all", "bestAic", "bestBic") &&
    options[["selectedParametricDistributionsGompertz"]])
    return(gettext("Gompertz was omitted because it does not support the Eyring specification."))
  return(NULL)
}

# Adjust baseline starts when imposing Eyring's fixed log-temperature term.
.sapEyringInitials <- function(fitCall, schema, fixed) {

  specification <- schema[["specification"]]
  if (length(specification[["structuralFixed"]]) == 0L)
    return(fitCall)
  if (is.null(fitCall[["inits"]]))
    fitCall[["inits"]] <- unname(schema[["fit"]][["res"]][, "est"])

  # Offset the baseline by the weighted mean change in the structural linear predictor.
  adjustments <- list()
  for (parameter in names(specification[["equations"]])) {
    equation   <- specification[["equations"]][[parameter]]
    structural <- equation[["structuralFixed"]]
    if (length(structural) == 0L)
      next
    index      <- match(names(structural), specification[["parameterNames"]])
    difference <- fitCall[["inits"]][index] - structural
    delta      <- 0
    if (any(difference != 0) && !parameter %in% names(fixed)) {
      matrix  <- schema[["fit"]][["data"]][["mml"]][[parameter]]
      columns <- match(names(structural), equation[["slopeNames"]]) + 1L
      weights <- fitCall[["weights"]]
      if (is.null(weights))
        weights <- rep(1, nrow(matrix))
      if (length(weights) != nrow(matrix) || any(!is.finite(weights) | weights < 0) ||
        any(!is.finite(matrix[, columns, drop = FALSE])))
        stop(gettext("The Eyring initial values require finite design columns and finite nonnegative fitting weights."))
      if (!any(weights > 0))
        stop(gettext("The Eyring initial adjustment is undefined when the fitting weights have zero total. Supply at least one positive fitting weight."))
      weights <- weights / max(weights)
      means   <- colSums(matrix[, columns, drop = FALSE] * weights) / sum(weights)
      delta   <- sum(means * difference)
      if (!is.finite(delta))
        stop(gettext("The Eyring structural initial adjustment exceeds the supported numerical range."))
      # Matching warm/final values retain their natural baseline bit for bit.
      if (delta != 0) {
        baseline <- equation[["baselineIndex"]]
        eta      <- equation[["transform"]](fitCall[["inits"]][baseline]) + delta
        value    <- equation[["inverseLink"]](eta)
        if (!is.finite(value) || !is.finite(equation[["transform"]](value)))
          stop(gettext("The Eyring initial baseline cannot be represented on the native parameter scale. Choose a different predictor transformation or scale."))
        fitCall[["inits"]][baseline] <- value
      }
    }
    fitCall[["inits"]][index] <- structural
    adjustments[[parameter]] <- delta
  }
  attr(fitCall, "structuralInitialAdjustment") <- adjustments
  return(fitCall)
}

# Keep the established Eyring search controls. Actual design standardization,
# rather than optimizer parscale alone, now conditions native Hessian conversion.
.sapEyringControls <- function(fitCall, schema) {

  specification <- schema[["specification"]]
  if (length(specification[["structuralFixed"]]) == 0L)
    return(fitCall)

  free <- setdiff(seq_along(specification[["parameterNames"]]), fitCall[["fixedpars"]])
  if (is.null(fitCall[["method"]]))
    fitCall[["method"]] <- "BFGS"
  if (fitCall[["method"]] == "BFGS" && (is.null(fitCall[["control"]][["maxit"]]) || fitCall[["control"]][["maxit"]] != 0)) {
    fitCall[["control"]][["maxit"]] <- 10000
    fitCall[["control"]][["reltol"]] <- 0
    fitCall[["control"]][["ndeps"]] <- rep(1e-5, length(free))
  }
  return(fitCall)
}

