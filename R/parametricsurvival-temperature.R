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
  return(switch(unit, kelvin = gettext("from Kelvin"), celsius = gettext("from Celsius"), fahrenheit = gettext("from Fahrenheit")))
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
    if (!.sapEquationHasEyring(model)) next
    equations <- c(list(model), model[["ancillaryEquations"]])
    if (any(vapply(equations, function(equation) any(lengths(equation[["components"]]) > 1L), logical(1))))
      .quitAnalysis(gettext("Eyring models cannot include interactions in any parameter equation. Remove the interaction terms or choose another transformation."))
  }
  return()
}
.sapEyringFixed <- function(dataset, equation, formula, family, parameter) {

  variables <- names(equation[["transformations"]])[vapply(equation[["transformations"]], function(specification)
    specification[["transformation"]] == "eyring", logical(1))]
  if (length(variables) == 0L) return(numeric(0))
  if (parameter != family[["location"]] || family[["family"]] == "gompertz")
    stop("The Eyring structural coefficient has no supported native parameter mapping.")
  matrix <- stats::model.matrix(formula, stats::model.frame(formula, dataset))
  labels <- attr(stats::terms(formula), "term.labels")
  fixed <- numeric(0)
  for (variable in variables) {
    expression <- .sapEyringLogExpression(variable, equation[["transformations"]][[variable]][["temperatureUnit"]])
    index <- which(attr(matrix, "assign") == match(expression, labels))
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
.sapEyringInitials <- function(fitCall, schema, fixed) {

  specification <- schema[["specification"]]
  if (length(specification[["structuralFixed"]]) == 0L)
    return(fitCall)
  if (is.null(fitCall[["inits"]]))
    fitCall[["inits"]] <- unname(schema[["fit"]][["res"]][, "est"])

  adjustments <- list()
  for (parameter in names(specification[["equations"]])) {
    equation <- specification[["equations"]][[parameter]]
    structural <- equation[["structuralFixed"]]
    if (length(structural) == 0L)
      next
    index <- match(names(structural), specification[["parameterNames"]])
    difference <- fitCall[["inits"]][index] - structural
    delta <- 0
    if (any(difference != 0) && !parameter %in% names(fixed)) {
      matrix <- schema[["fit"]][["data"]][["mml"]][[parameter]]
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
      means <- colSums(matrix[, columns, drop = FALSE] * weights) / sum(weights)
      delta <- sum(means * difference)
      if (!is.finite(delta))
        stop(gettext("The Eyring structural initial adjustment exceeds the supported numerical range."))
      # Matching warm/final values retain their natural baseline bit for bit.
      if (delta != 0) {
        baseline <- equation[["baselineIndex"]]
        eta <- equation[["transform"]](fitCall[["inits"]][baseline]) + delta
        value <- equation[["inverseLink"]](eta)
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
.sapEyringControls <- function(fitCall, schema) {

  specification <- schema[["specification"]]
  if (length(specification[["structuralFixed"]]) == 0L)
    return(fitCall)

  scales <- rep(1, length(specification[["parameterNames"]]))
  for (parameter in names(specification[["equations"]])) {
    equation <- specification[["equations"]][[parameter]]
    indices <- equation[["slopeIndices"]]
    if (length(indices) == 0L)
      next
    matrix <- schema[["fit"]][["data"]][["mml"]][[parameter]][, -1L, drop = FALSE]
    for (j in seq_len(ncol(matrix))) {
      column <- matrix[, j]
      if (any(!is.finite(column)))
        stop(gettext("The Eyring optimizer requires finite native design columns."))
      if (length(column) == 0L || all(column == column[1L]))
        next
      maximum <- max(abs(column))
      rms <- maximum * sqrt(mean((column / maximum)^2))
      scale <- 1 / rms
      if (!is.finite(scale) || scale <= 0)
        stop(gettext("The Eyring optimizer scale exceeds the supported numerical range. Choose a different predictor transformation or scale."))
      scales[indices[j]] <- scale
    }
  }
  free <- setdiff(seq_along(scales), fitCall[["fixedpars"]])
  fitCall[["control"]][["parscale"]] <- scales[free]
  if (is.null(fitCall[["method"]]))
    fitCall[["method"]] <- "BFGS"
  if (fitCall[["method"]] == "BFGS" && (is.null(fitCall[["control"]][["maxit"]]) || fitCall[["control"]][["maxit"]] != 0)) {
    fitCall[["control"]][["maxit"]] <- 10000
    fitCall[["control"]][["reltol"]] <- 0
    fitCall[["control"]][["ndeps"]] <- rep(1e-5, length(free))
  }
  attr(fitCall, "fullParameterScales") <- scales
  return(fitCall)
}
.sapEyringUnavailableInference <- function(fit, reason) {

  names <- rownames(fit[["res.t"]])
  free <- setdiff(seq_along(names), fit[["fixedpars"]])
  fit[["cov"]] <- matrix(NA_real_, length(free), length(free), dimnames = list(names[free], names[free]))
  for (field in c("res", "res.t"))
    fit[[field]][, colnames(fit[[field]]) != "est"] <- NA_real_
  attr(fit, "inferenceUnavailable") <- reason
  return(fit)
}
.sapEyringInformation <- function(fit, specification, requestedHessian, censoringType, provenance = NULL) {

  if (is.null(provenance))
    provenance <- list(nativeH = fit[["opt"]][["hessian"]], nativeAttempt = requestedHessian && fit[["npars"]] > 0L,
      nativeCondition = NULL, nativeWarnings = character(0), analyticReason = NULL)
  if (length(specification[["structuralFixed"]]) == 0L)
    return(.sapInformationRecord(fit, "native", "nativeIneligible", provenance))
  if (fit[["npars"]] == 0L) {
    provenance["nativeH"] <- list(NULL)
    provenance[["nativeAttempt"]] <- FALSE
    return(.sapInformationRecord(fit, "native", "fullyFixed", provenance))
  }
  if (!requestedHessian)
    return(.sapInformationRecord(fit, "native", "nativeNoH", provenance))
  names <- specification[["parameterNames"]]
  free <- setdiff(seq_along(names), fit[["fixedpars"]])
  if (length(free) == 0L)
    return(.sapInformationRecord(fit, "native", "fullyFixed", provenance))
  target <- if (specification[["family"]] == "genf.orig" && specification[["components"]] == 1L)
    .sapGenfOrigTarget(fit, specification, censoringType) else NULL
  analytic <- !is.null(target)
  method <- if (analytic) "analyticGenfOrigDensity" else "native"
  unavailable <- function(reason) {
    provenance["analyticReason"] <- list(if (analytic) reason else NULL)
    return(.sapInformationRecord(.sapEyringUnavailableInference(fit, reason), method,
      if (analytic) "analyticUnavailable" else "nativeIneligible", provenance))
  }
  H <- fit[["opt"]][["hessian"]]
  if (analytic) {
    H <- .sapGenfOrigInformation(target)
    fit[["opt"]][["hessian"]] <- H
  }
  if (length(free) == 1L && is.numeric(H) && is.null(dim(H)) && length(H) == 1L)
    H <- matrix(H, 1L, 1L, dimnames = list(names[free], names[free]))
  valid <- is.matrix(H) && identical(dim(H), rep(length(free), 2L)) &&
    identical(rownames(H), names[free]) && identical(colnames(H), names[free]) &&
    all(is.finite(H)) && isSymmetric(H)
  decomposition <- if (valid) try(chol(H), silent = TRUE) else NULL
  if (!valid || inherits(decomposition, "try-error"))
    return(unavailable(if (analytic) gettext("The analytic exact-event information matrix does not support confidence intervals at this fitted point.") else
      gettext("The native information matrix does not support confidence intervals at this fitted point.")))
  covariance <- chol2inv(decomposition)
  dimnames(covariance) <- dimnames(H)
  se <- sqrt(diag(covariance))
  residual <- max(abs((H %*% covariance - diag(length(free))) * outer(se, 1 / se)))
  if (any(!is.finite(covariance)) || any(!is.finite(se) | se <= 0) || !is.finite(residual) || residual > 1e-6)
    return(unavailable(if (analytic) gettext("The analytic exact-event information matrix could not be inverted accurately. Confidence intervals are unavailable.") else
      gettext("The native information matrix could not be inverted accurately. Confidence intervals are unavailable.")))
  fit[["cov"]] <- covariance
  fit[["opt"]][["hessian"]] <- H
  critical <- stats::qnorm((1 + fit[["cl"]]) / 2)
  fit[["res.t"]][, colnames(fit[["res.t"]]) != "est"] <- NA_real_
  fit[["res.t"]][free, "se"] <- se
  fit[["res.t"]][free, 2L] <- fit[["res.t"]][free, "est"] - critical * se
  fit[["res.t"]][free, 3L] <- fit[["res.t"]][free, "est"] + critical * se
  naturalEstimates <- fit[["res"]][, "est"]
  fit[["res"]] <- fit[["res.t"]]
  for (i in seq_along(fit[["dlist"]][["pars"]])) {
    fit[["res"]][i, 1:3] <- fit[["dlist"]][["inv.transforms"]][[i]](fit[["res.t"]][i, 1:3])
    if (identical(body(fit[["dlist"]][["transforms"]][[i]]), body(log)))
      fit[["res"]][i, "se"] <- exp(fit[["res.t"]][i, "est"]) * fit[["res.t"]][i, "se"]
    else if (!identical(fit[["dlist"]][["transforms"]][[i]], identity))
      fit[["res"]][i, "se"] <- NA_real_
  }
  fit[["res"]][, "est"] <- naturalEstimates
  attr(fit, "inferenceUnavailable") <- NULL
  attr(fit, "informationInverseResidual") <- residual
  if (analytic) {
    attr(fit, "nativeHessianWarning") <- FALSE
    mixture <- attr(fit, "mixture", exact = TRUE)
    if (!is.null(mixture)) {
      mixture[["hessianWarning"]] <- FALSE
      attr(fit, "mixture") <- mixture
    }
  }
  return(.sapInformationRecord(fit, method, if (analytic) "analyticAvailable" else "nativeIneligible", provenance))
}
.sapEyringNativeFit <- function(fitCall, schema, censoringType, searchProvenance = NULL) {

  if (is.null(schema) || length(schema[["specification"]][["structuralFixed"]]) == 0L) {
    fit <- do.call(flexsurv::flexsurvreg, fitCall)
    provenance <- list(nativeH = fit[["opt"]][["hessian"]], nativeAttempt = fitCall[["hessian"]] && fit[["npars"]] > 0L,
      nativeCondition = NULL, nativeWarnings = character(0), analyticReason = NULL)
    return(.sapInformationRecord(fit, "native", if (fit[["npars"]] == 0L) "fullyFixed" else
      if (!fitCall[["hessian"]]) "nativeNoH" else "nativeIneligible", provenance))
  }
  requestedHessian <- fitCall[["hessian"]]
  searchCall <- fitCall
  searchCall[["hessian"]] <- FALSE
  point <- do.call(flexsurv::flexsurvreg, searchCall)
  .sapVerifyNativeSchema(point, schema[["specification"]])
  if (is.null(searchProvenance))
    searchProvenance <- list(opt = point[["opt"]], control = searchCall[["control"]],
      loglik = point[["loglik"]], AIC = point[["AIC"]], BIC = point[["BIC"]])
  else
    .sapmCheckFinalPoint(point, fitCall[["inits"]], searchProvenance[["loglik"]])
  fit <- point
  finalControl <- NULL
  provenance <- list(nativeH = NULL, nativeAttempt = FALSE, nativeCondition = NULL,
    nativeWarnings = character(0), analyticReason = NULL)
  if (requestedHessian && point[["npars"]] > 0L) {
    provenance[["nativeAttempt"]] <- TRUE
    finalCall <- fitCall
    finalCall[["inits"]] <- unname(point[["res"]][, "est"])
    finalCall[["method"]] <- "BFGS"
    finalCall[["lower"]] <- finalCall[["upper"]] <- NULL
    finalCall[["control"]] <- list(maxit = 0, parscale = fitCall[["control"]][["parscale"]])
    finalControl <- finalCall[["control"]]
    covarianceError <- NULL
    covarianceFunction <- get(".hess_to_cov", envir = asNamespace("flexsurv"), inherits = FALSE)
    fit <- tryCatch(withCallingHandlers(do.call(flexsurv::flexsurvreg, finalCall), warning = function(warning) {
      if (grepl("hessian|covariance|positive definite|eigen|standard error", conditionMessage(warning), ignore.case = TRUE)) {
        provenance[["nativeWarnings"]] <<- c(provenance[["nativeWarnings"]], conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    }, error = function(error) {
      if (any(vapply(seq_len(sys.nframe()), function(frame)
          identical(sys.function(frame), covarianceFunction), logical(1))))
        covarianceError <<- error
    }), error = function(error) {
      if (is.null(covarianceError)) stop(error)
      provenance[["nativeCondition"]] <<- conditionMessage(error)
      return(.sapEyringUnavailableInference(point,
        gettextf("Native covariance finalization failed: %1$s. Confidence intervals are unavailable.", conditionMessage(error))))
    })
    .sapVerifyNativeSchema(fit, schema[["specification"]])
    .sapmCheckFinalPoint(fit, unname(point[["res"]][, "est"]), point[["loglik"]])
    if (is.null(covarianceError)) provenance["nativeH"] <- list(fit[["opt"]][["hessian"]])
    for (field in c("res", "res.t")) fit[[field]][, "est"] <- point[[field]][, "est"]
  }
  fit <- .sapEyringInformation(fit, schema[["specification"]], requestedHessian, censoringType, provenance)
  if (!identical(attr(fit, "informationOutcome", exact = TRUE), "analyticAvailable"))
    for (message in provenance[["nativeWarnings"]]) warning(message, call. = FALSE)
  for (field in c("convergence", "counts", "message", "value"))
    fit[["opt"]][[field]] <- searchProvenance[["opt"]][[field]]
  for (field in c("loglik", "AIC", "BIC"))
    fit[[field]] <- searchProvenance[[field]]
  attr(fit, "searchProvenance") <- searchProvenance
  attr(fit, "searchControl") <- searchProvenance[["control"]]
  attr(fit, "finalControl") <- finalControl
  attr(fit, "requestedHessian") <- requestedHessian
  return(fit)
}
