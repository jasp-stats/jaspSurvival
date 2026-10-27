RecurrentEventAnalysis <- function(jaspResults, dataset, options) {

  if (.sareReady(options))
    .sareCheckDataset(dataset, options)
  .sareFit(jaspResults, dataset, options)
  .sareStatistics(jaspResults, dataset, options)
  .sarePredictions(jaspResults, options)
  .sareDiagnostics(jaspResults, options)
  .sareExport(jaspResults, dataset, options)

  return()
}

# Output positions: 1xx statistics, 2xx prediction plots, 3xx prediction tables, 4xx diagnostics, 5xx exports.
.sareFitDependencies <- c("time", "eventCount", "subgroup", "timeType", "model", "estimationMethod",
  "piecewiseStartingBreakpoints", "piecewiseStartingBreakpointsValues", "setSeed", "seed")
# Prediction measures in interface order; the order also sets their output positions.
.sareMeasureOptions <- c(
  survivalProbability                = "predictionsOutcomeSurvivalProbability",
  failureProbability                 = "predictionsOutcomeFailureProbability",
  cumulativeEvents                   = "predictionsOutcomeCumulativeEvents",
  eventIntensity                     = "predictionsOutcomeEventIntensity",
  cumulativeMeanTimeBetweenEvents    = "predictionsOutcomeCumulativeMeanTimeBetweenEvents",
  instantaneousMeanTimeBetweenEvents = "predictionsOutcomeInstantaneousMeanTimeBetweenEvents"
)
.sarePredictionDependencies <- c(unname(.sareMeasureOptions),
  paste0("predictionsTime", c("Grid", "Intervals", "From", "To", "Step", "Values", "Percentages")),
  "predictionsDisplayConfidenceIntervals", "predictionsDisplayConfidenceIntervalsLevel", "confidenceIntervalBounds")

.sareReady <- function(options) {

  return(options[["time"]] != "")
}

.sareModelLabel <- function(model) {

  return(switch(model, duane = gettext("Duane"), crowAmsaa = gettext("Crow\u2013AMSAA"),
    logLinear = gettext("Log-linear NHPP"), piecewise = gettext("Piecewise power-law NHPP")))
}

.sareMethod <- function(options) {

  return(switch(options[["model"]],
    duane     = "LS",
    piecewise = "LS",
    logLinear = "MLE",
    crowAmsaa = if (options[["estimationMethod"]] == "mle") "MLE" else "LS"))
}

.sareCheckDataset <- function(dataset, options) {

  variables <- unique(c(options[["time"]], options[["eventCount"]], unlist(options[["subgroup"]])))
  variables <- variables[variables != ""]
  if (nrow(dataset) == 0)
    .quitAnalysis(gettext("No observations are available."))
  if (any(!stats::complete.cases(dataset[variables])))
    .quitAnalysis(gettext("Time, event counts and subgroup variables cannot contain missing values. Complete the interval records rather than omitting them."))
  time <- dataset[[options[["time"]]]]
  if (any(!is.finite(time) | time <= 0))
    .quitAnalysis(gettext("All times must be finite and greater than zero."))
  if (options[["eventCount"]] != "") {
    counts <- dataset[[options[["eventCount"]]]]
    if (any(!is.finite(counts) | counts < 0 | counts != floor(counts)))
      .quitAnalysis(gettext("Event counts must be finite, non-negative integers."))
    if (any(counts == 0))
      .quitAnalysis(gettext("ReliaGrowR currently requires a positive event count in every interval. Zero-count intervals cannot be removed because they contain exposure information."))
  }
  return()
}

.sareNumbers <- function(text, message, empty = numeric(0)) {

  if (!nzchar(trimws(text)))
    return(empty)
  tokens <- strsplit(trimws(text), "[,;[:space:]]+")[[1]]
  numbers <- suppressWarnings(as.numeric(tokens))
  if (length(numbers) == 0 || any(!is.finite(numbers) | numbers <= 0))
    stop(message)
  return(numbers)
}

.sareFit <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fit"]]))
    return()
  state <- createJaspState()
  state$dependOn(.sareFitDependencies)
  jaspResults[["fit"]] <- state
  state$object <- list()
  if (!.sareReady(options))
    return()

  hasSubgroups <- .sapHasSubgroups(options)
  groups <- if (hasSubgroups) split(seq_len(nrow(dataset)), .sapSubgroupFactor(dataset, options), drop = TRUE) else
    list(seq_len(nrow(dataset)))
  fits <- lapply(seq_along(groups), function(index) {
    rows <- groups[[index]]
    label <- if (hasSubgroups) names(groups)[index] else gettext("Full dataset")
    result <- try(.sareFitModel(dataset[rows, , drop = FALSE], options), silent = TRUE)
    if (jaspBase::isTryError(result))
      result <- list(error = jaspBase::.extractErrorMessage(result))
    result[["label"]] <- label
    result[["rows"]] <- rows
    result
  })
  state$object <- fits
  return()
}

.sareFitModel <- function(dataset, options) {

  time   <- dataset[[options[["time"]]]]
  counts <- if (options[["eventCount"]] == "") rep(1, length(time)) else dataset[[options[["eventCount"]]]]
  if (length(time) < 3)
    stop(gettext("At least three observation intervals are required in each subgroup."))
  if (options[["timeType"]] == "interval")
    time <- cumsum(time)
  if (any(diff(time) <= 0))
    stop(gettext("Cumulative interval endpoints must be strictly increasing within each subgroup. Order the rows by time or choose Interval durations."))

  model  <- options[["model"]]
  method <- .sareMethod(options)
  breaks <- NULL
  if (model == "piecewise") {
    jaspBase::.setSeedJASP(options)
    if (options[["piecewiseStartingBreakpoints"]]) {
      breaks <- .sareNumbers(options[["piecewiseStartingBreakpointsValues"]],
        gettext("Enter positive starting change-point times separated by commas."))
      if (length(breaks) == 0 || any(breaks <= min(time) | breaks >= max(time)) || anyDuplicated(breaks))
        stop(gettext("Starting change points must be distinct and lie strictly between the first and last observed endpoints."))
    }
  }

  # Dimensionless MLE times avoid dependence on whether inputs use hours or seconds.
  # Keep the native fit in its fitting units; transform only reported parameters.
  scale <- if (method == "MLE") max(time) else 1
  warnings <- character(0)
  native <- withCallingHandlers({
    # Native Duane cumulatively sums its time argument, so pass interval durations.
    if (model == "duane")
      ReliaGrowR::duane(diff(c(0, time)), counts)
    else if (model == "piecewise")
      ReliaGrowR::rga(time, counts, times_type = "cumulative_failure_times", model_type = "Piecewise NHPP", breaks = breaks, method = "LS")
    else
      .sareNhpp(time / scale, counts, model, method)
  }, warning = function(warning) {
    warnings <<- c(warnings, conditionMessage(warning))
    invokeRestart("muffleWarning")
  })

  # ReliaGrowR 0.7 uses these internal optimizer bounds; reject boundary solutions.
  if (method == "MLE" && model == "logLinear" &&
      (abs(native[["params"]][["b"]]) >= 5 - 1e-4 || abs(native[["params"]][["a"]]) >= 20 - 1e-4))
    stop(gettext("The log-linear estimate reached a numerical bound in ReliaGrowR. This fit cannot be reported reliably."))
  if (method == "MLE" && model == "crowAmsaa" &&
      (native[["params"]][["beta"]] <= 1e-6 * 1.01 || native[["params"]][["lambda"]] <= 1e-10 * 1.01))
    stop(gettext("The power-law estimate reached a numerical bound in ReliaGrowR. This fit cannot be reported reliably."))

  record <- list(native = native, model = model, method = method, scale = scale, time = time,
    counts = counts, cumulativeCounts = cumsum(counts), warnings = unique(warnings))
  record <- .sareParameters(record)
  fitted <- native[[if (model == "duane") "Fitted_Values" else "fitted_values"]]
  if (any(!is.finite(record[["estimates"]])) || any(!is.finite(fitted)))
    stop(gettext("The model returned non-finite estimates. The fit cannot be reported."))
  covariance <- record[["covariance"]]
  record[["inferenceAvailable"]] <- all(is.finite(covariance)) &&
    all(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values > 0)
  if (!record[["inferenceAvailable"]])
    record[["warnings"]] <- c(record[["warnings"]], gettext("The covariance matrix is singular or not positive definite. Standard errors and confidence intervals are unavailable."))
  return(record)
}

.sareNhpp <- function(time, counts, model, method) {

  return(tryCatch(
    ReliaGrowR::nhpp(time, counts, model_type = if (model == "logLinear") "Log-Linear" else "Power Law", method = method),
    error = function(error) {
      # ReliaGrowR 0.7 differentiates the power law numerically with steps that cross
      # beta = 0 once beta is near zero, so optim fails before reaching its beta bound.
      call <- conditionCall(error)
      if (method == "MLE" && model == "crowAmsaa" && is.call(call) && identical(call[[1L]], quote(stats::optim)))
        stop(gettext("The power-law fit did not converge. ReliaGrowR's optimizer fails when beta is close to zero, which happens when nearly all events occur early in follow-up. Consider the log-linear model or least-squares estimation."), call. = FALSE)
      stop(error)
    }
  ))
}

.sareParameters <- function(record) {

  native <- record[["native"]]
  model  <- record[["model"]]
  scale  <- record[["scale"]]
  if (record[["method"]] == "LS") {
    fit        <- native[["model"]]
    estimates  <- stats::coef(fit)
    covariance <- stats::vcov(fit)
    record[["df"]] <- stats::df.residual(fit)
    if (model == "piecewise") {
      breaks <- fit[["psi"]][, "Est."]
      estimates[rownames(fit[["psi"]])] <- breaks
      record[["positive"]] <- rep(FALSE, length(estimates))
      labels <- names(estimates)
    } else {
      estimates[1] <- exp(estimates[1])
      jacobian <- diag(c(estimates[1], 1))
      covariance <- jacobian %*% covariance %*% jacobian
      record[["positive"]] <- c(TRUE, FALSE)
      labels <- if (model == "duane") c("A", "alpha") else c("lambda", "beta")
    }
  } else if (model == "logLinear") {
    estimates <- c(native[["params"]][["a"]] - log(scale), native[["params"]][["b"]] / scale)
    jacobian <- diag(c(1, 1 / scale))
    covariance <- jacobian %*% native[["vcov"]] %*% jacobian
    record[["positive"]] <- c(FALSE, FALSE)
    labels <- c("a", "b")
    record[["df"]] <- Inf
  } else {
    beta   <- native[["params"]][["beta"]]
    lambda <- native[["params"]][["lambda"]] / scale^beta
    estimates <- c(lambda, beta)
    # Native power-law covariance is ordered beta, lambda; report lambda, beta.
    jacobian <- rbind(c(-lambda * log(scale), scale^(-beta)), c(1, 0))
    covariance <- jacobian %*% native[["vcov"]] %*% t(jacobian)
    record[["positive"]] <- c(TRUE, TRUE)
    labels <- c("lambda", "beta")
    record[["df"]] <- Inf
  }
  names(estimates) <- labels
  dimnames(covariance) <- list(names(estimates), names(estimates))
  record[["estimates"]] <- estimates
  record[["covariance"]] <- covariance
  return(record)
}

.sareFits <- function(jaspResults, valid = FALSE) {

  fits <- jaspResults[["fit"]]$object
  if (valid)
    fits <- Filter(function(fit) is.null(fit[["error"]]), fits)
  return(fits)
}

.sareCritical <- function(level, df, direction) {

  probability <- if (direction == "twoSided") (1 + level) / 2 else level
  return(if (is.finite(df)) stats::qt(probability, df) else stats::qnorm(probability))
}
