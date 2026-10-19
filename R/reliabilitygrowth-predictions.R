.sargMeasures <- function(options) {
  values <- c("cumulativeFailures", "failureIntensity", "cumulativeMtbf", "instantaneousMtbf")
  values[vapply(.sargPredictionOptions, function(key) options[[key]], logical(1))]
}

.sargMeasureLabel <- function(measure) {
  switch(measure, cumulativeFailures = gettext("Cumulative Failures"), failureIntensity = gettext("Failure Intensity"),
    cumulativeMtbf = gettext("Cumulative MTBF"), instantaneousMtbf = gettext("Instantaneous MTBF"))
}

.sargTimes <- function(record, options, plot = FALSE) {

  message <- gettext("Prediction times must be positive finite numbers. Enter numbers separated by commas.")
  if (options[["predictionsTimeGrid"]] == "custom") {
    times <- sort(unique(.sargNumbers(options[["predictionsTimeValues"]], message)))
    if (length(times) == 0)
      stop(message)
  } else {
    from <- .sargNumbers(options[["predictionsTimeFrom"]], message, min(record[["time"]]))
    to   <- .sargNumbers(options[["predictionsTimeTo"]], message, max(record[["time"]]))
    if (length(from) != 1 || length(to) != 1 || from > to)
      stop(gettext("From and To must each contain one positive time, with From less than or equal to To."))
    if (options[["predictionsTimeGrid"]] == "sequence") {
      step <- .sargNumbers(options[["predictionsTimeStep"]], gettext("Step must contain one positive finite number."),
        if (from == to) from else (to - from) / 10)
      if (length(step) != 1 || (to - from) / step > 10000)
        stop(gettext("Specify one positive step producing no more than 10,001 prediction times."))
      times <- seq(from, to, by = step)
    } else {
      times <- seq(from, to, length.out = options[["predictionsTimeIntervals"]] + 1L)
    }
  }
  if (length(times) > 10001)
    stop(gettext("Specify no more than 10,001 prediction times."))
  if (plot && length(times) > 1) {
    range <- range(times)
    grid <- if (options[["predictionsXAxisTransformation"]] == "log")
      exp(seq(log(range[1]), log(range[2]), length.out = 250)) else seq(range[1], range[2], length.out = 250)
    times <- sort(unique(c(times, grid, .sargBreakpoints(record))))
    times <- times[times >= range[1] & times <= range[2]]
  }
  return(unique(times))
}

.sargBreakpoints <- function(record) {
  if (record[["model"]] != "piecewise")
    return(numeric(0))
  return(exp(record[["native"]][["model"]][["psi"]][, "Est."]))
}

.sargPredict <- function(record, times, measure, level) {

  native <- record[["native"]]
  # The package warns about evaluating inside the observed range; this is expected
  # for fitted curves. Other prediction warnings remain visible.
  prediction <- withCallingHandlers({
    if (record[["model"]] == "duane")
      ReliaGrowR::predict_duane(native, times, conf_level = level)
    else if (record[["model"]] == "piecewise")
      ReliaGrowR::predict_rga(native, times, conf_level = level)
    else
      ReliaGrowR::predict_nhpp(native, times / record[["scale"]], conf_level = level)
  }, warning = function(warning) {
    if (grepl("Hindcasting is allowed", conditionMessage(warning), fixed = TRUE))
      invokeRestart("muffleWarning")
  })
  if (record[["model"]] == "duane") {
    mean  <- times / prediction[["mtbf"]]
    lower <- times / prediction[["upper_bounds"]]
    upper <- times / prediction[["lower_bounds"]]
  } else {
    mean  <- prediction[[if (record[["model"]] == "piecewise") "cum_failures" else "cum_events"]]
    lower <- prediction[["lower_bounds"]]
    upper <- prediction[["upper_bounds"]]
  }

  coefficients <- record[["estimates"]]
  model <- record[["model"]]
  if (model == "logLinear") {
    intensity <- exp(coefficients[1] + coefficients[2] * times)
    gradient  <- cbind(1, times)
  } else if (model == "piecewise") {
    slopes <- native[["betas"]][["log_times"]][, "Est."]
    segment <- findInterval(log(times), log(.sargBreakpoints(record))) + 1L
    intensity <- mean * slopes[segment] / times
    gradient <- NULL
  } else if (model == "duane") {
    intensity <- mean * (1 - coefficients[2]) / times
    gradient  <- cbind(-1 / coefficients[1], -1 / (1 - coefficients[2]) - log(times))
  } else {
    intensity <- mean * coefficients[2] / times
    gradient  <- cbind(1 / coefficients[1], 1 / coefficients[2] + log(times))
  }

  if (measure == "cumulativeFailures") {
    estimate <- mean
  } else if (measure == "cumulativeMtbf") {
    estimate <- times / mean
    newLower <- times / upper
    upper    <- times / lower
    lower    <- newLower
  } else {
    estimate <- if (measure == "failureIntensity") intensity else 1 / intensity
    lower <- upper <- rep(NA_real_, length(times))
    if (!is.null(gradient) && record[["inferenceAvailable"]]) {
      variance <- rowSums((gradient %*% record[["covariance"]]) * gradient)
      halfWidth <- .sargCritical(level, record[["df"]]) * sqrt(pmax(variance, 0))
      lower <- exp(log(estimate) - halfWidth)
      upper <- exp(log(estimate) + halfWidth)
    }
  }
  if (!record[["inferenceAvailable"]])
    lower <- upper <- rep(NA_real_, length(times))
  if (any(!is.finite(estimate) | estimate <= 0))
    stop(gettext("The model cannot provide finite positive predictions at these times. Reduce the prediction range or choose another model."))
  return(data.frame(time = times, estimate = as.numeric(estimate), lower = as.numeric(lower), upper = as.numeric(upper),
    subgroup = record[["label"]], forecast = times > max(record[["time"]])))
}

.sargObserved <- function(record) {
  return(if (record[["model"]] == "duane") record[["time"]] / record[["native"]][["Fitted_Values"]] else record[["native"]][["fitted_values"]])
}

.sargResiduals <- function(record) {
  if (record[["method"]] == "LS")
    return(as.numeric(stats::residuals(record[["native"]][["model"]])))
  mean <- diff(c(0, .sargObserved(record)))
  return((record[["counts"]] - mean) / sqrt(mean))
}

.sargPredictionState <- function(jaspResults, options) {

  if (!is.null(jaspResults[["predictions"]]))
    return(jaspResults[["predictions"]]$object)
  state <- createJaspState()
  state$dependOn(c(.sargFitDependencies, .sargPredictionDependencies, "predictionsXAxisTransformation",
    "predictionsOutputPlot", "predictionsOutputTable"))
  jaspResults[["predictions"]] <- state
  predictions <- list()
  for (measure in .sargMeasures(options)) {
    predictions[[measure]] <- lapply(.sargFits(jaspResults, valid = TRUE), function(record) {
      evaluate <- function(plot) .sargPredict(record, .sargTimes(record, options, plot), measure, options[["predictionsConfidenceIntervalsLevel"]])
      list(label = record[["label"]],
        table = if (options[["predictionsOutputTable"]]) try(evaluate(FALSE), silent = TRUE),
        plot  = if (options[["predictionsOutputPlot"]]) try(evaluate(TRUE), silent = TRUE))
    })
  }
  state$object <- predictions
  return(predictions)
}

.sargPredictions <- function(jaspResults, options) {

  if (!options[["predictionsOutputPlot"]] && !options[["predictionsOutputTable"]])
    return()
  predictions <- .sargPredictionState(jaspResults, options)
  for (measure in .sargMeasures(options)) {
    if (options[["predictionsOutputTable"]])
      .sargPredictionTable(jaspResults, options, measure, predictions[[measure]])
    if (options[["predictionsOutputPlot"]])
      .sargPredictionPlots(jaspResults, options, measure, predictions[[measure]])
  }
  return()
}

.sargPredictionTable <- function(jaspResults, options, measure, predictions) {

  key <- paste0(measure, "Table")
  if (!is.null(jaspResults[[key]]))
    return()
  table <- .sargCreateTable(jaspResults, key, .sargMeasureLabel(measure),
    c(.sargPredictionDependencies, "predictionsOutputTable"), 4 + match(measure, c("cumulativeFailures", "failureIntensity", "cumulativeMtbf", "instantaneousMtbf")) / 10)
  .sargGroupColumn(table, options)
  .sargColumn(table, "time", gettext("Time"))
  .sargColumn(table, "estimate", gettext("Estimate"))
  if (options[["predictionsConfidenceIntervals"]])
    .sargCiColumns(table, options[["predictionsConfidenceIntervalsLevel"]])
  errors <- character(0)
  data <- lapply(predictions, function(prediction) {
    if (inherits(prediction[["table"]], "try-error")) {
      errors <<- c(errors, gettextf("%1$s: %2$s", prediction[["label"]], jaspBase::.extractErrorMessage(prediction[["table"]])))
      return(NULL)
    }
    prediction[["table"]]
  })
  data <- Filter(Negate(is.null), data)
  if (length(data) > 0)
    table$setData(do.call(rbind, data))
  if (length(errors) > 0 && length(data) == 0)
    table$setError(paste(errors, collapse = "\n"))
  else
    for (error in errors) table$addFootnote(error)
  if (options[["predictionsConfidenceIntervals"]]) {
    table$addFootnote(gettext("Intervals describe uncertainty in the fitted quantity, not variation in future failure counts."))
    if (options[["model"]] == "piecewise" && measure %in% c("failureIntensity", "instantaneousMtbf"))
      table$addFootnote(gettext("Confidence intervals for rate-based measures are unavailable for piecewise models."))
  }
  if (length(data) > 0 && any(vapply(data, function(x) any(x[["forecast"]]), logical(1))))
    table$addFootnote(gettext("Predictions beyond observed follow-up extrapolate the fitted trend."))
  return()
}
