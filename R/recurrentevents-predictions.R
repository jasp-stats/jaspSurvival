.sareMeasures <- function(options) {

  return(.sareMeasureNames[vapply(.sarePredictionOptions, function(key) options[[key]], logical(1))])
}

.sareMeasureLabel <- function(measure) {

  return(switch(measure, survivalProbability = gettext("Survival Probability"), failureProbability = gettext("Failure Probability"),
    cumulativeEvents = gettext("Cumulative Events"), eventIntensity = gettext("Event Intensity"),
    cumulativeMeanTimeBetweenEvents = gettext("Cumulative Mean Time Between Events"), instantaneousMeanTimeBetweenEvents = gettext("Instantaneous Mean Time Between Events")))
}

.sareTimes <- function(record, options, plot = FALSE) {

  message <- gettext("Prediction times must be positive finite numbers. Enter numbers separated by commas.")
  grid <- options[["predictionsTimeGrid"]]
  if (grid == "customAbsolute") {
    times <- sort(unique(.sareNumbers(options[["predictionsTimeValues"]], message, max(record[["time"]]) * c(.25, .5, .75))))
    if (length(times) == 0)
      stop(message)
  } else if (grid == "customRelative") {
    percentages <- .sareNumbers(options[["predictionsTimePercentages"]],
      gettext("Enter positive percentages of maximum follow-up, separated by commas."))
    if (length(percentages) == 0)
      stop(gettext("Enter positive percentages of maximum follow-up, separated by commas."))
    times <- sort(unique(max(record[["time"]]) * percentages / 100))
  } else if (grid == "quantiles") {
    times <- seq(min(record[["time"]]), max(record[["time"]]), length.out = options[["predictionsTimeIntervals"]] + 1L)
  } else if (grid == "sequence") {
    from <- .sareNumbers(options[["predictionsTimeFrom"]], message, min(record[["time"]]))
    to   <- .sareNumbers(options[["predictionsTimeTo"]], message, max(record[["time"]]))
    if (length(from) != 1 || length(to) != 1 || from > to)
      stop(gettext("From and To must each contain one positive time, with From less than or equal to To."))
    step <- .sareNumbers(options[["predictionsTimeStep"]], gettext("Step must contain one positive finite number."),
      if (from == to) from else (to - from) / 10)
    if (length(step) != 1 || (to - from) / step > 10000)
      stop(gettext("Specify one positive step producing no more than 10,001 prediction times."))
    times <- seq(from, to, by = step)
  }
  if (!plot && grid %in% c("quantiles", "sequence") && options[["predictionsRoundTimeTableValues"]]) {
    times <- unique(round(times))
    if (any(times <= 0))
      stop(gettext("Rounding produces a non-positive evaluation time. Disable Round time table values to retain the original times."))
  }
  if (length(times) > 10001)
    stop(gettext("Specify no more than 10,001 prediction times."))
  if (plot && length(times) > 1) {
    range <- range(times)
    grid <- if (options[["predictionsXAxisTransformation"]] == "log")
      exp(seq(log(range[1]), log(range[2]), length.out = 250)) else seq(range[1], range[2], length.out = 250)
    times <- sort(unique(c(times, grid, .sareBreakpoints(record))))
    times <- times[times >= range[1] & times <= range[2]]
  }
  return(unique(times))
}

.sareBreakpoints <- function(record) {

  if (record[["model"]] != "piecewise")
    return(numeric(0))
  return(exp(record[["native"]][["model"]][["psi"]][, "Est."]))
}

.sarePredict <- function(record, times, measure, level) {

  if (measure %in% c("survivalProbability", "failureProbability"))
    return(.sareProbabilityPrediction(.sarePredict(record, times, "cumulativeEvents", level), measure))

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

  if (measure == "cumulativeEvents") {
    estimate <- mean
  } else if (measure == "cumulativeMeanTimeBetweenEvents") {
    estimate <- times / mean
    newLower <- times / upper
    upper    <- times / lower
    lower    <- newLower
  } else {
    coefficients <- record[["estimates"]]
    model        <- record[["model"]]
    if (model == "logLinear") {
      intensity <- exp(coefficients[1] + coefficients[2] * times)
      gradient  <- cbind(1, times)
    } else if (model == "piecewise") {
      slopes    <- native[["betas"]][["log_times"]][, "Est."]
      segment   <- findInterval(log(times), log(.sareBreakpoints(record))) + 1L
      intensity <- mean * slopes[segment] / times
      gradient  <- NULL
    } else if (model == "duane") {
      intensity <- mean * (1 - coefficients[2]) / times
      gradient  <- cbind(-1 / coefficients[1], -1 / (1 - coefficients[2]) - log(times))
    } else {
      intensity <- mean * coefficients[2] / times
      gradient  <- cbind(1 / coefficients[1], 1 / coefficients[2] + log(times))
    }
    estimate <- if (measure == "eventIntensity") intensity else 1 / intensity
    lower <- upper <- rep(NA_real_, length(times))
    if (!is.null(gradient) && record[["inferenceAvailable"]]) {
      variance <- rowSums((gradient %*% record[["covariance"]]) * gradient)
      halfWidth <- .sareCritical(level, record[["df"]]) * sqrt(pmax(variance, 0))
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

.sareProbabilityPrediction <- function(prediction, measure) {

  columns <- c("estimate", "lower", "upper")
  if (measure == "survivalProbability") {
    # The survival transform decreases, so its interval endpoints reverse.
    prediction[columns] <- lapply(prediction[c("estimate", "upper", "lower")], function(value) exp(-value))
  } else {
    # expm1 preserves small probabilities lost by subtracting exp(-M) from one.
    prediction[columns] <- lapply(prediction[columns], function(value) -expm1(-value))
  }
  return(prediction)
}

.sareFittedCumulativeEvents <- function(record) {

  return(if (record[["model"]] == "duane") record[["time"]] / record[["native"]][["Fitted_Values"]] else record[["native"]][["fitted_values"]])
}

.sarePredictionState <- function(jaspResults, options) {

  if (!is.null(jaspResults[["predictions"]]))
    return(jaspResults[["predictions"]]$object)
  state <- createJaspState()
  state$dependOn(c(.sareFitDependencies, .sarePredictionDependencies, "predictionsXAxisTransformation",
    "predictionsOutputPlot", "predictionsOutputTable", "predictionsRoundTimeTableValues"))
  jaspResults[["predictions"]] <- state
  predictions <- list()
  for (measure in .sareMeasures(options)) {
    predictions[[measure]] <- lapply(.sareFits(jaspResults, valid = TRUE), function(record) {
      evaluate <- function(plot) .sarePredict(record, .sareTimes(record, options, plot), measure, options[["predictionsDisplayConfidenceIntervalsLevel"]])
      list(label = record[["label"]],
        table = if (options[["predictionsOutputTable"]]) try(evaluate(FALSE), silent = TRUE),
        plot  = if (options[["predictionsOutputPlot"]]) try(evaluate(TRUE), silent = TRUE))
    })
  }
  state$object <- predictions
  return(predictions)
}

.sarePredictions <- function(jaspResults, options) {

  if (!options[["predictionsOutputPlot"]] && !options[["predictionsOutputTable"]])
    return()
  predictions <- .sarePredictionState(jaspResults, options)
  measures <- .sareMeasures(options)
  if (options[["predictionsOutputTable"]] && length(measures) > 0) {
    if (options[["predictionsMergeTablesAcrossMeasures"]])
      .sarePredictionTable(jaspResults, options, measures, predictions)
    else
      for (measure in measures) .sarePredictionTable(jaspResults, options, measure, predictions)
  }
  for (measure in measures) {
    if (options[["predictionsOutputPlot"]])
      .sarePredictionPlots(jaspResults, options, measure, predictions[[measure]])
  }
  return()
}

.sarePredictionTable <- function(jaspResults, options, measures, predictions) {

  merged <- length(measures) > 1L
  key <- if (merged) "predictionsTable" else paste0(measures, "Table")
  if (!is.null(jaspResults[[key]]))
    return()
  table <- .sareCreateTable(jaspResults, key, if (merged) gettext("Predictions") else .sareMeasureLabel(measures),
    c(.sarePredictionDependencies, .sareTableDependencies, "predictionsOutputTable"),
    if (merged) 4 else 4 + match(measures, .sareMeasureNames) / 10)
  .sareGroupColumn(table, options)
  errors <- character(0)
  forecast <- FALSE
  data <- list()
  for (measure in measures) {
    rows <- list()
    for (prediction in predictions[[measure]]) {
      if (jaspBase::isTryError(prediction[["table"]])) {
        errors <- c(errors, gettextf("%1$s: %2$s", prediction[["label"]], jaspBase::.extractErrorMessage(prediction[["table"]])))
        next
      }
      frame <- prediction[["table"]]
      forecast <- forecast || any(frame[["forecast"]])
      frame[["forecast"]] <- NULL
      if (merged)
        names(frame)[match(c("estimate", "lower", "upper"), names(frame))] <- paste0(measure, ".", c("estimate", "lower", "upper"))
      rows[[length(rows) + 1L]] <- frame
    }
    data[[measure]] <- .saSafeRbind(rows)
  }
  data <- Filter(Negate(is.null), data)
  wide <- options[["predictionsOutputTableFormat"]] == "wide"
  data <- if (length(data) > 0) Reduce(function(x, y) merge(x, y, by = c("subgroup", "time"), all = TRUE, sort = FALSE), data)
  times <- if (wide && !is.null(data)) sort(unique(data[["time"]]))
  if (!wide)
    table$addColumnInfo(name = "time", title = gettext("Time"), type = "number")
  .sarePredictionTableColumns(table, options, measures, times)
  if (!is.null(data)) {
    labels <- vapply(.sareFits(jaspResults, valid = TRUE), `[[`, character(1), "label")
    data <- data[order(match(data[["subgroup"]], labels), data[["time"]]), , drop = FALSE]
    if (wide)
      data <- .sarePredictionWideData(data, measures, times)
    else {
      bounds <- grep("(lower|upper)$", names(data))
      names(data)[bounds] <- paste0(names(data)[bounds], "1")
    }
    table$setData(data)
  }
  if (length(errors) > 0 && is.null(data))
    table$setError(paste(errors, collapse = "\n"))
  else
    for (error in errors) table$addFootnote(error)
  if (any(measures %in% c("survivalProbability", "failureProbability"))) {
    table$addFootnote(gettext("Probabilities assume an NHPP: survival is no events and failure is at least one event since time zero."))
    if (options[["model"]] == "duane")
      table$addFootnote(gettext("Duane probabilities assume its fitted growth curve is the cumulative mean of an NHPP."))
  }
  if (options[["predictionsDisplayConfidenceIntervals"]]) {
    table$addFootnote(gettext("Intervals describe uncertainty in the fitted quantity, not variation in future event counts."))
    if (options[["model"]] == "piecewise" && any(measures %in% c("eventIntensity", "instantaneousMeanTimeBetweenEvents")))
      table$addFootnote(gettext("Confidence intervals for rate-based measures are unavailable for piecewise models."))
  }
  if (forecast)
    table$addFootnote(gettext("Predictions beyond observed follow-up extrapolate the fitted trend."))
  return()
}

.sarePredictionTableColumns <- function(table, options, measures, times = NULL) {

  specification <- list(levels = if (options[["predictionsDisplayConfidenceIntervals"]]) options[["predictionsDisplayConfidenceIntervalsLevel"]] else numeric(0), direction = "twoSided")
  for (measure in measures) {
    prefix <- if (length(measures) > 1L) paste0(measure, ".") else ""
    title <- .sareMeasureLabel(measure)
    if (is.null(times)) {
      .sapPredictionTableColumns(table, specification, title, prefix)
    } else {
      for (i in seq_along(times))
        .sapPredictionTableColumns(table, specification,
          .sapPredictionTableEvaluationTitle(title, "time", times[i]), paste0(prefix, "at", i, "."))
    }
  }
  return()
}

.sarePredictionWideData <- function(data, measures, times) {

  labels <- unique(data[["subgroup"]])
  wide <- data.frame(subgroup = labels)
  for (measure in measures) {
    prefix <- if (length(measures) > 1L) paste0(measure, ".") else ""
    for (i in seq_along(times)) {
      rows <- data[data[["time"]] == times[i], , drop = FALSE]
      selected <- match(labels, rows[["subgroup"]])
      for (field in c("estimate", "lower", "upper"))
        wide[[paste0(prefix, "at", i, ".", if (field == "estimate") "estimate" else paste0(field, "1"))]] <- rows[[paste0(prefix, field)]][selected]
    }
  }
  return(wide)
}
