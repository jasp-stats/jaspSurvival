# Prediction input stays on the original predictor scale. Defaults and bases
# come from fitting observations, never from prediction rows.
.sapCustomPredictionVariables <- function(options) {

  variables <- unique(c(.sapOptionsVariables(options), unlist(options[["subgroup"]], use.names = FALSE)))
  return(variables[nzchar(variables)])
}

.sapCustomPredictionSpecification <- function(options, fit) {

  measures <- list()
  if (options[["predictionsOutcomeSurvivalTime"]]) {
    for (summary in .sapSelectedLifetimeSummaries(options)) {
      if (summary == "quantile") {
        percentiles <- .sapOptions2PredictionQuantile(options)
        measures    <- c(measures, lapply(percentiles, function(percentile) list(type = "quantile", probability = percentile,
          label = gettextf("Percentile %1$s", 100 * percentile), kind = "lifetime", id = "quantile")))
      } else measures[[length(measures) + 1L]] <- list(type = summary, label = .sapPredictionTitle(summary, options), kind = "lifetime", id = summary)
    }
  }
  selected <- vapply(.sapModelPredictionTimeOptions, function(name) options[[name]], logical(1))
  times    <- if (any(selected)) .sapModelPredictionAt(fit, options, "survival", "x") else numeric(0)
  ids      <- setdiff(names(.sapPredictionTypes), c("survivalTime", "relativeSurvivalTime"))
  for (id in ids[selected]) {
    type <- .sapPredictionTypes[[id]]
    measures[[length(measures) + 1L]] <- list(
      type = type, times = times, label = .sapPredictionTitle(type, options, id), kind = "time", id = id
    )
  }
  if (!length(measures)) stop(gettext("Select a mean, percentile or time-evaluated measure for predictions."))
  levels <- if (options[["predictionsDisplayConfidenceIntervals"]]) options[["predictionsDisplayConfidenceIntervalsLevel"]] else numeric(0)
  return(list(measures = measures, levels = levels, direction = options[["confidenceIntervalBounds"]], times = times,
    plotTimes = if (any(selected)) .sapModelPredictionAt(fit, options, "survival", "x", plot = TRUE) else numeric(0),
    plotPercentiles = if (any(vapply(measures, function(measure) measure[["type"]] == "quantile", logical(1))))
      .sapModelPredictionAt(fit, options, "quantile", "x", plot = TRUE) else numeric(0)))
}

.sapCustomPredictionInput <- function(original, fitting, options, required = .sapCustomPredictionVariables(options)) {

  source    <- options[["predictionsPredictFor"]]
  variables <- .sapCustomPredictionVariables(options)
  data      <- switch(source,
    dataset = original,
    csv     = .sapCustomPredictionCsv(options[["predictionsCsvFile"]], required),
    manual  = .sapCustomPredictionManual(options[["predictionsManualInput"]], variables)
  )

  # Resolve subgroup defaults before choosing a fit; row eligibility still uses
  # the original data, so a default cannot make an empty row eligible.
  if (source %in% c("dataset", "manual")) {
    for (variable in intersect(variables, options[["subgroup"]])) {
      blank <- is.na(data[[variable]]) | !nzchar(trimws(data[[variable]]))
      data[[variable]][blank] <- as.character(fitting[[variable]][1L])
    }
  }
  if (source == "dataset")
    return(data)
  if (nrow(data) == 0L)
    stop(gettext("The prediction input contains no observations."))
  if (.sapHasSubgroups(options))
    .sapCustomPredictionCheckSubgroups(data, fitting, options[["subgroup"]])

  return(data)
}

.sapCustomPredictionCsv <- function(path, required) {

  path <- trimws(path)
  if (!nzchar(path) || !file.exists(path))
    stop(gettext("Select an existing CSV file for custom predictions."))
  data <- try(utils::read.csv(
    file         = path,
    check.names  = FALSE,
    colClasses   = "character",
    na.strings   = "",
    fileEncoding = "UTF-8-BOM",
    strip.white  = TRUE
  ), silent = TRUE)
  if (inherits(data, "try-error"))
    stop(gettext("The prediction CSV file could not be read. Use a comma-separated file with a header row."))
  if (anyDuplicated(names(data)))
    stop(gettext("The prediction CSV file contains duplicate column names."))

  labels  <- jaspBase::decodeColNames(required)
  missing <- setdiff(labels, names(data))
  if (length(missing) > 0L)
    stop(gettextf("The prediction CSV file is missing these columns: %1$s.", paste(missing, collapse = ", ")))
  data <- data[labels]
  names(data) <- required

  return(data)
}

.sapCustomPredictionManual <- function(columns, variables) {

  if (length(columns) == 0L)
    stop(gettext("Add at least one observation to the prediction input table."))
  rows <- length(columns[[1L]][["values"]])
  if (length(variables) == 0L)
    return(data.frame(row.names = seq_len(rows)))

  columnRows <- vapply(columns, function(column) length(column[["values"]]), integer(1))
  if (length(columns) != length(variables) || any(columnRows != rows))
    stop(gettext("The prediction input table does not match the predictors. Reset the table and enter the observations again."))
  data <- as.data.frame(lapply(columns, function(column)
    as.character(unlist(column[["values"]], use.names = FALSE))), stringsAsFactors = FALSE)
  names(data) <- variables

  return(data)
}

.sapCustomPredictionCheckSubgroups <- function(data, fitting, subgroups) {

  for (variable in subgroups) {
    invalid <- is.na(data[[variable]]) | !data[[variable]] %in% levels(fitting[[variable]])
    if (any(invalid))
      stop(gettextf("Prediction row %1$i: '%2$s' must be one of these subgroup levels: %3$s.", which(invalid)[1L],
                    jaspBase::decodeColNames(variable), paste(levels(fitting[[variable]]), collapse = ", ")))
  }
  combinations <- unique(fitting[subgroups])
  supported    <- vapply(seq_len(nrow(data)), function(row)
    any(Reduce(`&`, lapply(subgroups, function(variable)
      as.character(combinations[[variable]]) == as.character(data[[variable]][row])))), logical(1))
  if (any(!supported))
    stop(gettextf("Prediction row %1$i specifies a subgroup combination absent from the fitting data.", which(!supported)[1L]))

  return()
}

.sapCustomPredictionDatasetRows <- function(dataset, options) {

  predictors <- .sapCustomPredictionVariables(options)
  available  <- Reduce(`|`, lapply(predictors, function(variable)
    !is.na(dataset[[variable]]) & nzchar(trimws(as.character(dataset[[variable]])))), init = rep(FALSE, nrow(dataset)))
  if (options[["censoringType"]] == "interval") {
    # A single open bound still records a censored outcome.
    missing <- !is.finite(dataset[[options[["intervalStart"]]]]) & !is.finite(dataset[[options[["intervalEnd"]]]])
  } else {
    timeVariables <- if (options[["censoringType"]] == "counting")
      c(options[["intervalStart"]], options[["intervalEnd"]]) else options[["timeToEvent"]]
    missing <- !stats::complete.cases(dataset[c(timeVariables, options[["eventStatus"]])])
  }

  return(which(available & missing))
}

.sapCustomPredictionRows <- function(input, fit, options, original = input) {

  rows     <- if (options[["predictionsPredictFor"]] == "dataset") .sapCustomPredictionDatasetRows(original, options) else seq_len(nrow(input))
  training <- attr(fit, "dataset")
  if (.sapHasSubgroups(options) && attr(fit, "subgroupLabel") != gettext("Full dataset")) {
    # Match subgroup values directly, rather than comparing concatenated labels.
    for (variable in options[["subgroup"]])
      rows <- rows[!is.na(input[[variable]][rows]) & as.character(input[[variable]][rows]) == as.character(training[[variable]][1L])]
  }

  return(rows)
}

.sapCustomPredictionData <- function(input, rows, fit, options) {

  variables <- .sapModelVariables(attr(fit, "modelTerms"))
  training  <- attr(fit, "dataset")
  source    <- options[["predictionsPredictFor"]]
  data      <- input[rows, variables, drop = FALSE]
  valid     <- rep(TRUE, length(rows))
  referenceNotes <- character(0)
  for (variable in variables) {
    if (is.factor(training[[variable]]) && source %in% c("manual", "dataset") &&
        any(is.na(data[[variable]]) | !nzchar(trimws(as.character(data[[variable]])))))
      referenceNotes <- c(referenceNotes, gettextf("Blank values for %1$s in %2$s use the default profile level %3$s.",
        jaspBase::decodeColNames(variable), attr(fit, "modelTitle"), .sapFittedFactorReference(fit, variable)))
    predictor <- .sapCustomPredictionPredictor(data[[variable]], training[[variable]], source, variable, rows, .sapCaseWeights(training, options),
      if (is.factor(training[[variable]])) .sapFittedFactorReference(fit, variable) else NULL)
    data[[variable]] <- predictor[["value"]]
    valid <- valid & predictor[["valid"]]
  }

  # Check domains while retaining the fitted scaling constants and polynomial
  # basis for the actual prediction; sample-level fitting checks do not apply.
  for (transformations in .sapModelTransformations(attr(fit, "modelTerms"))) for (variable in names(transformations)) {
    value   <- suppressWarnings(as.numeric(as.character(data[[variable]])))
    invalid <- .sapTransformationInvalid(value, transformations[[variable]])
    if (source != "dataset" && any(invalid))
      stop(gettextf("Prediction row %1$i: the value of '%2$s' is outside the domain of its model transformation.",
                    rows[which(invalid)[1L]], jaspBase::decodeColNames(variable)))
    valid <- valid & !invalid
  }

  return(list(data = data[valid, , drop = FALSE], rows = rows[valid], omitted = sum(!valid), referenceNotes = referenceNotes))
}

.sapCustomPredictionPredictor <- function(value, training, source, variable, rows, weights, referenceLevel) {

  value <- as.character(value)
  blank <- is.na(value) | !nzchar(trimws(value))
  if (is.factor(training)) {
    levels <- levels(training)
    if (source %in% c("dataset", "manual"))
      value[blank] <- referenceLevel
    invalid <- is.na(value) | !value %in% levels
    if (source != "dataset" && any(invalid))
      stop(gettextf("Prediction row %1$i: '%2$s' must be one of these levels: %3$s.", rows[which(invalid)[1L]],
                    jaspBase::decodeColNames(variable), paste(levels, collapse = ", ")))
    value <- factor(value, levels = levels, ordered = is.ordered(training))
  } else {
    value   <- .sapCustomPredictionNumeric(value, blank, training, source, weights)
    invalid <- !is.finite(value)
    if (source != "dataset" && any(invalid))
      stop(gettextf("Prediction row %1$i: '%2$s' requires one finite numeric value%3$s.", rows[which(invalid)[1L]],
                    jaspBase::decodeColNames(variable), if (source == "manual") gettext(" or expression") else ""))
  }

  return(list(value = value, valid = !invalid))
}

.sapCustomPredictionNumeric <- function(value, blank, training, source, weights) {

  if (source == "manual") {
    values <- rep(.sapWeightedMean(training, weights), length(value))
    values[!blank] <- vapply(value[!blank], function(expression) {
      result <- .sapNumericExpression(expression)
      if (!.sapFiniteNumeric(result, expectedLength = 1L))
        return(NA_real_)
      return(as.numeric(result))
    }, numeric(1))
  } else {
    values <- suppressWarnings(as.numeric(value))
    if (source == "dataset")
      values[blank] <- .sapWeightedMean(training, weights)
  }

  return(values)
}
