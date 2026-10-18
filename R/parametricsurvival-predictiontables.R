.sapPredictionTableVariables <- function(options) {

  if (options[["predictionsOutputPlotType"]] == "contour")
    variables <- c(.sapModelPredictionAxes(options), options[["predictionsContourSeparateLines"]], options[["predictionsContourSeparatePlots"]])
  else
    variables <- c(.sapModelPredictionAxes(options), options[["predictionsSeparateLines"]], options[["predictionsSeparatePlots"]])
  return(setdiff(unique(unlist(variables, use.names = FALSE)), options[["predictionsLifetimeTimeId"]]))
}

.sapModelPredictionTable <- function(fit, options, type, measure = type) {

  return(.sapPredictionVariablesTable(fit, options, stats::setNames(list(type), measure)))
}

.sapLifeTimeTableFun <- function(fit, options) {

  measures <- as.list(.sapPredictionTypes[!names(.sapPredictionTypes) %in% c("survivalTime", "relativeSurvivalTime")])
  selected <- vapply(.sapModelPredictionTimeOptions, function(name) options[[name]], logical(1))
  return(.sapPredictionVariablesTable(fit, options, measures[selected], merged = TRUE))
}

.sapPredictionVariablesTable <- function(fit, options, measures, merged = FALSE) {

  table <- createJaspTable()
  .sapAddModelColumns(table, options, output = "coefficientsCovarianceMatrix")
  variables <- .sapPredictionTableVariables(options)
  columns   <- stats::setNames(paste0("variable", seq_along(variables), recycle0 = TRUE), variables)
  for (variable in variables)
    table$addColumnInfo(name = columns[[variable]], title = jaspBase::decodeColNames(variable),
      type = if (variable %in% options[["covariates"]]) "number" else "string", overtitle = gettext("Variables"))
  type <- measures[[1L]]
  wide <- options[["predictionsOutputTableFormat"]] == "wide" && !type %in% c("mean", "median")
  if (!wide && !type %in% c("mean", "median"))
    table$addColumnInfo(name = "at",
      title = if (type == "quantile") gettext("Percentile") else gettext("Time"), type = "number")
  interval <- type %in% c("quantile", "mean", "median") && !merged && names(measures)[1L] != "relativeSurvivalTime" &&
    options[["predictionsDisplayPredictionInterval"]] && options[["predictionsOutputPlotType"]] == "standard" &&
    !options[["predictionsLifetimeTimeId"]] %in% .sapModelPredictionAxes(options)
  contour <- options[["predictionsOutputPlotType"]] == "contour"
  axes    <- .sapModelPredictionAxes(options)
  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit) || !length(axes) ||
    (contour && !.sapPredictionContourReady(options))) {
    .sapPredictionVariablesTableColumns(table, options, measures, merged, interval)
    return(table)
  }
  if (contour)
    axes <- axes[1L]
  records <- try(lapply(axes, function(axis)
    .sapPredictionVariablesTableRows(fit, options, measures, merged, interval, columns, axis)), silent = TRUE)
  if (jaspBase::isTryError(records)) {
    .sapPredictionVariablesTableColumns(table, options, measures, merged, interval)
    table$setError(conditionMessage(attr(records, "condition")))
    return(table)
  }

  at <- if (wide) unique(unlist(lapply(records, function(record) record[["data"]][["at"]]), use.names = FALSE))
  .sapPredictionVariablesTableColumns(table, options, measures, merged, interval, at)
  data <- if (wide) lapply(records, .sapPredictionVariablesTableWideData, measures = measures, merged = merged, at = at) else lapply(records, `[[`, "data")
  table$setData(.saSafeRbind(data))
  table$showSpecifiedColumnsOnly <- TRUE
  notes <- unique(unlist(lapply(records, `[[`, "notes"), use.names = FALSE))
  for (note in notes)
    table$addFootnote(note)
  for (note in .sapPredictionConditionNotes(fit, options))
    table$addFootnote(note)
  if (interval)
    table$addFootnote(gettext("Prediction ranges include fitted event-time variability and exclude parameter-estimation uncertainty."))
  return(table)
}

.sapPredictionTableColumns <- function(table, specification, label, prefix) {

  table$addColumnInfo(name = paste0(prefix, "estimate"), title = gettext("Estimate"), overtitle = label, type = "number")
  for (j in seq_along(specification[["levels"]])) {
    level <- 100 * specification[["levels"]][j]
    if (specification[["direction"]] != "upper")
      table$addColumnInfo(
        name      = paste0(prefix, "lower", j),
        title     = gettextf("Lower %1$s%%", level),
        overtitle = label,
        type      = "number"
      )
    if (specification[["direction"]] != "lower")
      table$addColumnInfo(
        name      = paste0(prefix, "upper", j),
        title     = gettextf("Upper %1$s%%", level),
        overtitle = label,
        type      = "number"
      )
  }
}

.sapPredictionTableEvaluationTitle <- function(title, type, at) {

  at <- as.character(at)
  return(if (type == "quantile") gettextf("%1$s (Percentile: %2$s)", title, at) else if (type == "rmst")
    gettextf("%1$s (Horizon: %2$s)", title, at) else gettextf("%1$s (Time: %2$s)", title, at))
}

.sapPredictionVariablesTableColumns <- function(table, options, measures, merged, interval, at = NULL) {

  for (measure in names(measures))
    for (k in seq_len(if (is.null(at)) 1L else length(at))) {
      prefix <- if (merged) paste0(measure, ".") else ""
      if (!is.null(at))
        prefix <- paste0(prefix, "at", k, ".")
      title  <- .sapPredictionTitle(measures[[measure]], options, measure)
      if (!is.null(at)) {
        title         <- .sapPredictionTableEvaluationTitle(title, measures[[measure]], at[k])
        specification <- list(levels = if (options[["predictionsDisplayConfidenceIntervals"]]) options[["predictionsDisplayConfidenceIntervalsLevel"]] else numeric(0),
          direction = options[["confidenceIntervalBounds"]])
        .sapPredictionTableColumns(table, specification, title, prefix)
      } else {
        table$addColumnInfo(name = paste0(prefix, "estimate"), title = title, type = "number")
        if (options[["predictionsDisplayConfidenceIntervals"]])
          .sapAddBoundColumns(table, options[["predictionsDisplayConfidenceIntervalsLevel"]],
            options[["confidenceIntervalBounds"]], paste0(prefix, "lCi"), paste0(prefix, "uCi"), if (merged) paste(title, .sapBoundTitle(options[["predictionsDisplayConfidenceIntervalsLevel"]], options[["confidenceIntervalBounds"]])))
      }
    }
  if (interval)
    .sapAddBoundColumns(table, options[["predictionsDisplayPredictionIntervalLevel"]], "twoSided", "predictionLower", "predictionUpper",
      gettextf("%1$s%% Prediction range", 100 * options[["predictionsDisplayPredictionIntervalLevel"]]))
}

.sapPredictionVariablesTableWideData <- function(record, measures, merged, at) {

  data      <- record[["data"]]
  scenarios <- record[["scenarios"]]
  prefixes  <- if (merged) paste0(names(measures), ".") else ""
  fields    <- unlist(lapply(prefixes, function(prefix) paste0(prefix, c("estimate", "lCi", "uCi"))), use.names = FALSE)
  rows      <- which(!duplicated(scenarios))
  wide      <- data[rows, setdiff(names(data), c("at", fields)), drop = FALSE]
  for (prefix in prefixes)
    for (k in seq_along(at)) {
      selected <- which(data[["at"]] == at[k])
      selected <- selected[match(scenarios[rows], scenarios[selected])]
      wide[[paste0(prefix, "at", k, ".estimate")]] <- data[[paste0(prefix, "estimate")]][selected]
      for (side in c("lower", "upper"))
        wide[[paste0(prefix, "at", k, ".", side, "1")]] <- data[[paste0(prefix, if (side == "lower") "lCi" else "uCi")]][selected]
    }
  return(wide)
}

.sapPredictionVariablesTableRows <- function(fit, options, measures, merged, interval, columns, axis) {

  type      <- measures[[1L]]
  variables <- names(columns)
  contour   <- options[["predictionsOutputPlotType"]] == "contour"
  notes     <- character(0)
  settings  <- if (contour) .sapPredictionContourSettings(options) else .sapPredictionAxisOptions(options, axis)
  scenarios <- if (contour) .sapPredictionContourScenarios(
    fit,
    settings,
    .sapModelPredictionAxes(options),
    options[["predictionsPredictorValuesInTable"]]
  ) else .sapModelPredictionScenarios(fit, settings)
  at     <- .sapModelPredictionAt(fit, settings, type, scenarios[["timeRole"]])
  values <- lapply(
    names(measures),
    function(measure) .sapModelPredictionValues(fit, scenarios, settings, measures[[measure]], at, measure)
  )
  names(values) <- names(measures)
  first <- values[[1L]]
  data  <- data.frame(at = first[["at"]])
  if (type == "quantile")
    data[["at"]] <- 100 * data[["at"]]
  marginal <- .sapPredictionMarginalFactors(fit, settings, .sapPredictionUsedVariables(settings))
  for (variable in variables) {
    value <- scenarios[["data"]][[variable]][first[["Scenario"]]]
    if (variable %in% marginal)
      value <- rep(NA_character_, nrow(first))
    else if (is.factor(value))
      value <- as.character(value)
    data[[columns[[variable]]]] <- value
  }
  for (measure in names(measures)) {
    prefix <- if (merged) paste0(measure, ".") else ""
    for (field in c("estimate", "lCi", "uCi"))
      data[[paste0(prefix, field)]] <- values[[measure]][[field]]
    warnings <- attr(values[[measure]], "predictionWarnings")
    if (measures[[measure]] == "quantile" && any(is.infinite(values[[measure]][["estimate"]])))
      warnings <- c(setdiff(warnings, gettext("Some requested quantities are infinite or exceed numerical range.")),
        gettext("Lifetime percentiles that are infinite or exceed numerical range cannot be drawn as finite plot points."))
    notes <- unique(c(notes, warnings))
  }
  if (interval) {
    pointOptions <- settings
    pointOptions[["predictionsDisplayConfidenceIntervals"]] <- FALSE
    probabilities <- unname(.sapBoundProbabilities(options[["predictionsDisplayPredictionIntervalLevel"]], "twoSided"))
    range         <- .sapModelPredictionValues(fit, scenarios, pointOptions, "quantile", probabilities, "survivalTime")
    notes <- unique(c(notes, attr(range, "predictionWarnings")))
    data[["predictionLower"]] <- range[["estimate"]][range[["at"]] == probabilities[1L]][first[["Scenario"]]]
    data[["predictionUpper"]] <- range[["estimate"]][range[["at"]] == probabilities[2L]][first[["Scenario"]]]
  }

  return(list(data = .sapAddRowModelInformation(data, fit), scenarios = first[["Scenario"]], notes = notes))
}

.sapPredictionConditionNotes <- function(fit, options, used = .sapPredictionTableVariables(options)) {

  if (options[["predictionsUnassignedReferenceValues"]] == "population")
    return(gettext("Predictions average survival over observed fitting-data profiles, holding displayed predictors fixed."))

  training  <- attr(fit, "dataset")
  variables <- setdiff(.sapModelVariables(attr(fit, "modelTerms")), used)
  if (!length(variables))
    return(character(0))

  factors     <- variables[vapply(training[variables], is.factor, logical(1))]
  numeric     <- setdiff(variables, factors)
  mode        <- options[["predictionsUnassignedReferenceValues"]]
  notes       <- character(0)
  namesLabel  <- function(variables) paste(jaspBase::decodeColNames(variables), collapse = ", ")
  valuesLabel <- function(variables, digits = 6L, separator = "; ") paste(vapply(variables, function(variable) {
    value <- .sapPredictionReferenceValue(training[[variable]], variable, options, .sapCaseWeights(training, options),
      if (is.factor(training[[variable]])) .sapFittedFactorReference(fit, variable) else NULL)
    paste0(jaspBase::decodeColNames(variable), " = ", if (is.factor(value)) as.character(value) else format(signif(value, digits), trim = TRUE))
  }, character(1)), collapse = separator)
  if (mode == "average") {
    if (length(factors) && length(numeric))
      return(gettextf(
        "Numerical predictors are at their sample means (%1$s) and categorical predictor contributions (%2$s) are averaged equally across their levels on each parameter's link scale.",
        valuesLabel(numeric, 4L, ", "),
        namesLabel(factors)
      ))
    if (length(factors))
      return(gettextf(
        "Categorical predictor contributions (%1$s) are averaged equally across their levels on each parameter's link scale.",
        namesLabel(factors)
      ))
    return(gettextf("Numerical predictors are at their sample means (%1$s).", valuesLabel(numeric, 4L, ", ")))
  }
  if (mode == "reference") {
    if (length(numeric) && length(factors))
      return(gettextf(
        "Numerical predictors are at zero (%1$s) and categorical predictors use their default levels (%2$s).",
        valuesLabel(numeric, 4L, ", "),
        valuesLabel(factors, separator = ", ")
      ))
    if (length(numeric))
      return(gettextf("Numerical predictors are at zero (%1$s).", valuesLabel(numeric, 4L, ", ")))
    return(gettextf("Categorical predictors use their default levels (%1$s).", valuesLabel(factors, separator = ", ")))
  } else {
    custom       <- .sapPredictionCustomValues(options)
    entered      <- variables[nzchar(custom[variables])]
    blankNumeric <- setdiff(numeric, entered)
    blankFactors <- setdiff(factors, entered)
    if (length(entered))
      notes <- c(notes, gettextf("Custom values: %1$s.", valuesLabel(entered)))
    if (length(blankNumeric))
      notes <- c(notes, gettextf("Numerical predictors are at their sample means (%1$s).", valuesLabel(blankNumeric, 4L, ", ")))
    if (length(blankFactors))
      notes <- c(notes, gettextf(
        "Categorical predictor contributions (%1$s) are averaged equally across their levels on each parameter's link scale.",
        namesLabel(blankFactors)
      ))
  }
  return(notes)
}
