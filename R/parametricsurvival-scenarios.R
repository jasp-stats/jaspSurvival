# Model displays use raw fitting-data scenarios, then native fitted bases.
.sapSelectedLifetimeSummaries <- function(options) {

  selected <- vapply(.sapLifetimeSummaryOptions, function(name) options[[name]], logical(1))
  return(c("quantile", "median", "mean")[selected])
}

.sapModelPredictionRoleDependencies <- c("predictionsXAxisVariables", "predictionsUnassignedReferenceValues", "predictionsUnassignedReferenceValuesCustomValues", "predictionsLifetimeTimeId", "predictionsSeparateLines", "predictionsSeparatePlots", "predictionsAssignedCovariateValues", "predictionsAssignedCovariateValuesQuantiles", "predictionsAssignedCovariateValuesCustomValues", "predictionsAssignedCovariateValuesGroupUniqueLevels", "predictionsAssignedCovariateValuesGroupUniqueLevelsLimit",
  "predictionsContourXAxis", "predictionsContourYAxis", "predictionsContourSeparateLines", "predictionsContourSeparatePlots")
.sapModelPredictionEvaluationDependencies <- c("predictionsTimePercentages", "predictionsPredictorValuesInTable")

.sapModelPredictionTimeRole <- function(options) {

  time <- options[["predictionsLifetimeTimeId"]]
  if (options[["predictionsXAxisVariables"]] == time)
    return("x")
  if (time %in% options[["predictionsSeparateLines"]])
    return("lines")
  if (time %in% options[["predictionsSeparatePlots"]])
    return("panels")
}

.sapModelPredictionAt <- function(fit, options, type, timeRole, plot = FALSE) {

  if (type %in% c("mean", "median"))
    return(0)
  if (type == "quantile") {
    values <- .sapOptions2PredictionQuantile(options)
    if (plot && timeRole == "x") {
      plotRange <- if (options[["predictionsLifetimePercentilesGrid"]] == "sequence")
        .sapPredictionSequenceSettings(options, "predictionsLifetimePercentiles", 100, probability = TRUE)[c("From", "To")] / 100 else range(values)
      if (diff(plotRange) > 0)
        values <- sort(unique(c(values, seq(plotRange[1L], plotRange[2L], length.out = 101L))))
    }
    return(values)
  }
  if (plot)
    options[["predictionsRoundTimeTableValues"]] <- FALSE
  smooth <- plot && timeRole == "x"
  return(.sapOptions2PredictionTime(options, fit, if (smooth) type else "time", smooth))
}

.sapValidateModelPredictionEvaluations <- function(options) {

  axes <- .sapModelPredictionAxes(options)
  if (!.sapModelPredictionActive(options) || identical(axes, options[["predictionsLifetimeTimeId"]]))
    return()
  if (options[["predictionsLifetimePercentiles"]] &&
    (options[["predictionsOutcomeSurvivalTime"]] ||
      ((options[["predictionsOutcomeTimeRatio"]] || options[["predictionsOutcomeRelativeSurvivalTime"]]) && .sapRelativeLifeAvailable(options))))
    .sapOptions2PredictionQuantile(options)
  if (any(vapply(.sapModelPredictionTimeOptions, function(name) options[[name]], logical(1)))) {
    if (options[["predictionsTimeGrid"]] == "customRelative")
      .sapPredictionRelativeTimes(options)
    if (options[["predictionsTimeGrid"]] == "customAbsolute" && nzchar(trimws(options[["predictionsTimeValues"]])))
      .sapPredictionAbsoluteTimes(options)
  }
  return()
}

.sapPredictionGroups <- function(value, groups, variable, weights) {

  if (is.factor(value))
    return(list(values = factor(levels(value), levels = levels(value), ordered = is.ordered(value)), labels = levels(value),
      rows = lapply(levels(value), function(level) which(value == level)), collapsed = FALSE))
  cuts <- if (groups == 1L) numeric(0) else
    unique(.sapFrequencyQuantile(value, weights, seq_len(groups - 1L) / groups))
  bins            <- cut(value, breaks = c(-Inf, cuts, Inf), right = TRUE, labels = FALSE)
  groupRows       <- split(seq_along(value), bins)
  representatives <- unname(vapply(groupRows, function(rows) .sapWeightedMean(value[rows], weights[rows]), numeric(1)))
  count           <- length(representatives)
  rows            <- lapply(sort(unique(bins)), function(bin) which(bins == bin))
  return(list(
    values    = representatives,
    labels    = .sapPredictionCovariateLabels(representatives, variable),
    rows      = rows,
    collapsed = count < groups
  ))
}

.sapPredictionCovariateLabels <- function(values, variable) {

  labels <- format(signif(values, 4L), trim = TRUE)
  if (anyDuplicated(labels))
    labels <- format(values, digits = 17L, trim = TRUE)
  return(vapply(labels, function(value) gettextf("%1$s = %2$s", jaspBase::decodeColNames(variable), value), character(1)))
}

.sapPredictionAssignedCovariates <- function(options) {

  roles <- if (options[["predictionsOutputPlotType"]] == "contour")
    c(options[["predictionsContourSeparateLines"]], options[["predictionsContourSeparatePlots"]]) else
    c(options[["predictionsSeparateLines"]], options[["predictionsSeparatePlots"]])
  return(intersect(unlist(options[["covariates"]], use.names = FALSE), unlist(roles, use.names = FALSE)))
}

.sapPredictionAssignedValues <- function(options) {

  variables <- .sapPredictionAssignedCovariates(options)
  if (!length(variables))
    return(list())

  data <- .sapCustomPredictionManual(options[["predictionsAssignedCovariateValuesCustomValues"]], variables)
  return(lapply(stats::setNames(variables, variables), function(variable) {
    text   <- trimws(data[[variable]])
    text   <- text[nzchar(text)]
    values <- vapply(text, function(value) {
      number <- .sapNumericExpression(value)
      if (!.sapFiniteNumeric(number, expectedLength = 1L))
        stop(gettextf(
          "Custom assigned values for %1$s must be finite numbers or numeric expressions.",
          jaspBase::decodeColNames(variable)
        ))
      as.numeric(number)
    }, numeric(1))
    return(unique(unname(values)))
  }))
}

.sapPredictionCustomCovariateGroup <- function(value, values, variable, weights) {

  averaged <- !length(values)
  if (averaged)
    values <- .sapWeightedMean(value, weights)
  rows <- if (averaged) list(seq_along(value)) else lapply(values, function(number) which(value == number))
  return(list(values = values, labels = .sapPredictionCovariateLabels(values, variable), rows = rows, collapsed = FALSE))
}

.sapPredictionUniqueCovariateGroup <- function(value, variable) {

  levels <- sort(unique(value))
  return(list(values = levels, labels = .sapPredictionCovariateLabels(levels, variable),
    rows = lapply(levels, function(level) which(value == level)), collapsed = FALSE))
}

# Build reference profiles and the grid assigned to axes, lines and panels.
.sapModelPredictionScenarios <- function(fit, options, plot = FALSE, timeRole = .sapModelPredictionTimeRole(options)) {

  training  <- attr(fit, "dataset")
  weights   <- .sapCaseWeights(training, options)
  lines     <- unlist(options[["predictionsSeparateLines"]], use.names = FALSE)
  panels    <- unlist(options[["predictionsSeparatePlots"]], use.names = FALSE)
  lines     <- setdiff(lines, options[["predictionsLifetimeTimeId"]])
  panels    <- setdiff(panels, options[["predictionsLifetimeTimeId"]])
  variables <- unique(c(.sapOptionsVariables(options), lines, panels))
  xVariable <- options[["predictionsXAxisVariables"]]
  timeAxis  <- timeRole == "x"
  if (!timeAxis)
    variables <- unique(c(variables, xVariable))

  # Start with reference values for predictors that have no display role.
  reference <- training[1L, variables, drop = FALSE]
  for (variable in variables)
    reference[[variable]] <- .sapPredictionReferenceValue(training[[variable]], variable, options, weights,
      if (is.factor(training[[variable]])) .sapFittedFactorReference(fit, variable) else NULL)

  # Choose representative values for line and panel groups.
  roles    <- unique(c(lines, panels))
  custom   <- options[["predictionsAssignedCovariateValues"]] == "custom"
  count    <- if (custom) 1L else options[["predictionsAssignedCovariateValuesQuantiles"]]
  assigned <- if (custom) .sapPredictionAssignedValues(options) else list()
  groups   <- lapply(roles, function(variable) {
    if (custom && !is.factor(training[[variable]]))
      .sapPredictionCustomCovariateGroup(training[[variable]], assigned[[variable]], variable, weights)
    else if (!is.factor(training[[variable]]) && options[["predictionsAssignedCovariateValuesGroupUniqueLevels"]] &&
      length(unique(training[[variable]])) <= options[["predictionsAssignedCovariateValuesGroupUniqueLevelsLimit"]])
      .sapPredictionUniqueCovariateGroup(training[[variable]], variable)
    else
      .sapPredictionGroups(training[[variable]], count, variable, weights)
  })
  names(groups) <- roles
  if (!timeAxis) {
    value <- training[[xVariable]]
    axis  <- if (is.factor(value)) factor(levels(value), levels = levels(value), ordered = is.ordered(value)) else
      if (plot) .sapPredictionAxisPlotGrid(fit, options, xVariable, 101L) else
        seq(min(value), max(value), length.out = options[["predictionsPredictorValuesInTable"]])
    groups[[xVariable]] <- list(values = axis, labels = as.character(axis), collapsed = FALSE)
  }

  # Cross the assigned values while retaining the observation groups for overlays.
  indices <- if (length(groups)) expand.grid(lapply(groups, function(group) seq_along(group[["values"]])), KEEP.OUT.ATTRS = FALSE) else data.frame(.scenario = 1L)
  data    <- reference[rep(1L, nrow(indices)), , drop = FALSE]
  for (variable in names(groups))
    data[[variable]] <- groups[[variable]][["values"]][indices[[variable]]]
  label <- function(role) {
    if (!length(role))
      return(rep("", nrow(indices)))
    values <- lapply(role, function(variable) {
      labels <- groups[[variable]][["labels"]][indices[[variable]]]
      if (is.factor(training[[variable]]))
        paste0(jaspBase::decodeColNames(variable), " = ", labels) else
        labels
    })
    do.call(paste, c(values, sep = " | "))
  }
  notes <- vapply(roles[vapply(groups[roles], `[[`, logical(1), "collapsed")], function(variable)
    gettextf(
      "%1$s has %2$i nonempty groups instead of the requested %3$i; tied values remain together.",
      jaspBase::decodeColNames(variable),
      length(groups[[variable]][["values"]]),
      count
    ), character(1))
  roleKeys       <- if (length(roles)) do.call(paste, c(indices[roles], sep = "|")) else rep("", nrow(indices))
  uniqueRoles    <- which(!duplicated(roleKeys))
  observedGroups <- lapply(uniqueRoles, function(i) {
    rows <- seq_len(nrow(training))
    for (variable in roles)
      rows <- intersect(rows, groups[[variable]][["rows"]][[indices[[variable]][i]]])
    rows
  })
  observedRows <- observedGroups[match(roleKeys, roleKeys[uniqueRoles])]
  return(list(data = data, x = if (!timeAxis) data[[xVariable]], timeAxis = timeAxis, timeRole = timeRole, xVariable = xVariable,
    lines = label(lines), panels = label(panels), observedRows = observedRows, notes = notes))
}

.sapModelPredictionPercentileLabels <- function(at) {

  at     <- 100 * at
  labels <- as.character(at)
  if (anyDuplicated(labels[!duplicated(at)]))
    labels <- format(at, digits = 17L, trim = TRUE)
  return(labels)
}

.sapModelPredictionEvaluationLabels <- function(at, type) {

  if (type == "quantile")
    at <- 100 * at
  labels <- as.character(at)
  if (anyDuplicated(labels[!duplicated(at)]))
    labels <- format(at, digits = 17L, trim = TRUE)
  return(paste0(if (type == "quantile") gettext("Lifetime percentile") else gettext("Time"), " = ", labels))
}

# Evaluate scenarios, then attach the grouping columns used by tables and plots.
.sapModelPredictionValues <- function(fit, scenarios, options, type, at, measure = type, fast = FALSE) {

  fit       <- .sapPredictionConditionedFit(fit, scenarios[["data"]], options, .sapPredictionUsedVariables(options))
  arguments <- .sapPredictionArguments(fit, options, type, at, measure)
  if (.sapRelativeMeasure(measure)) {
    result <- .sapPredictionRatioSummary(fit, scenarios, options, at, type, measure)
  } else if (options[["predictionsUnassignedReferenceValues"]] == "population") {
    result <- .sapPopulationSummary(fit, options, if (measure == "failureProbability") "failure" else type, at, fast)
  } else if (fast) {
    result <- .sapPredictionPointSummary(fit, scenarios[["data"]], type, at, fn = arguments[["fn"]])
  } else {
    result <- do.call(.sapSummaryPredictions, arguments)
  }

  # Normalize the summary layout before adding scenario labels and plot coordinates.
  warnings <- attr(result, "predictionWarnings")
  if (fit[["ncovs"]] == 0L && length(result) == 1L)
    result <- rep(result, nrow(scenarios[["data"]]))
  stopifnot(length(result) == nrow(scenarios[["data"]]))
  rows <- lapply(seq_along(result), function(i) {
    prediction <- result[[i]]
    value      <- data.frame(at = if (type %in% c("mean", "median")) 0 else prediction[[1L]],
      estimate = prediction[["est"]], lCi = prediction[["lcl"]], uCi = prediction[["ucl"]])
    value[["x"]] <- if (scenarios[["timeAxis"]]) if (type %in% c("mean", "median"))
      factor(rep(" ", nrow(value))) else value[["at"]] else rep(scenarios[["x"]][i], nrow(value))
    if (!is.null(scenarios[["y"]]))
      value[["y"]] <- rep(scenarios[["y"]][i], nrow(value))
    value[["Line"]] <- scenarios[["lines"]][i]
    value[["ConditionLine"]] <- value[["Line"]]
    value[["Panel"]] <- scenarios[["panels"]][i]
    if (!scenarios[["timeAxis"]] && !type %in% c("mean", "median")) {
      field  <- if (scenarios[["timeRole"]] == "lines") "Line" else "Panel"
      labels <- .sapModelPredictionEvaluationLabels(value[["at"]], type)
      value[[field]] <- vapply(labels, function(label) .sapPredictionSeriesLabel(value[[field]][1L], label), character(1))
    }
    value[["Scenario"]] <- i
    value[["Distribution"]] <- .sapSeriesLabel(fit, options)
    value[["Subgroup"]] <- attr(fit, "subgroupLabel")
    value
  })
  out <- do.call(rbind, rows)
  attr(out, "predictionWarnings") <- unique(c(scenarios[["notes"]], warnings))
  return(out)
}

.sapPredictionSeriesLabel <- function(...) {

  parts <- c(...)
  return(paste(parts[!is.na(parts) & nzchar(parts)], collapse = " | "))
}

# Match observed rows to the same line and panel groups as fitted predictions.
.sapModelPredictionObserved <- function(fit, options, kaplanMeier = FALSE, measure = "survivalProbability") {

  effectiveVariables <- unique(unlist(lapply(fit, function(model) .sapModelVariables(attr(model, "modelTerms"))), use.names = FALSE))
  records            <- lapply(.sapPlotSubgroupFits(fit), function(model) {
    scenarios <- .sapModelPredictionScenarios(model, options)
    dataset   <- .sapPlotObservedDataset(model)
    roles     <- data.frame(Line = scenarios[["lines"]], Panel = scenarios[["panels"]])
    records   <- lapply(which(!duplicated(roles)), function(i) {
      rows <- scenarios[["observedRows"]][[i]]
      if (kaplanMeier)
        for (variable in effectiveVariables)
          rows <- rows[as.character(dataset[[variable]][rows]) == as.character(scenarios[["data"]][[variable]][i])]
      if (!length(rows))
        return(NULL)
      observed <- dataset[rows, , drop = FALSE]
      value    <- if (kaplanMeier) .sapKaplanMeierStepData(observed, options, measure == "failureProbability") else
        data.frame(x = observed[[options[["timeToEvent"]]]], Event = observed[[options[["eventStatus"]]]])
      if (!kaplanMeier && !scenarios[["timeAxis"]]) {
        value[["estimate"]] <- value[["x"]]
        value[["x"]] <- observed[[scenarios[["xVariable"]]]]
        value[["Status"]] <- factor(
          ifelse(value[["Event"]], gettext("Event"), gettext("Censored")),
          levels = c(gettext("Event"), gettext("Censored"))
        )
      }
      value[["Panel"]] <- scenarios[["panels"]][i]
      value[["Line"]] <- scenarios[["lines"]][i]
      value[["Subgroup"]] <- attr(model, "subgroupLabel")
      value[["Series"]] <- .sapPredictionSeriesLabel(if (kaplanMeier) gettext("Kaplan-Meier") else gettext("Observed"),
        if (.sapHasSubgroups(options)) attr(model, "subgroupLabel"), scenarios[["lines"]][i])
      value
    })
    .saSafeRbind(records)
  })
  return(.saSafeRbind(records))
}

.sapModelPredictionKaplanMeierEligible <- function(fit, options) {

  return(all(vapply(fit, function(model) {
    variables <- .sapModelVariables(attr(model, "modelTerms"))
    all(vapply(attr(model, "dataset")[variables], is.factor, logical(1))) &&
      !length(.sapPredictionMarginalFactors(model, options, .sapPredictionUsedVariables(options)))
  }, logical(1))))
}

.sapKaplanMeierAvailability <- function(jaspResults, options) {

  if (!is.null(jaspResults[["kaplanMeierAvailability"]]))
    return()
  available <- FALSE
  if (options[["predictionsPredictFor"]] == "variables" && .saSurvivalReady(options) && options[["predictionsOutputPlot"]] &&
    options[["predictionsOutputPlotType"]] == "standard" && options[["censoringType"]] == "right" &&
    identical(.sapModelPredictionAxes(options), options[["predictionsLifetimeTimeId"]]) &&
    (options[["predictionsOutcomeSurvivalProbability"]] || options[["predictionsOutcomeFailureProbability"]])) {
    groups <- if (.sapMergePlots(options, "modelPrediction"))
      .sapExtractFit(jaspResults, options, type = "byModel", output = "modelPrediction") else
      .sapNestFit(.sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options))
    available <- any(vapply(groups, function(models) .sapKaplanMeierGroupAvailable(models, options), logical(1)))
  }
  dependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
    .sapModelPredictionRoleDependencies, .sapModelPredictionDisplayDependencies,
    "predictionsPredictFor", "predictionsOutputPlot", "predictionsOutcomeSurvivalProbability", "predictionsOutcomeFailureProbability",
    "predictionsXAxisTransformation", "predictionsMergeSeriesAcrossDistributions", "predictionsMergeSeriesAcrossSubgroups",
    if (options[["analysisType"]] == "mixture") "predictionsMergeSeriesAcrossComponents",
    paste0("predictionsTime", c("Grid", "Intervals", "From", "Step", "To", "Values", "Percentages")),
    "predictionsDisplayConfidenceIntervalsLevel")
  jaspResults[["kaplanMeierAvailability"]] <- jaspBase::createJaspQmlSource(
    sourceID = "kaplanMeierAvailability", value = if (available) '["available"]' else "[]", dependencies = dependencies)
}

.sapKaplanMeierGroupAvailable <- function(models, options) {

  models <- models[!vapply(models, jaspBase::isTryError, logical(1))]
  if (!length(models) || !.sapModelPredictionKaplanMeierEligible(models, options))
    return(FALSE)

  observed <- try(.sapModelPredictionObserved(models, options, kaplanMeier = TRUE), silent = TRUE)
  if (inherits(observed, "try-error") || is.null(observed))
    return(FALSE)

  times <- try(unlist(lapply(models, function(model)
    .sapModelPredictionAt(model, options, "survival", "x", plot = TRUE)), use.names = FALSE), silent = TRUE)
  if (inherits(times, "try-error") || !length(times))
    return(FALSE)

  clipped <- .sapPredictionClipKaplanMeier(observed, range(times))
  return(!is.null(clipped))
}

.sapPredictionClipKaplanMeier <- function(data, bounds) {

  groups <- split(seq_len(nrow(data)), interaction(data[["Series"]], data[["Panel"]], drop = TRUE))
  parts  <- lapply(groups, function(rows) {
    curve <- data[rows, , drop = FALSE]
    lower <- max(bounds[1L], min(curve[["at"]]))
    upper <- min(bounds[2L], max(curve[["at"]]))
    if (upper <= lower)
      return(NULL)
    first <- curve[tail(which(curve[["at"]] <= lower), 1L), , drop = FALSE]
    last  <- curve[tail(which(curve[["at"]] <= upper), 1L), , drop = FALSE]
    first[["at"]] <- lower
    last[["at"]] <- upper
    rbind(first, curve[curve[["at"]] >= lower & curve[["at"]] <= upper, , drop = FALSE], last)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts))
    return(NULL)
  return(.saSafeRbind(parts))
}

# Collect model predictions and series labels before constructing plot layers.
.sapModelPredictionPlotRecords <- function(fit, options, type, measure, timeRole) {

  evaluationFit <- if (timeRole != "x") .sapPlotTimeFit(fit)
  records       <- lapply(fit, function(model) {
    scenarios <- .sapModelPredictionScenarios(model, options, plot = TRUE, timeRole = timeRole)
    at        <- .sapModelPredictionAt(
      if (timeRole == "x") model else evaluationFit,
      options,
      type,
      scenarios[["timeRole"]],
      plot = TRUE
    )
    if (scenarios[["timeAxis"]] && !type %in% c("quantile", "mean", "median")) {
      # Refine point estimates on a dense time grid; plotting does not require
      # lifetime percentiles or confidence bounds at scout points.
      scoutOptions <- options
      scoutOptions[["predictionsDisplayConfidenceIntervals"]] <- FALSE
      evaluate   <- function(times) matrix(
        .sapModelPredictionValues(model, scenarios, scoutOptions, type, times, measure)[["estimate"]],
        nrow = length(times)
      )
      logTime    <- options[["predictionsXAxisTransformation"]] == "log"
      yTransform <- if (type == "survival") switch(options[["predictionsProbabilityYAxisScale"]],
        none = identity, log = log, logmlogmp = function(p) log(-log1p(-p))) else identity
      at      <- .sapAdaptivePlotTimes(at, evaluate, xTransform = if (logTime) log else identity,
        xInverse = if (logTime) exp else identity, yTransform = yTransform, minimum = if (options[["predictionsDisplayConfidenceIntervals"]]) 65L else 17L,
        maximum  = if (options[["predictionsDisplayConfidenceIntervals"]]) 129L else 201L,
        scoutCount = 513L)
    }
    .sapModelPredictionValues(model, scenarios, options, type, at, measure)
  })
  return(records)
}

.sapModelPredictionPlotForAxis <- function(fit, options, type, measure = type) {

  fit <- fit[!vapply(fit, jaspBase::isTryError, logical(1))]
  if (!.saSurvivalReady(options) || !nzchar(options[["predictionsXAxisVariables"]]) || !length(fit))
    return(createJaspPlot())

  timeRole <- .sapModelPredictionTimeRole(options)
  records  <- try(.sapModelPredictionPlotRecords(fit, options, type, measure, timeRole), silent = TRUE)
  if (jaspBase::isTryError(records)) {
    plot <- createJaspPlot()
    plot$setError(conditionMessage(attr(records, "condition")))
    return(plot)
  }
  notes <- unique(unlist(lapply(records, attr, "predictionWarnings"), use.names = FALSE))
  data  <- do.call(rbind, records)
  if (type %in% c("mean", "median") && any(!is.finite(data[["estimate"]]))) {
    plot <- createJaspPlot()
    plot$setError(gettext("This lifetime summary is not finite or could not be evaluated for the selected model and predictor values."))
    return(plot)
  }
  if (!any(is.finite(data[["estimate"]]))) {
    plot <- createJaspPlot()
    plot$setError(gettext("No finite predictions are available for this plot."))
    return(plot)
  }
  for (field in c("estimate", "lCi", "uCi"))
    data[[field]][!is.finite(data[[field]])] <- NA_real_
  timeAxis    <- options[["predictionsXAxisVariables"]] == options[["predictionsLifetimeTimeId"]]
  categorical <- is.factor(data[["x"]])
  xValues     <- data[["x"]]
  fitLabels   <- .sapPredictionFitLabels(fit, options)
  modelIndex  <- rep(seq_along(records), vapply(records, nrow, integer(1)))
  data[["Series"]] <- vapply(seq_len(nrow(data)), function(i) .sapPredictionSeriesLabel(
    fitLabels[modelIndex[i]], data[["Line"]][i]), character(1))
  data[["Series"]][!nzchar(data[["Series"]])] <- gettext("Prediction")
  if (timeRole %in% c("lines", "panels")) {
    rows <- order(modelIndex, data[["Scenario"]], data[["at"]])
    data[["Series"]] <- factor(data[["Series"]], levels = unique(data[["Series"]][rows]))
    data[["Panel"]] <- factor(data[["Panel"]], levels = unique(data[["Panel"]][rows]))
  }
  encoding <- .sapPredictionPlotEncoding(fit, data, options, modelIndex,
    percentileLines = type == "quantile" && timeRole == "lines")
  data  <- encoding[["data"]]
  typed <- !is.null(encoding[["field"]])
  plot  <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = estimate, color = Colour, group = Series))
  if (type %in% c("quantile", "mean", "median") && !timeAxis && options[["predictionsDisplayPredictionInterval"]] && !.sapRelativeMeasure(measure)) {
    prediction <- .sapModelPredictionRange(fit, options, type)
    if (!is.null(prediction)) {
      if (any(!is.finite(prediction[["lower"]]) | !is.finite(prediction[["upper"]]))) {
        notes <- c(notes, gettext("Some prediction-range endpoints are unbounded and are not drawn."))
        prediction[["lower"]][!is.finite(prediction[["lower"]])] <- NA_real_
        prediction[["upper"]][!is.finite(prediction[["upper"]])] <- NA_real_
      }
      prediction[["Panel"]] <- factor(prediction[["Panel"]], levels = levels(data[["Panel"]]))
      if (categorical) {
        prediction[["Series"]] <- factor(prediction[["Series"]], levels = levels(data[["Series"]]))
        plot <- plot + ggplot2::geom_linerange(data = prediction, ggplot2::aes(x = x, ymin = lower, ymax = upper, group = Series),
          inherit.aes = FALSE, colour = "grey70", linewidth = 3, alpha = .35, position = ggplot2::position_dodge(width = .3))
      }
      else
        plot <- plot + ggplot2::geom_ribbon(
          data = prediction,
          ggplot2::aes(x = x, ymin = lower, ymax = upper, group = Group),
          inherit.aes = FALSE,
          fill        = "grey75",
          alpha       = .25,
          colour      = NA
        )
      notes <- c(notes, gettextf(
        "%1$s%% prediction range: fitted event-time variability; parameter uncertainty excluded.",
        100 * options[["predictionsDisplayPredictionIntervalLevel"]]
      ))
    }
  }
  showObserved <- type %in% c("quantile", "mean", "median") && !timeAxis && !.sapRelativeMeasure(measure) && options[["predictionsDisplayObservedData"]]
  if (showObserved) {
    plot <- .sapAddObservedLifetimeLayer(plot, fit, options, data, type, timeRole, categorical)
  }
  if (categorical) {
    dodge <- ggplot2::position_dodge(width = .3)
    if (options[["predictionsDisplayConfidenceIntervals"]]) {
      data <- .sapPlotBoundData(data, options[["confidenceIntervalBounds"]])
      plot <- plot + ggplot2::geom_errorbar(data = data, ggplot2::aes(ymin = boundLower, ymax = boundUpper), width = .15, position = dodge)
    }
    if (!timeAxis && options[["predictionsDisplayConnectXAxisLevels"]])
      plot <- plot + jaspGraphs::geom_line(mapping = if (typed) ggplot2::aes(linetype = Type),
        position = dodge, na.rm = TRUE, show.legend = FALSE)
    plot <- plot + ggplot2::geom_point(mapping = if (typed) ggplot2::aes(shape = Type), position = dodge)
  } else {
    if (options[["predictionsDisplayConfidenceIntervals"]])
      plot <- .sapAddModelBoundLayer(
        plot,
        data,
        options[["predictionsDisplayConfidenceIntervalsLevel"]],
        options[["confidenceIntervalBounds"]],
        x      = "x",
        groups = "Series",
        color  = "Colour",
        fill   = "Colour",
        na.rm  = TRUE
      )
    plot <- plot + jaspGraphs::geom_line(mapping = if (typed) ggplot2::aes(linetype = Type), na.rm = TRUE)
  }
  if (typed)
    plot <- plot + .sapPredictionTypeScale(encoding, points = categorical)
  if (length(unique(data[["Panel"]])) > 1L || any(nzchar(as.character(data[["Panel"]]))))
    plot <- plot + ggplot2::facet_wrap(~Panel, ncol = 2,
      labeller = if (type == "quantile" && timeRole == "panels") ggplot2::label_value else ggplot2::label_wrap_gen(20))
  kaplanMeierColours <- character(0)
  if (timeAxis && type == "survival" && options[["censoringType"]] == "right") {
    if (options[["predictionsDisplayKaplanMeier"]]) {
      eligible <- .sapModelPredictionKaplanMeierEligible(fit, options)
      km       <- if (eligible) .sapModelPredictionObserved(fit, options, kaplanMeier = TRUE, measure = measure)
      if (!is.null(km))
        km <- .sapPredictionClipKaplanMeier(km, range(xValues))
      if (!eligible)
        notes <- c(notes, gettext("Kaplan-Meier overlays require fixed factor profiles; continuous predictors or averaged factor levels have no matching empirical curve."))
      if (!is.null(km)) {
        km[["Colour"]] <- vapply(km[["Line"]], function(line) .sapPredictionSeriesLabel(gettext("Kaplan-Meier"), line), character(1))
        kaplanMeierColours <- unique(km[["Colour"]])
        if (identical(encoding[["field"]], "subgroup"))
          km[["Type"]] <- factor(km[["Subgroup"]], levels = levels(data[["Type"]]))
        # Match the step curves and confidence bands in .saSurvivalPlot().
        if (options[["predictionsDisplayConfidenceIntervals"]])
          plot <- plot + ggplot2::geom_ribbon(
            data = km,
            ggplot2::aes(x = at, ymin = lCi, ymax = uCi, fill = Colour, group = Series),
            inherit.aes = FALSE,
            alpha       = .2,
            colour      = NA
          )
        if (identical(encoding[["field"]], "subgroup"))
          plot <- plot + ggplot2::geom_step(
            data = km,
            ggplot2::aes(x = at, y = estimate, color = Colour, group = Series, linetype = Type),
            inherit.aes = FALSE,
            linewidth   = 1
          )
        else
          plot <- plot + ggplot2::geom_step(
            data = km,
            ggplot2::aes(x = at, y = estimate, color = Colour, group = Series),
            inherit.aes = FALSE,
            linewidth   = 1
          )
      }
    }
  }
  if (!.sapRelativeMeasure(measure) && options[["censoringType"]] == "right" &&
    options[["predictionsDisplayCensoringEvents"]] &&
    ((timeAxis && !type %in% c("mean", "median")) || (!timeAxis && type %in% c("quantile", "mean", "median")))) {
    observed <- .sapPredictionRugData(fit, options, data, type, timeRole)
    if (!is.null(observed)) {
      observed <- .sapPredictionLayerEncoding(observed, data)
      if (timeAxis && type != "quantile") {
        observed <- observed[observed[["x"]] >= min(xValues) & observed[["x"]] <= max(xValues), , drop = FALSE]
        plot     <- plot + ggplot2::geom_rug(data = observed, ggplot2::aes(x = x, colour = Colour),
          inherit.aes = FALSE, sides = "b", alpha = .4, show.legend = FALSE)
      } else {
        if (!timeAxis && !categorical)
          observed <- observed[observed[["x"]] >= min(xValues) & observed[["x"]] <= max(xValues), , drop = FALSE]
        plot <- plot + ggplot2::geom_rug(data = observed, ggplot2::aes(y = if (timeAxis) x else estimate, colour = Colour),
          inherit.aes = FALSE, sides = "l", alpha = .4, show.legend = FALSE)
      }
    }
  }
  colours <- levels(data[["Colour"]])
  for (layer in plot$layers)
    if (is.data.frame(layer$data))
      for (field in c("Colour", "ObservedSeries"))
        if (field %in% names(layer$data))
          colours <- unique(c(colours, as.character(layer$data[[field]])))
  colours     <- unique(c(kaplanMeierColours, colours))
  # Unmatched observed points use fixed gray and have no colour legend key.
  colours     <- colours[!is.na(colours)]
  colorScale  <- jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]])
  fillScale   <- jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])
  singleTrace <- length(setdiff(colours, kaplanMeierColours)) == 1L && length(kaplanMeierColours) <= 1L
  if (length(kaplanMeierColours)) {
    colorScale$palette <- .sapKaplanMeierPalette(colorScale$palette, colours, kaplanMeierColours, "black")
    fillScale$palette <- .sapKaplanMeierPalette(fillScale$palette, colours, kaplanMeierColours, "#333333FF")
  }
  if (singleTrace) {
    # Censoring marks keep their original colours; only curves and bands turn gray.
    for (layer in plot$layers)
      if (inherits(layer$geom, "GeomRug"))
        layer$aes_params$colour <- unname(colorScale$palette(length(colours))[
          match(unique(layer$data[["Colour"]]), colours)])
    colorScale$palette <- .sapPlotPalette(colorScale$palette, 1L)
    fillScale$palette  <- .sapPlotPalette(fillScale$palette, 1L)
  }
  colorScale$limits <- fillScale$limits <- colours
  plot <- plot + ggplot2::xlab(if (timeAxis) if (type %in% c("mean", "median")) "" else
    if (type == "quantile") gettext("Lifetime percentile") else gettext("Time") else jaspBase::decodeColNames(options[["predictionsXAxisVariables"]])) +
    ggplot2::ylab(.sapPredictionTitle(type, options, measure)) + colorScale + fillScale
  if (!categorical) {
    axis <- try(
      .sapPredictionAxisScale(
        fit,
        options[["predictionsXAxisVariables"]],
        .sapPredictionAxisSettings(options, options[["predictionsXAxisVariables"]])[["scale"]],
        timeAxis,
        options
      ),
      silent = TRUE
    )
    if (jaspBase::isTryError(axis)) {
      result <- createJaspPlot()
      result$setError(conditionMessage(attr(axis, "condition")))
      return(result)
    }
    if (is.null(axis)) {
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(xValues[is.finite(xValues)])
      if (length(unique(data[["Panel"]])) > 1L && length(xBreaks) > 4L)
        xBreaks <- xBreaks[unique(c(seq.int(1L, length(xBreaks), by = 2L), length(xBreaks)))]
      plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep)
    } else
      plot <- plot + .sapPredictionPositionScale(axis, "x")
  }
  if (timeAxis && type == "quantile") {
    percentileScale <- plot$scales$get_scales("x")
    percentileScale$labels <- scales::label_number(scale = 100)
  }
  plot <- plot + .sapPredictionOutcomeScale(options, type)
  plot <- .sapPredictionPlotAddTheme(plot, options)
  if (.sapRelativeMeasure(measure))
    plot <- plot + ggplot2::geom_hline(yintercept = 1, linetype = 3, colour = "grey50")
  plot <- plot + ggplot2::labs(colour = encoding[["colourTitle"]], fill = NULL) + ggplot2::guides(fill = "none")
  if (!is.null(encoding[["colourTitle"]]) || identical(encoding[["field"]], "percentile"))
    plot <- plot + ggplot2::theme(legend.title.position = "top")
  if (typed)
    plot <- plot + ggplot2::guides(
      colour   = ggplot2::guide_legend(order = 1, override.aes = list(linetype = 1, shape = 16)),
      linetype = ggplot2::guide_legend(order = 2, override.aes = list(colour = "grey40")),
      shape    = ggplot2::guide_legend(order = 2, override.aes = list(colour = "grey40")))
  if (length(colours) == 1L)
    plot <- plot + ggplot2::guides(colour = "none")
  if (length(colours) == 1L && !typed)
    plot <- plot + ggplot2::theme(legend.position = "none")
  for (layer in plot$layers)
    if (inherits(layer$geom, "GeomRibbon"))
      layer$show.legend <- FALSE
  plot <- plot + ggplot2::labs(caption = NULL)
  axes <- try(
    .sapPredictionPrettyAxes(plot, quantileAxis = timeAxis && type == "quantile", probabilityAxis = type == "survival"),
    silent = TRUE
  )
  if (jaspBase::isTryError(axes)) {
    result <- createJaspPlot()
    result$setError(conditionMessage(attr(axes, "condition")))
    return(result)
  }
  plot <- axes
  if (timeAxis && type %in% c("mean", "median"))
    plot <- plot + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank())
  panelCount <- length(unique(data[["Panel"]]))
  size       <- .sapPredictionPlotSize(plot, options, if (panelCount > 1L) 650 else 550,
    if (panelCount > 1L) 220 * ceiling(panelCount / 2) + 130 else 320)
  result <- createJaspPlot(width = size[["width"]], height = size[["height"]])
  result$plotObject <- plot
  return(result)
}
