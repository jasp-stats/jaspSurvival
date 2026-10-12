#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.sapModelPredictions <- function(jaspResults, options) {

  if (options[["predictionsPredictFor"]] != "variables")
    return()

  selected    <- names(.sapPredictionTypes)[vapply(.sapModelPredictionOptions, function(name) options[[name]], logical(1))]
  mergeTables <- options[["predictionsMergeTablesAcrossMeasures"]] && options[["predictionsOutputPlotType"]] != "contour"

  # Display all figures before the tables, in outcome-control order.
  for (plot in c(TRUE, FALSE)) {
    if (!options[[if (plot) "predictionsOutputPlot" else "predictionsOutputTable"]])
      next

    for (measure in selected) {
      lifetime <- measure %in% c("survivalTime", "relativeSurvivalTime")
      if (measure == "relativeSurvivalTime" && !.sapRelativeLifeAvailable(options))
        next
      if (!plot && !lifetime && mergeTables)
        next

      summaries <- if (lifetime) .sapSelectedLifetimeSummaries(options) else "quantile"
      for (summary in summaries)
        .sapPredictionOutput(jaspResults, options, measure, plot = plot, summary = summary)
    }
  }

  if (options[["predictionsOutputTable"]] && mergeTables)
    .sapLifeTimeTable(jaspResults, options)

  return()
}

# One output contract for all survival prediction measures.
.sapPredictionOutput <- function(jaspResults, options, measure, plot = FALSE, summary = "quantile") {

  specification <- switch(measure,
    "survivalTime" = list(type = "quantile", table = "survivalTimeTable", plot = "survivalTimePlot",
                          title = gettext("Predicted Survival Time"), position = c(3.11, 3.01)),
    "relativeSurvivalTime" = list(type = "quantile", table = "relativeSurvivalTimeTable", plot = "relativeSurvivalTimePlot",
      title = gettext("Relative Survival Time"), position = c(3.12, 3.02)),
    "survivalProbability" = list(type = "survival", table = "survivalProbabilityTable", plot = "survivalProbabilityPlot",
                                 title = gettext("Predicted Survival Probability"),
                                 position = c(3.13, 3.03)),
    "failureProbability" = list(type = "survival", table = "failureProbabilityTable", plot = "failureProbabilityPlot",
                                title = gettext("Predicted Failure Probability"), position = c(3.14, 3.04)),
    "hazard" = list(type = "hazard", table = "hazardTable", plot = "hazardPlot",
                    title = gettext("Predicted Hazard"), position = c(3.15, 3.05)),
    "cumulativeHazard" = list(type = "cumhaz", table = "cumHazardTable", plot = "cumulativeHazardPlot",
                              title = gettext("Predicted Cumulative Hazard"), position = c(3.16, 3.06)),
    "restrictedMeanSurvivalTime" = list(type = "rmst", table = "rmstTable", plot = "restrictedMeanSurvivalTimePlot",
                                        title = gettext("Predicted Restricted Mean Survival Time"), position = c(3.17, 3.07))
  )
  if (measure %in% c("survivalTime", "relativeSurvivalTime") && summary != "quantile") {
    specification[["type"]] <- summary
    prefix <- paste0(summary, "SurvivalTime")
    if (measure == "relativeSurvivalTime")
      prefix <- paste0("relative", toupper(substr(prefix, 1L, 1L)), substring(prefix, 2L))
    specification[["table"]] <- paste0(prefix, "Table")
    specification[["plot"]] <- paste0(prefix, "Plot")
    specification[["title"]] <- .sapPredictionTitle(summary, options, measure)
    specification[["position"]] <- specification[["position"]] + if (summary == "median") .001 else .002
  }
  name <- specification[[if (plot) "plot" else "table"]]
  if (!is.null(jaspResults[[name]]))
    return()

  output <- "modelPrediction"
  if (plot && .sapMergePlots(options, output)) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = output)
  } else {
    fit <- .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
    if (plot)
      fit <- .sapNestFit(fit)
  }
  commonContourLevels <- NULL
  if (plot && options[["predictionsOutputPlotType"]] == "contour" && .sapPredictionContourReady(options) &&
      options[["predictionsContoursLevels"]] == "number" && options[["predictionsContoursCommonAcrossFigures"]]) {
    commonContourLevels <- try(.sapPredictionCommonContourLevels(fit, options, specification[["type"]], measure), silent = TRUE)
  }
  builder <- if (plot)
    function(fit, options) .sapModelPredictionPlot(fit, options, type = specification[["type"]], measure = measure, contourLevels = commonContourLevels)
  else
    function(fit, options) .sapModelPredictionTable(fit, options, type = specification[["type"]], measure = measure)

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = builder,
    name           = name,
    title          = specification[["title"]],
    dependencies   = .sapPredictionDependencies(options, measure, plot),
    position       = specification[["position"]][if (plot) 2 else 1]
  )

  return()
}

.sapPredictionDependencies <- function(options, measure, plot = FALSE) {

  survivalTime <- measure %in% c("survivalTime", "relativeSurvivalTime")
  dependencies <- c(
    "predictionsPredictFor",
    .sapGetDependencies(options), .sapSelectedOutputDependencies,
    if (survivalTime) .sapLifetimeSummaryOptions,
    .sapModelPredictionOptions[match(measure, names(.sapPredictionTypes))],
    if (plot) "predictionsOutputPlot" else "predictionsOutputTable", .sapModelPredictionDisplayDependencies, .sapModelPredictionRoleDependencies, .sapModelPredictionEvaluationDependencies,
    if (survivalTime) .sapPredictionQuantileDependencies else .sapPredictionTimeDependencies,
    .sapPredictionCiDependencies, "predictionsTimeIntervals", "predictionsRoundTimeTableValues"
  )
  if (!survivalTime)
    dependencies <- c(dependencies, "predictionsMergeTablesAcrossMeasures", if (!plot) "predictionsRoundTimeTableValues")
  if (plot) {
    dependencies <- c(dependencies, "predictionsDisplayCensoringEvents", "predictionsDisplayObservedData",
      "predictionsDisplayCombinePredictorPlots", "predictionsDisplayConnectXAxisLevels", "predictionsSurvivalTimeYAxisScale", .sapPredictionAxisSettingOptions,
      .sapPredictionContourLevelOptions, if (options[["predictionsOutputPlotType"]] == "contour") .sapPredictionHeatmapOptions(measure))
    dependencies <- c(dependencies, "predictionsMergeSeriesAcrossDistributions", "predictionsMergeSeriesAcrossSubgroups", "colorPalette", "plotLegend", "plotTheme")
    if (options[["analysisType"]] == "mixture")
      dependencies <- c(dependencies, "predictionsMergeSeriesAcrossComponents")
    if (measure %in% c("survivalProbability", "failureProbability"))
      dependencies <- c(dependencies, "predictionsDisplayKaplanMeier", "predictionsDisplayCensoringEvents",
                         "predictionsProbabilityYAxisScale")
  }

  return(unique(dependencies))
}

.sapLifeTimeTable            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["lifeTimeTable"]]))
    return()

  if (!any(vapply(.sapModelPredictionTimeOptions, function(name) options[[name]], logical(1))))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies, .sapPredictionCiDependencies,
                          "predictionsPredictFor",
                          .sapModelPredictionOptions, "predictionsOutputTable", .sapModelPredictionDisplayDependencies, .sapModelPredictionRoleDependencies, .sapModelPredictionEvaluationDependencies, "predictionsMergeTablesAcrossMeasures",
                          .sapPredictionTimeDependencies
  )

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = .sapLifeTimeTableFun,
    name           = "lifeTimeTable",
    title          = gettext("Life Time Table"),
    dependencies   = outputDependencies,
    position       = 3.13
  )

  return()
}

.sapSummaryPredictions <- function(fit, ..., ci, seed = NULL, cl = .95, B = 1000L,
  direction = "twoSided", context = NULL, transform = identity, type = "survival", fn = NULL, start = 0) {

  if (type %in% c("survival", "hazard", "mean") && is.null(fn)) fn <- .sapNativePredictionFunction(fit, type)
  activeBound <- .sapConstraintActive(fit)
  includeCI   <- ci && .sapUsableCovariance(fit)
  messages    <- character(0)
  predictionX <- attr(fit, "predictionX", exact = TRUE)
  captureWarning <- function(w) {
    if (!.sapRetainedFactorContrastWarning(w, fit)) messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  data        <- withCallingHandlers({
    if (is.null(predictionX))
      summary(fit, ..., type = type, fn = fn, start = start, ci = FALSE)
    else
      summary(fit, ..., X = predictionX, type = type, fn = fn, start = start, ci = FALSE)
  }, warning = captureWarning)
  bounds <- NULL
  if (includeCI) {
    bounds <- withCallingHandlers({
      rawsim <- .sapJointDraws(fit, B, seed, context)
      .sapPredictionBounds(fit, data, type, fn, start, B, rawsim, transform, context, cl, direction)
    }, warning = captureWarning)
    messages <- c(messages, attr(bounds, "predictionWarnings"))
  }
  predictionLabels <- attr(fit, "predictionLabels", exact = TRUE)
  if (!is.null(predictionLabels))
    names(data) <- predictionLabels
  for (i in seq_along(data)) {
    data[[i]][["est"]] <- transform(data[[i]][["est"]])
    data[[i]][["lcl"]] <- rep(NA_real_, nrow(data[[i]]))
    data[[i]][["ucl"]] <- rep(NA_real_, nrow(data[[i]]))
    if (!is.null(bounds)) {
      shared <- ncol(bounds) == nrow(data[[i]])
      index  <- if (shared) seq_len(nrow(data[[i]])) else (i - 1L) * nrow(data[[i]]) + seq_len(nrow(data[[i]]))
      data[[i]][["lcl"]] <- bounds["lower", index]
      data[[i]][["ucl"]] <- bounds["upper", index]
    }
    if (ci && !activeBound && fit[["npars"]] == 0L) {
      for (field in .sapBoundFields(direction))
        data[[i]][[field]][is.finite(data[[i]][["est"]])] <- data[[i]][["est"]][is.finite(data[[i]][["est"]])]
    }
    if (anyNA(data[[i]][["est"]]))
      messages <- c(messages, gettext("Some predictions could not be evaluated and are shown as missing."))
    if (any(is.infinite(data[[i]][["est"]])))
      messages <- c(messages, gettext("Some requested quantities are infinite or exceed numerical range."))
    if (ci && !activeBound && anyNA(data[[i]][.sapBoundFields(direction)]))
      messages <- c(messages, gettext("Some confidence intervals could not be evaluated and are shown as missing."))
  }
  attr(data, "predictionWarnings") <- unique(messages)

  return(data)
}

.sapPredictionTitle <- function(type, options, measure = type) {

  if (type %in% c("mean", "median")) {
    if (measure == "relativeSurvivalTime")
      return(if (type == "mean") gettext("Relative Mean Survival Time") else gettext("Relative Median Survival Time"))
    return(if (type == "mean") gettext("Mean Survival Time") else gettext("Median Survival Time"))
  }
  if (measure == "relativeSurvivalTime") return(gettext("Relative Survival Time"))
  if (measure == "failureProbability") return(gettext("Failure Probability"))
  return(switch(type,
    "quantile" = gettext("Survival Time"),
    "survival" = gettext("Survival Probability"),
    "hazard"   = gettext("Hazard"),
    "cumhaz"   = gettext("Cumulative Hazard"),
    "rmst"     = gettext("Restricted Mean Survival Time")
  ))
}

.sapPredictionArguments <- function(fit, options, type, at = NULL, measure = type) {

  arguments <- list(
    fit       = fit,
    type      = type,
    ci        = options[["predictionsDisplayConfidenceIntervals"]],
    cl        = options[["predictionsDisplayConfidenceIntervalsLevel"]],
    B         = options[["confidenceIntervalSimulationDraws"]],
    seed      = if (options[["setSeed"]]) options[["seed"]],
    direction = options[["confidenceIntervalBounds"]],
    context   = options[["simulationContext"]],
    fn        = if (measure == "failureProbability") .sapNativePredictionFunction(fit, "failure")
  )
  if (!is.null(at))
    arguments[[if (type == "quantile") "quantiles" else "t"]] <- at

  return(arguments)
}

.sapPredictionOutcomeScale <- function(options, type) {

  if (type %in% c("quantile", "mean", "median")) {
    if (options[["predictionsSurvivalTimeYAxisScale"]] == "log")
      return(ggplot2::scale_y_log10())
    return(NULL)
  }
  if (type != "survival")
    return(NULL)
  if (options[["plotTheme"]] == "detailed")
    return(.sapDetailedPredictionProbabilityScale(options))

  return(switch(options[["predictionsProbabilityYAxisScale"]],
    none = NULL,
    log = ggplot2::scale_y_log10(),
    logmlogmp = ggplot2::scale_y_continuous(
      transform = scales::new_transform("logmlogmp", function(p) log(-log1p(-p)), function(x) -expm1(-exp(x)))
    )
  ))
}

.sapKaplanMeierStepData          <- function(dataset, options, failureProbability) {

  kmFit <- survival::survfit(
    formula = .saGetFormula(options, type = "KM"),
    type    = "kaplan-meier",
    data    = dataset,
    weights = .sapCaseWeights(dataset, options),
    conf.int = options[["predictionsDisplayConfidenceIntervalsLevel"]]
  )
  kmTable <- summary(kmFit) # , times = optionsSequence
  kmTable <- with(kmTable, data.frame(
    at       = time,
    estimate = surv,
    lCi      = lower,
    uCi      = upper
  ))
  kmTable <- rbind(data.frame(at = 0, estimate = 1, lCi = 1, uCi = 1), kmTable)

  if (failureProbability) {
    kmTable$estimate <- 1 - kmTable$estimate
    kmTable[c("lCi", "uCi")] <- 1 - kmTable[c("uCi", "lCi")]
  }

  # transform into a step function
  kmTable <- kmTable[rep(seq_len(nrow(kmTable)), each = 2), ]
  kmTable$at[seq_len(nrow(kmTable) - 1)] <- kmTable$at[seq_len(nrow(kmTable) - 1) + 1]

  # extend the last step to match the last data point
  if (max(kmTable$at) < max(dataset[[options[["timeToEvent"]]]])) {
    kmTable <- rbind(kmTable, data.frame(
      at       = max(dataset[[options[["timeToEvent"]]]]),
      estimate = kmTable[["estimate"]][nrow(kmTable)],
      lCi      = kmTable[["lCi"]][nrow(kmTable)],
      uCi      = kmTable[["uCi"]][nrow(kmTable)]
    ))
  }

  return(kmTable)
}

.sapPredictionPlotAddTheme        <- function(plot, options) {

  if (options[["plotTheme"]] == "jasp") {
    plot <- plot + jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = options[["plotLegend"]])
  } else {
    plot <- plot +
      switch(
        options[["plotTheme"]],
        "whiteBackground" = ggplot2::theme_bw()       + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "light"           = ggplot2::theme_light()    + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "detailed"        = ggplot2::theme_light()    + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "minimal"         = ggplot2::theme_minimal()  + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "pubr"            = jaspGraphs::themePubrRaw(legend = options[["plotLegend"]]),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = switch(
          options[["plotLegend"]],
          "none"   = "none",
          "bottom" = "bottommiddle",
          "right"  = "bottomright",
          "top"    = "topmiddle",
          "left"   = "bottomleft"
        ))
      )
  }

  return(.sapPlotCanvasPadding(plot))
}
# Detailed probability axes use the same displayed probability as ordinary plots.
.sapDetailedPredictionProbabilityScale <- function(options) {

  transformation <- options[["predictionsProbabilityYAxisScale"]]
  if (transformation == "none") return(ggplot2::scale_y_continuous(breaks = seq(0, 1, .1),
    limits = c(0, 1), minor_breaks = seq(0, 1, .05), oob = scales::oob_keep))
  breaks <- c(.001, .005, .01, .02, .05, .1, .5, .8, .9)
  if (transformation == "log") return(ggplot2::scale_y_log10(breaks = breaks,
    limits = c(.001, 1), oob = scales::oob_keep))
  probabilityTransform <- scales::new_transform("logmlogmp", function(p) log(-log1p(-p)), function(x) -expm1(-exp(x)))
  return(ggplot2::scale_y_continuous(breaks = c(breaks, .95, .98, .99, .999), limits = c(.001, .999),
    transform = probabilityTransform, oob = scales::oob_keep))
}

.sapOptions2PredictionQuantile  <- function(options) {

  if (options[["predictionsLifetimePercentilesGrid"]] == "quantiles") {

    setQuantiles <- seq(0, 100, length.out = options[["predictionsLifetimePercentilesIntervals"]] + 1L)

  } else if (options[["predictionsLifetimePercentilesGrid"]] == "sequence") {

    steps        <- .sapPredictionSequenceSettings(options, "predictionsLifetimePercentiles", 100, probability = TRUE)
    setQuantiles <- seq(steps[["From"]], steps[["To"]], by = steps[["Step"]])

  } else if (options[["predictionsLifetimePercentilesGrid"]] == "custom") {

    setQuantiles <- options[["predictionsLifetimePercentilesValues"]]
    setQuantiles <- .sapCleanCustomOptions(setQuantiles, gettext("Lifetime percentiles must be finite values separated by commas, such as '25, 50, 75'."))
    setQuantiles <- sort(setQuantiles)
    if (any(setQuantiles < 0 | setQuantiles > 100))
      .quitAnalysis(gettext("Lifetime percentiles must be between 0 and 100."))

  }

  return(setQuantiles / 100)
}

.sapPredictionSequenceSettings <- function(options, prefix, maximum, probability = FALSE) {

  defaults <- c(From = 0, Step = maximum / 10, To = maximum)
  values   <- vapply(names(defaults), function(field) {
    text  <- trimws(options[[paste0(prefix, field)]])
    value <- if (nzchar(text)) .sapNumericExpression(text) else defaults[[field]]
    if (!.sapFiniteNumeric(value, expectedLength = 1L))
      .quitAnalysis(gettext("Sequence values must be finite numeric expressions."))
    as.numeric(value)
  }, numeric(1))
  if (values[["From"]] < 0 || values[["To"]] <= values[["From"]] || values[["Step"]] <= 0 ||
      (probability && any(values[c("From", "To")] > 100)))
    .quitAnalysis(if (probability) gettext("Percentile sequences require 0 <= From < To <= 100 and a positive Step.") else
      gettext("Time sequences require 0 <= From < To and a positive Step."))
  return(values)
}

.sapOptions2PredictionTime      <- function(options, fit, type = "time", plot = FALSE) {

  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  minTime <- min(time[time > 0])
  maxTime <- max(time[time < Inf])
  logTime <- if (type %in% c("mixtureComponents", "density"))
    options[[if (type == "density") "densityPlotXAxisTransformation" else "mixtureComponentPlotXAxisTransformation"]] == "log" else
    plot && options[["predictionsXAxisTransformation"]] == "log"

  # Include both endpoints; plotting uses a smooth grid independently of table intervals.
  number <- if (plot) 101L else options[["predictionsTimeIntervals"]] + 1L

  if (options[["predictionsTimeGrid"]] == "quantiles") {

    # Log time excludes zero for survival and mixture component plots.
    if (logTime) {
      setTime <- exp(seq(log(minTime), log(maxTime), length.out = number))
    } else {
      setTime <- seq(0, maxTime, length.out = number)
      if (options[["predictionsRoundTimeTableValues"]])
        setTime <- unique(round(setTime))
    }

  } else if (options[["predictionsTimeGrid"]] == "sequence") {

    steps    <- .sapPredictionSequenceSettings(options, "predictionsTime", maxTime)
    stepFrom <- steps[["From"]]
    stepTo   <- steps[["To"]]

    # Log time excludes zero for survival and mixture component plots.
    if (logTime && stepFrom == 0)
      stepFrom <- minTime

    if (plot) {
      setTime <- if (logTime)
        exp(seq(log(stepFrom), log(stepTo), length.out = number))
        else seq(stepFrom, stepTo, length.out = number)
    } else {
      setTime <- seq(stepFrom, stepTo, by = steps[["Step"]])
      if (!logTime && options[["predictionsRoundTimeTableValues"]])
        setTime <- unique(round(setTime))
    }

  } else if (options[["predictionsTimeGrid"]] %in% c("customRelative", "customAbsolute")) {

    setTime <- if (options[["predictionsTimeGrid"]] == "customRelative")
      .sapPredictionRelativeTimes(options) * maxTime
    else if (!nzchar(trimws(options[["predictionsTimeValues"]]))) c(.25, .5, .75) * maxTime
    else .sapPredictionAbsoluteTimes(options)

    # Log time excludes zero for survival and mixture component plots.
    if (logTime) {
      setTime[setTime <= 0] <- minTime
    }

    if (plot) {
      setTime <- sort(unique(c(setTime, seq(min(setTime), max(setTime), length.out = number))))
    }
  }

  return(setTime)
}

.sapPredictionRelativeTimes <- function(options) {

  percentages <- .sapCleanCustomOptions(options[["predictionsTimePercentages"]], gettext("Follow-up percentages must be finite values separated by commas, such as '25, 50, 75'."))
  if (any(percentages < 0 | percentages > 100)) .quitAnalysis(gettext("Follow-up percentages must be between 0 and 100."))
  return(sort(percentages) / 100)
}

.sapPredictionAbsoluteTimes <- function(options) {

  values <- .sapCleanCustomOptions(options[["predictionsTimeValues"]], gettext("Times or horizons must be finite values separated by commas."))
  if (any(values < 0)) .quitAnalysis(gettext("Times or horizons must be greater than or equal to 0."))
  return(sort(values))
}

.sapCleanCustomOptions          <- function(x, message) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  if (!nzchar(x))
    .quitAnalysis(message)

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || any(!is.finite(x)))
    .quitAnalysis(message)

  return(x)
}
