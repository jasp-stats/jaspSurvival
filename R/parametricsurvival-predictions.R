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

# One output contract for all survival prediction measures.
.sapPredictionOutput <- function(jaspResults, options, measure, plot = FALSE) {

  specification <- switch(measure,
    "survivalTime" = list(type = "quantile", table = "survivalTimeTable", plot = "survivalTimePlot",
                          title = gettext("Predicted Survival Time"), position = c(3.01, 3.02)),
    "survivalProbability" = list(type = "survival", table = "survivalProbabilityTable", plot = "survivalProbabilityPlot",
                                 title = if (options[["survivalProbabilityAsFailureProbability"]])
                                   gettext("Predicted Failure Probability") else gettext("Predicted Survival Probability"),
                                 position = c(3.11, 3.12)),
    "hazard" = list(type = "hazard", table = "hazardTable", plot = "hazardPlot",
                    title = gettext("Predicted Hazard"), position = c(3.21, 3.22)),
    "cumulativeHazard" = list(type = "cumhaz", table = "cumHazardTable", plot = "cumulativeHazardPlot",
                              title = gettext("Predicted Cumulative Hazard"), position = c(3.31, 3.32)),
    "restrictedMeanSurvivalTime" = list(type = "rmst", table = "rmstTable", plot = "restrictedMeanSurvivalTimePlot",
                                        title = gettext("Predicted Restricted Mean Survival Time"), position = c(3.41, 3.42))
  )
  name <- specification[[if (plot) "plot" else "table"]]
  if (!is.null(jaspResults[[name]]))
    return()

  output <- if (measure == "survivalTime") "survivalTime" else "lifeTime"
  if (plot && .sapMergePlots(options, output)) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = output)
  } else {
    fit <- .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
    if (plot)
      fit <- .sapNestFit(fit)
  }
  builder <- if (plot)
    function(fit, options) .sapCreatePredictionPlotWrapper(fit, options, type = specification[["type"]])
  else
    function(fit, options) .sapCreatePredictionTableWrapper(fit, options, type = specification[["type"]])

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

  survivalTime <- measure == "survivalTime"
  grid <- if (survivalTime) "SurvivalTime" else "LifeTime"
  dependencies <- c(
    .sapGetDependencies(options), .sapSelectedOutputDependencies,
    paste0(measure, if (plot) "Plot" else "Table"),
    paste0("predictions", grid, c("StepsType", "StepsNumber", "StepsFrom", "StepsSize", "StepsTo", "Custom")),
    .sapPredictionCiDependencies
  )
  if (!survivalTime)
    dependencies <- c(dependencies, "lifeTimeMergeTablesAcrossMeasures", if (!plot) "predictionsLifeTimeRoundSteps")
  if (measure == "survivalProbability")
    dependencies <- c(dependencies, "survivalProbabilityAsFailureProbability")
  if (plot) {
    output <- if (survivalTime) "survivalTime" else "lifeTime"
    dependencies <- c(dependencies, paste0(output, c("MergePlotsAcrossDistributions", "MergePlotsAcrossFactors", "MergePlotsAcrossSubgroups")), "colorPalette", "plotLegend", "plotTheme")
    if (options[["analysisType"]] == "mixture")
      dependencies <- c(dependencies, paste0(output, "MergePlotsAcrossComponents"))
    if (measure == "survivalProbability")
      dependencies <- c(dependencies, "survivalProbabilityPlotKaplanMeier", "survivalProbabilityPlotCensoringEvents",
                         "survivalProbabilityPlotTransformXAxis", "survivalProbabilityPlotTransformYAxis")
  }

  return(dependencies)
}

.sapLifeTimeTable            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["lifeTimeTable"]]))
    return()

  if (!options[["survivalProbabilityTable"]] && !options[["hazardTable"]] && !options[["cumulativeHazardTable"]] && !options[["restrictedMeanSurvivalTimeTable"]])
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies, .sapPredictionCiDependencies,
                          "survivalProbabilityTable", "hazardTable", "cumulativeHazardTable", "restrictedMeanSurvivalTimeTable", "lifeTimeMergeTablesAcrossMeasures",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom", "survivalProbabilityAsFailureProbability"
  )

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = .sapLifeTimeTableFun,
    name           = "lifeTimeTable",
    title          = gettext("Life Time Table"),
    dependencies   = outputDependencies,
    position       = 3.11
  )

  return()
}

.sapSummaryPredictions <- function(fit, ..., ci, seed = NULL) {

  activeBound <- .sapConstraintActive(fit)
  includeCI <- ci && !activeBound && all(is.finite(fit[["cov"]]))
  # Reset each simulation so output order and cached fits do not change its draws.
  if (includeCI && !is.null(seed))
    jaspBase::.setSeedJASP(list(setSeed = TRUE, seed = seed))
  messages <- character(0)
  predictionX <- attr(fit, "predictionX", exact = TRUE)
  data <- withCallingHandlers(if (is.null(predictionX)) summary(fit, ..., ci = includeCI) else
    summary(fit, ..., X = predictionX, ci = includeCI), warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  predictionLabels <- attr(fit, "predictionLabels", exact = TRUE)
  if (!is.null(predictionLabels))
    names(data) <- predictionLabels
  for (i in seq_along(data)) {
    if (!"lcl" %in% names(data[[i]])) data[[i]][["lcl"]] <- rep(NA_real_, nrow(data[[i]]))
    if (!"ucl" %in% names(data[[i]])) data[[i]][["ucl"]] <- rep(NA_real_, nrow(data[[i]]))
    if (anyNA(data[[i]][["est"]]))
      messages <- c(messages, gettext("Some predictions could not be evaluated and are shown as missing."))
    if (any(is.infinite(data[[i]][["est"]])))
      messages <- c(messages, gettext("Some requested quantities are infinite or exceed numerical range."))
    if (ci && !activeBound && anyNA(data[[i]][c("lcl", "ucl")]))
      messages <- c(messages, gettext("Some confidence intervals could not be evaluated and are shown as missing."))
  }
  attr(data, "predictionWarnings") <- unique(messages)

  return(data)
}

.sapPredictionTitle <- function(type, options) {
  return(switch(type,
    "quantile" = gettext("Survival Time"),
    "survival" = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"),
    "hazard"   = gettext("Hazard"),
    "cumhaz"   = gettext("Cumulative Hazard"),
    "rmst"     = gettext("Restricted Mean Survival Time")
  ))
}
.sapPredictAt <- function(fit, options, type, at) {

  arguments <- list(fit = fit, type = type,
    ci = options[["predictionsConfidenceInterval"]], cl = options[["predictionsConfidenceIntervalLevel"]],
    B = options[["confidenceIntervalSimulationDraws"]], seed = if (options[["setSeed"]]) options[["seed"]])
  arguments[[if (type == "quantile") "quantiles" else "t"]] <- at

  return(do.call(.sapSummaryPredictions, arguments))
}

.sapCreatePredictionTableWrapper <- function(fit, options, type) {

  atTitle       <- if (type == "quantile") gettext("Quantile") else gettext("Time")
  estimateTitle <- .sapPredictionTitle(type, options)

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit)) {
    tempTable <- .sapCreatePredictionTable(options, atTitle = atTitle, estimateNames = "", estimateTitles = estimateTitle)
    return(tempTable)
  }

  # if there is any continuous predictor, the output is averaged across the predictors matrix
  if (type == "quantile") {
    optionsSequence <- .sapOptions2PredictionQuantile(options)
  } else {
    optionsSequence <- .sapOptions2PredictionTime(options, fit)
  }

  data <- try(.sapPredictAt(fit, options, type, optionsSequence))

  # error handling for divergent integrals
  if (jaspBase::isTryError(data)) {
    tempTable <- .sapCreatePredictionTable(options, atTitle = atTitle, estimateNames = "", estimateTitles = estimateTitle)
    tempTable$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempTable)
  }

  predictionWarnings <- attr(data, "predictionWarnings")
  dataLength <- length(data)

  for (i in seq_along(data)) {
    data[[i]]           <- data[[i]][,-1]
    colnames(data[[i]]) <- c("estimate", "lCi", "uCi")

    # transform survival to failure if requested
    if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
      data[[i]]$estimate <- 1 - data[[i]]$estimate
      data[[i]][c("lCi", "uCi")] <- 1 - data[[i]][c("uCi", "lCi")]
    }
  }

  estimateTitles <- names(data)
  names(data)    <- paste0("par", seq_along(data))
  data           <- do.call(cbind, data)

  tempTable <- .sapCreatePredictionTable(
    options        = options,
    atTitle        = atTitle,
    estimateNames  = paste0("par", 1:dataLength, "."),
    estimateTitles = if (dataLength == 1) estimateTitle else estimateTitles
  )

  # add remaining information
  data$at              <- optionsSequence
  data <- .sapAddRowModelInformation(data, fit)

  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    tempTable$addFootnote(attr(fit, "label"))
  for (message in predictionWarnings)
    tempTable$addFootnote(message)

  tempTable$setData(data)
  tempTable$showSpecifiedColumnsOnly <- TRUE

  return(tempTable)
}
.sapLifeTimeTableWrapper         <- function(fit, options, type, timeSequence) {

  tempData           <- .sapPredictAt(fit, options, type, timeSequence)
  predictionWarnings <- attr(tempData, "predictionWarnings")
  if (length(tempData) > 1)
    stop(errorCondition(gettext("Life time tables cannot be merged when a model produces multiple predictions. Disable 'Merge tables across measures'."), class = "sapMultiplePredictionsError"))
  tempData           <- tempData[[1]][,-1]
  colnames(tempData) <- c("estimate", "lCi", "uCi")

  # transform survival to failure if requested
  if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
    tempData$estimate <- 1 - tempData$estimate
    tempData[c("lCi", "uCi")] <- 1 - tempData[c("uCi", "lCi")]
  }

  attr(tempData, "predictionWarnings") <- predictionWarnings
  return(tempData)
}
.sapLifeTimePredictionError <- function(prediction, message) {

  condition <- attr(prediction, "condition")
  if (inherits(condition, "sapMultiplePredictionsError"))
    return(conditionMessage(condition))

  return(message)
}

.sapLifeTimeTableFun             <- function(fit, options) {

  tempTable <- createJaspTable()
  .sapAddModelColumns(tempTable, options, output = "coefficientsCovarianceMatrix")
  tempTable$addColumnInfo(name = "at", title = gettext("Time"), type = "number")

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(tempTable)

  timeSequence <- .sapOptions2PredictionTime(options, fit)
  data <- list()

  # add dots to 'estimateName' since cbind merges names with collapse = "."

  measures <- c(survivalProbability = "survival", hazard = "hazard", cumulativeHazard = "cumhaz", restrictedMeanSurvivalTime = "rmst")
  errors <- c(
    survivalProbability = gettext("The model failed to produce survival predictions. Consider simplifying the model."),
    hazard = gettext("The model failed to produce hazard predictions. Consider simplifying the model."),
    cumulativeHazard = gettext("The model failed to produce cumulative predictions. Consider simplifying the model."),
    restrictedMeanSurvivalTime = gettext("The model failed to produce restricted mean survival time predictions. Consider simplifying the model.")
  )
  for (measure in names(measures)) {
    if (!options[[paste0(measure, "Table")]])
      next

    type <- measures[[measure]]
    .sapAddColumnsPredictionTable(tempTable, options,
      estimateTitle = .sapPredictionTitle(type, options), estimateName = paste0(measure, "."))
    prediction <- try(.sapLifeTimeTableWrapper(fit, options, type, timeSequence))
    if (jaspBase::isTryError(prediction)) {
      tempTable <- createJaspTable()
      tempTable$setError(.sapLifeTimePredictionError(prediction, errors[[measure]]))
      return(tempTable)
    }
    data[[measure]] <- prediction
  }

  for (message in unique(unlist(lapply(data, attr, "predictionWarnings"))))
    tempTable$addFootnote(message)
  data <- do.call(cbind, data)

  data$at              <- timeSequence
  data <- .sapAddRowModelInformation(data, fit)

  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    tempTable$addFootnote(attr(fit, "label"))

  tempTable$setData(data)
  tempTable$showSpecifiedColumnsOnly <- TRUE

  return(tempTable)
}

.sapCreatePredictionPlotWrapper <- function(fit, options, type) {
  mergeOption <- if (type == "quantile") "survivalTimeMergePlotsAcrossFactors" else "lifeTimeMergePlotsAcrossFactors"
  return(.sapPlotAcrossFactors(fit, options, mergeOption,
    function(fit, options) .sapCreatePredictionPlotForData(fit, options, type)))
}
.sapCreatePredictionPlotForData <- function(fit, options, type) {

  atTitle       <- if (type == "quantile") gettext("Quantile") else gettext("Time")
  estimateTitle <- .sapPredictionTitle(type, options)

  checkFit <- sapply(fit, jaspBase::isTryError)
  if (!.saSurvivalReady(options) || all(checkFit)) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    return(tempPlot)
  }

  # extract an example fit & dataset (make sure the fit converged)
  tempFit  <- fit[[which.min(checkFit)]]
  tempData <- .sapPlotObservedDataset(tempFit)
  output <- if (type == "quantile") "survivalTime" else "lifeTime"
  mergeSubgroups <- .sapMergePlotsAcrossSubgroups(options, output)

  if (type == "quantile") {
    optionsSequence <- .sapOptions2PredictionQuantile(options)
  } else {
    options[["predictionsLifeTimeRoundSteps"]] <- FALSE
    successfulFits <- fit[!checkFit]
    timeFit <- if (mergeSubgroups) .sapPlotTimeFit(successfulFits) else tempFit
    optionsSequence <- .sapOptions2PredictionTime(options, timeFit, type, plot = TRUE)
    logTime <- type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] == "log"
    yTransform <- if (type == "survival") switch(options[["survivalProbabilityPlotTransformYAxis"]],
      "none" = identity, "log" = log, "logmlogmp" = function(p) log(-log1p(-p))) else identity
    limits <- if (type == "survival") switch(options[["survivalProbabilityPlotTransformYAxis"]],
      "none" = if (options[["plotTheme"]] == "detailed") c(0, 1),
      "log" = log(c(if (options[["plotTheme"]] == "detailed") 0.001 else 0.01, 1)),
      "logmlogmp" = yTransform(if (options[["plotTheme"]] == "detailed") c(0.001, 0.999) else c(0.01, 0.99)))
    evaluate <- function(times) {
      values <- do.call(cbind, lapply(successfulFits, function(model)
        .sapPlotPredictionMatrix(.sapSummaryPredictions(model, type = type, t = times, ci = FALSE))))
      return(if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) 1 - values else values)
    }
    anchors <- try(.sapPlotFeatureTimes(successfulFits, optionsSequence, sparse = type == "rmst"), silent = TRUE)
    optionsSequence <- .sapAdaptivePlotTimes(optionsSequence, evaluate,
      xTransform = if (logTime) log else identity, xInverse = if (logTime) exp else identity,
      yTransform = yTransform, limits = limits, minimum = if (options[["predictionsConfidenceInterval"]]) 65L else 17L,
      maximum = if (options[["predictionsConfidenceInterval"]]) 129L else 201L,
      anchors = if (inherits(anchors, "try-error")) numeric(0) else anchors)
  }

  out <- list()
  predictionWarnings <- character(0)
  for (i in seq_along(fit)) {

    # skip model on error
    if (jaspBase::isTryError(fit[[i]]))
      next

    data <- try(.sapPredictAt(fit[[i]], options, type, optionsSequence))

    # error handling for divergent integrals
    if (jaspBase::isTryError(data)) {
      tempPlot <- createJaspPlot(title = estimateTitle)
      tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
      return(tempPlot)
    }

    predictionWarnings <- c(predictionWarnings, attr(data, "predictionWarnings"))
    levelLabels <- .sapPlotPredictionLevels(data, fit[[i]], options, output, emptyLabel = NA)
    # deal with potentially multiple predictions
    for (j in seq_along(data)) {

      # rename output
      colnames(data[[j]]) <- c("at", "estimate", "lCi", "uCi")

      if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
        data[[j]]$estimate <- 1 - data[[j]]$estimate
        data[[j]][c("lCi", "uCi")] <- 1 - data[[j]][c("uCi", "lCi")]
      }

      # add factor level
      data[[j]]$Level <- levelLabels[if (length(levelLabels) > 1) j else 1]

      # add distribution information
      data[[j]]$Distribution <- .sapSeriesLabel(fit[[i]], options)
    }

    # bind across levels
    out[[i]] <- do.call(rbind, data)
  }

  # bind across models
  out <- do.call(rbind, out)

  # set any Inf to NA
  out[["estimate"]][is.infinite(out[["estimate"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])] <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])] <- NA

  if (!any(is.finite(out[["at"]]) & is.finite(out[["estimate"]]))) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(paste(unique(c(gettext("No finite predictions are available for this plot."), predictionWarnings)), collapse = "\n"))
    return(tempPlot)
  }

  # check how to distribute legend
  hasDistribution <- length(unique(out[["Distribution"]])) > 1
  hasLevel        <- length(unique(out[["Level"]])) > 1
  hasSeries       <- hasDistribution || hasLevel
  colorVariable <- if (hasDistribution && !mergeSubgroups) "Distribution" else if (hasLevel) "Level" else if (hasDistribution) "Distribution"
  lineVariable <- if (hasDistribution && hasLevel) if (mergeSubgroups) "Distribution" else "Level"

  if (!hasSeries) {
    options[["plotLegend"]]   <- "none"
    options[["colorPalette"]] <- "colorblind"
  }

  # compute Kaplan-Meier if needed
  if (type == "survival" && !mergeSubgroups && options[["survivalProbabilityPlotKaplanMeier"]] && options[["censoringType"]] == "right")
    kmTable <- .sapKaplanMeierStepData(tempData, options, failureProbability = options[["survivalProbabilityAsFailureProbability"]])

  if (type == "survival" && mergeSubgroups && options[["censoringType"]] == "right") {
    observedFits <- .sapPlotSubgroupFits(successfulFits)
    if (options[["survivalProbabilityPlotKaplanMeier"]])
      kmTable <- .sapBindPlotData(lapply(observedFits, function(model)
        .sapProbabilityPlotObservedData(.sapPlotObservedDataset(model), model, options,
          function(dataset, options) .sapKaplanMeierStepData(dataset, options, options[["survivalProbabilityAsFailureProbability"]]), output)))
    if (options[["survivalProbabilityPlotCensoringEvents"]])
      censoringData <- .sapBindPlotData(lapply(observedFits, function(model)
        .sapProbabilityPlotObservedData(.sapPlotObservedDataset(model), model, options,
          function(dataset, options) data.frame(censoring = dataset[[options[["timeToEvent"]]]][!dataset[[options[["eventStatus"]]]]]), output)))
  }

  # create a plot
  plot <- ggplot2::ggplot(data = out)

  # add censoring observations if requested
  if (type == "survival" && options[["survivalProbabilityPlotCensoringEvents"]] && options[["censoringType"]] == "right") {
    rugCall <- list(
      data    = if (mergeSubgroups) censoringData else data.frame(censoring = tempData[[options[["timeToEvent"]]]][!tempData[[options[["eventStatus"]]]]]),
      mapping = if (mergeSubgroups) ggplot2::aes(x = censoring, color = Level) else ggplot2::aes(x = censoring),
      sides = "b", alpha = 0.5, size = 0.5
    )
    if (!mergeSubgroups)
      rugCall[["color"]] <- "darkblue"
    plot <- plot + do.call(ggplot2::geom_rug, rugCall)
  }

  # add Kaplan-Meier if needed
  if (type == "survival" && options[["survivalProbabilityPlotKaplanMeier"]] && options[["censoringType"]] == "right") {

    if (options[["predictionsConfidenceInterval"]]) {
      aesCall <- list(
        x        = as.name("at"),
        ymin     = as.name("lCi"),
        ymax     = as.name("uCi"),
        fill     = if (mergeSubgroups) as.name("Level"),
        group    = if (mergeSubgroups) as.name("Level")
      )
      # Start the CI at the first event, omitting the initial survival-one step.
      bandData <- if (mergeSubgroups) do.call(rbind, lapply(split(kmTable, kmTable[["Level"]]), function(data) data[-c(1, 2), , drop = FALSE])) else kmTable[-c(1, 2), ]
      geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = bandData, alpha = 0.10)
      if (!mergeSubgroups) {
        geomCall[["fill"]] <- "grey60"
        geomCall[["color"]] <- "grey60"
      }
      plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
    }

    aesCall <- list(
      x        = as.name("at"),
      y        = as.name("estimate"),
      color    = if (mergeSubgroups) as.name("Level"),
      group    = if (mergeSubgroups) as.name("Level")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = kmTable)
    if (mergeSubgroups) geomCall[["linetype"]] <- 2 else geomCall[["color"]] <- "grey60"
    plot <- plot + do.call(jaspGraphs::geom_line, geomCall)

  }

  # add available model intervals; active-bound fits retain point predictions only
  if (options[["predictionsConfidenceInterval"]] && any(is.finite(out[["lCi"]]) & is.finite(out[["uCi"]]))) {
    aesCall <- list(
      x        = as.name("at"),
      ymin     = as.name("lCi"),
      ymax     = as.name("uCi"),
      fill     = if (!is.null(colorVariable)) as.name(colorVariable),
      linetype = if (!is.null(lineVariable)) as.name(lineVariable)
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), alpha = 0.30)
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)

  }

  # add line
  aesCall <- list(
    x        = as.name("at"),
    y        = as.name("estimate"),
    color    = if (!is.null(colorVariable)) as.name(colorVariable),
    linetype = if (!is.null(lineVariable)) as.name(lineVariable)
  )
  geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]))
  plot <- plot + do.call(jaspGraphs::geom_line, geomCall)

  if (hasSeries) {
    plot <- plot +
      jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
      jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])
  }

  # scale axis & add labels
  if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] == "log") {
    xBreaks <- exp(seq(log(min(out[["at"]], na.rm = TRUE)), log(max(out[["at"]], na.rm = TRUE)), length.out = 5))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(out[["at"]], na.rm = TRUE))
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(.saPlotEstimateRange(
    estimate = out[["estimate"]],
    lCi      = if (options[["predictionsConfidenceInterval"]]) out[["lCi"]],
    uCi      = if (options[["predictionsConfidenceInterval"]]) out[["uCi"]],
    bounded  = type == "survival",
    at       = out[["at"]],
    group    = interaction(lapply(out[c("Distribution", "Level")], factor, exclude = NULL), drop = TRUE)))

  if (type == "survival") {
    plot <- .sapPredictionPlotAddSurvivalAxis(plot, options, xBreaks, yBreaks, atTitle, estimateTitle)
  } else {
    plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep) +
      jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep) +
      ggplot2::ylab(estimateTitle) + ggplot2::xlab(atTitle)
  }

  # themes
  if (type != "survival" && options[["plotTheme"]] == "detailed") {
    options[["plotTheme"]] <- "jasp"
  }
  plot <- .sapPredictionPlotAddTheme(plot, options)
  width <- if (hasDistribution || hasLevel) 550 else 400

  tempPlot <- createJaspPlot(width = width, height = 320)
  tempPlot$plotObject <- plot

  return(tempPlot)
}

.sapKaplanMeierStepData          <- function(dataset, options, failureProbability) {

  kmFit    <- survival::survfit(
    formula = .saGetFormula(options, type = "KM"),
    type    = "kaplan-meier",
    data    = dataset,
    weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]],
    conf.int = options[["predictionsConfidenceIntervalLevel"]]
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

  return(plot)
}
.sapPredictionPlotAddSurvivalAxis <- function(plot, options, xBreaks, yBreaks, atTitle, estimateTitle) {

  ### x-axis
  if (options[["survivalProbabilityPlotTransformXAxis"]] == "none") {
    # no transformation
    plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep)

  } else if (options[["survivalProbabilityPlotTransformXAxis"]] == "log") {
    # log transformation
    atTitle <- gettextf("%1$s (log scale)", atTitle)
    plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), trans = "log", transform = "log", oob = scales::oob_keep)

  }

  ### y-axis
  if (options[["survivalProbabilityPlotTransformYAxis"]] == "none") {
    # no transformation
    if (options[["plotTheme"]] == "detailed") {

      yRange    <- c(0, 1)
      yBreaks   <- seq(0, 1, by = 0.1)

      plot <- plot + ggplot2::scale_y_continuous(
        breaks = yBreaks, limits = yRange, oob = scales::oob_keep,
        minor_breaks = seq(0, 1, by = 0.05)
      )

    } else {

      plot <- plot + jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep)

    }

  } else if (options[["survivalProbabilityPlotTransformYAxis"]] == "log") {
    # log transformation
    estimateTitle <- gettextf("%1$s (log scale)", estimateTitle)

    if (options[["plotTheme"]] == "detailed") {

      yRange    <- c(0.001, 1)
      yBreaks   <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.10, 0.50, 0.80, 0.90)

      plot <- plot + ggplot2::scale_y_continuous(
        breaks = yBreaks, limits = yRange, oob = scales::oob_keep,
        minor_breaks = c(1:20/1000, 2:10/100, 1:9/10),
        transform = "log"
      )

    } else {

      yRange    <- range(yBreaks)
      yRange[1] <- max(0.01, yRange[1])
      yBreaks   <- exp(seq(log(yRange[1]), log(yRange[2]), length.out = 7))

      plot <- plot + jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = yRange, oob = scales::oob_keep, trans = "log", transform = "log")

    }

  } else if (options[["survivalProbabilityPlotTransformYAxis"]] == "logmlogmp") {
    # log-log transformation
    logmlogmp    <- function(x) log(-log1p(-x))
    logmlogmpInv <- function(x) -expm1(-exp(x))
    estimateTitle <- gettextf("%1$s (log(-log(1-p)) scale)", estimateTitle)

    if (options[["plotTheme"]] == "detailed") {

      yRange    <- c(0.001, 0.999)
      yBreaks   <- (c(0.001, 0.005, 0.02, 0.05, 0.10, 0.50, 0.80, 0.90, 0.95, 0.98, 0.99, 0.999))

      plot <- plot + ggplot2::scale_y_continuous(
        breaks = (yBreaks), limits = (yRange), oob = scales::oob_keep,
        minor_breaks = c(99:90/100, 8:1/10, 9:1/100),
        transform = scales::new_transform(name = "logmlogp", transform = logmlogmp, inverse = logmlogmpInv)
      )

    } else {

      yRange    <- range(yBreaks)
      yRange[1] <- max(0.01, yRange[1])
      yRange[2] <- min(0.99, yRange[2])
      yBreaks   <- logmlogmpInv(seq(logmlogmp(yRange[2]), logmlogmp(yRange[1]), length.out = 7))
      probabilityTransform <- scales::new_transform(name = "logmlogp", transform = logmlogmp, inverse = logmlogmpInv)

      plot <- plot + jaspGraphs::scale_y_continuous(
        breaks = (yBreaks), limits = (yRange), oob = scales::oob_keep,
        trans = probabilityTransform, transform = probabilityTransform
      )
    }
  }

  plot <- plot + ggplot2::ylab(estimateTitle) + ggplot2::xlab(atTitle)
  return(plot)
}


.sapOptions2PredictionQuantile  <- function(options) {

  if (options[["predictionsSurvivalTimeStepsType"]] == "quantiles") {

    setQuantiles <- seq(0, 1, length.out = options[["predictionsSurvivalTimeStepsNumber"]] + 1)
    setQuantiles <- setQuantiles[-length(setQuantiles)] # don't predict for 1 as it is infinity

  } else if (options[["predictionsSurvivalTimeStepsType"]] == "sequence") {

    setQuantiles <- seq(options[["predictionsSurvivalTimeStepsFrom"]], options[["predictionsSurvivalTimeStepsTo"]], options[["predictionsSurvivalTimeStepsSize"]])
    setQuantiles <- setQuantiles[setQuantiles < 1]

  } else if (options[["predictionsSurvivalTimeStepsType"]] == "custom") {

    setQuantiles <- options[["predictionsSurvivalTimeCustom"]]
    setQuantiles <- .sapCleanCustomOptions(setQuantiles, gettext("Custom steps for predicted survival time were specified in an incorrect format. Try '0.25, 0.50, 0.75'."))
    setQuantiles <- sort(setQuantiles)
    if (any(setQuantiles < 0 | setQuantiles > 1))
      .quitAnalysis(gettext("Custom steps for predicted survival time must be between 0 and 1."))

  }

  return(setQuantiles)
}
.sapOptions2PredictionTime      <- function(options, fit, type = "time", plot = FALSE) {

  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  minTime <- min(time[time > 0])
  maxTime <- max(time[time < Inf])
  logTime <- (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] == "log") ||
    (type == "mixtureComponents" && options[["mixtureComponentPlotTransformXAxis"]] == "log")

  # plotting preset which makes the plots look smoother than the generated tables
  if (plot) {
    options[["predictionsLifeTimeStepsNumber"]] <- 101
  }

  if (options[["predictionsLifeTimeStepsType"]] == "quantiles") {

    # Log time excludes zero for survival and mixture component plots.
    if (logTime) {
      setTime <- exp(seq(log(minTime), log(maxTime), length.out = options[["predictionsLifeTimeStepsNumber"]]))
    } else {
      setTime <- seq(0, maxTime, length.out = options[["predictionsLifeTimeStepsNumber"]])
      if (options[["predictionsLifeTimeRoundSteps"]])
        setTime <- unique(round(setTime))
    }

  } else if (options[["predictionsLifeTimeStepsType"]] == "sequence") {

    stepFrom <- options[["predictionsLifeTimeStepsFrom"]]
    stepSize <- options[["predictionsLifeTimeStepsSize"]]
    stepTo   <- options[["predictionsLifeTimeStepsTo"]]

    if (stepFrom != "") {
      stepFrom <- as.numeric(trimws(stepFrom, which = "both"))
      if (!is.finite(stepFrom) || stepFrom < 0)
        .quitAnalysis(gettext("Step from for predicted survival time must be a finite, non-negative number."))
    } else {
      stepFrom <- 0
    }
    if (stepTo != "") {
      stepTo <- as.numeric(trimws(stepTo, which = "both"))
      if (!is.finite(stepTo) || stepTo <= 0)
        .quitAnalysis(gettext("Step to for predicted survival time must be a finite, positive number."))
    } else {
      stepTo <- maxTime
    }
    if (stepSize != "") {
      stepSize <- as.numeric(trimws(stepSize, which = "both"))
      if (!is.finite(stepSize) || stepSize <= 0)
        .quitAnalysis(gettext("Step size for predicted survival time must be a finite, positive number."))
    } else {
      stepSize <- (stepTo - stepFrom) / 10
    }
    if (stepTo <= stepFrom)
      .quitAnalysis(gettext("Step to for predicted survival time must be greater than step from."))

    # Log time excludes zero for survival and mixture component plots.
    if (logTime && stepFrom == 0)
      stepFrom <- minTime

    if (plot) {
      setTime <- if (logTime)
        exp(seq(log(stepFrom), log(stepTo), length.out = options[["predictionsLifeTimeStepsNumber"]]))
        else seq(stepFrom, stepTo, length.out = options[["predictionsLifeTimeStepsNumber"]])
    } else {
      setTime <- seq(stepFrom, stepTo, stepSize)
      if (!logTime && options[["predictionsLifeTimeRoundSteps"]])
        setTime <- unique(round(setTime))
    }

  } else if (options[["predictionsLifeTimeStepsType"]] == "custom") {

    setTime <- options[["predictionsLifeTimeCustom"]]
    setTime <- .sapCleanCustomOptions(setTime, gettext("Custom steps for predicted survival time were specified in an incorrect format. Try '0.25, 0.50, 0.75'."))
    setTime <- sort(setTime)
    if (any(setTime < 0))
      .quitAnalysis(gettext("Custom steps for predicted survival time must be greater than or equal to 0."))

    # Log time excludes zero for survival and mixture component plots.
    if (logTime) {
      setTime[setTime <= 0] <- minTime
    }

    if (plot) {
      setTime <- seq(min(setTime), max(setTime), length.out = options[["predictionsLifeTimeStepsNumber"]])
    }
  }

  return(setTime)
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
