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

# predictions tables
.sapSurvivalTimeTable        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalTimeTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  # flatten the list - each model has to get its own matrix because they might differ in parameters etc...
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalTimeTable", "predictionsSurvivalTimeStepsType", "predictionsSurvivalTimeStepsNumber", "predictionsSurvivalTimeStepsFrom",
                          "predictionsSurvivalTimeStepsSize", "predictionsSurvivalTimeStepsTo", "predictionsSurvivalTimeCustom",
                          "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel"
                          )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalTimeTableFun,
    name          = "survivalTimeTable",
    title         = gettext("Predicted Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.01
  )

  return()
}
.sapSurvivalProbabilityTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalProbabilityTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalProbabilityTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom", "survivalProbabilityAsFailureProbability"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalProbabilityTableFun,
    name          = "survivalProbabilityTable",
    title         = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Predicted Failure Probability") else gettext("Predicted Survival Probability"),
    dependencies  = outputDependencies,
    position      = 3.11
  )

  return()
}
.sapHazardTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["hazardTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "hazardTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapHazardTableFun,
    name          = "hazardTable",
    title         = gettext("Predicted Hazard"),
    dependencies  = outputDependencies,
    position      = 3.21
  )

  return()
}
.sapCumHazardTable           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["cumHazardTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "cumulativeHazardTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapCumHazardTableFun,
    name          = "cumHazardTable",
    title         = gettext("Predicted Cumulative Hazard"),
    dependencies  = outputDependencies,
    position      = 3.31
  )

  return()
}
.sapRmstTable                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rmstTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "restrictedMeanSurvivalTimeTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapRmstTableFun,
    name          = "rmstTable",
    title         = gettext("Predicted Restricted Mean Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.41
  )

  return()
}
.sapLifeTimeTable            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["lifeTimeTable"]]))
    return()

  if (!options[["survivalProbabilityTable"]] && !options[["hazardTable"]] && !options[["cumulativeHazardTable"]] && !options[["restrictedMeanSurvivalTimeTable"]])
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "survivalProbabilityTable", "hazardTable", "cumulativeHazardTable", "restrictedMeanSurvivalTimeTable", "lifeTimeMergeTablesAcrossMeasures",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom", "survivalProbabilityAsFailureProbability"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapLifeTimeTableFun,
    name          = "lifeTimeTable",
    title         = gettext("Life Time Table"),
    dependencies  = outputDependencies,
    position      = 3.11
  )

  return()
}

# prediction plots
.sapSurvivalTimePlot        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalTimePlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (.sapMergePlots(options, "survivalTime")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "survivalTime")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalTimePlot", "predictionsSurvivalTimeStepsType", "predictionsSurvivalTimeStepsNumber", "predictionsSurvivalTimeStepsFrom",
                          "predictionsSurvivalTimeStepsSize", "predictionsSurvivalTimeStepsTo", "predictionsSurvivalTimeCustom",
                          "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel", "survivalTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme",
                          if (options[["analysisType"]] == "mixture") "survivalTimeMergePlotsAcrossComponents"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalTimePlotFun,
    name          = "survivalTimePlot",
    title         = gettext("Predicted Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.02
  )

  return()
}
.sapSurvivalProbabilityPlot <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalProbabilityPlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (.sapMergePlots(options, "lifeTime")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "lifeTime")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalProbabilityPlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme",
                          if (options[["analysisType"]] == "mixture") "lifeTimeMergePlotsAcrossComponents",
                          "survivalProbabilityPlotKaplanMeier", "survivalProbabilityPlotCensoringEvents", "survivalProbabilityPlotTransformXAxis", "survivalProbabilityPlotTransformYAxis",
                          "survivalProbabilityAsFailureProbability"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalProbabilityPlotFun,
    name          = "survivalProbabilityPlot",
    title         = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Predicted Failure Probability") else gettext("Predicted Survival Probability"),
    dependencies  = outputDependencies,
    position      = 3.12
  )

  return()
}
.sapHazardPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["hazardPlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (.sapMergePlots(options, "lifeTime")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "lifeTime")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "hazardPlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme",
                          if (options[["analysisType"]] == "mixture") "lifeTimeMergePlotsAcrossComponents"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapHazardPlotFun,
    name          = "hazardPlot",
    title         = gettext("Predicted Hazard"),
    dependencies  = outputDependencies,
    position      = 3.22
  )

  return()
}
.sapCumHazardPlot           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["cumulativeHazardPlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (.sapMergePlots(options, "lifeTime")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "lifeTime")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "cumulativeHazardPlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme",
                          if (options[["analysisType"]] == "mixture") "lifeTimeMergePlotsAcrossComponents"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapCumHazardPlotFun,
    name          = "cumulativeHazardPlot",
    title         = gettext("Predicted Cumulative Hazard"),
    dependencies  = outputDependencies,
    position      = 3.32
  )

  return()
}
.sapRmstPlot                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["restrictedMeanSurvivalTimePlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (.sapMergePlots(options, "lifeTime")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "lifeTime")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "restrictedMeanSurvivalTimePlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme",
                          if (options[["analysisType"]] == "mixture") "lifeTimeMergePlotsAcrossComponents"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapRmstPlotFun,
    name          = "restrictedMeanSurvivalTimePlot",
    title         = gettext("Predicted Restricted Mean Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.42
  )

  return()
}

.sapCreatePredictionTableWrapper <- function(fit, options, type) {

  if (type == "quantile") {
    atTitle <- gettext("Quantile")
  } else {
    atTitle <- gettext("Time")
  }

  estimateTitle <- switch(
    type,
    "quantile"  = gettext("Survival Time"),
    "survival"  = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"),
    "hazard"    = gettext("Hazard"),
    "cumhaz"    = gettext("Cumulative Hazard"),
    "rmst"      = gettext("Restricted Mean Survival Time")
  )

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit)) {
    tempTable <- .sapCreatePredictionTable(options, atTitle = atTitle, estimateNames = "", estimateTitles = estimateTitle)
    return(tempTable)
  }

  # if there is any continuous predictor, the output is averaged across the predictors matrix
  if (type == "quantile") {
    optionsSequence <- .sapOptions2PredictionQuantile(options)
    data  <- try(summary(fit, type = type, quantiles = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
  } else {
    optionsSequence <- .sapOptions2PredictionTime(options, fit)
    data  <- try(summary(fit, type = type, t = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
  }

  # error handling for divergent integrals
  if (jaspBase::isTryError(data)) {
    tempTable <- .sapCreatePredictionTable(options, atTitle = atTitle, estimateNames = "", estimateTitles = estimateTitle)
    tempTable$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempTable)
  }

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
  data$subgroup        <- NA
  data$distribution    <- NA
  data$components      <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$components[1]   <- attr(fit, "components")
  data$model[1]        <- attr(fit, "modelTitle")

  if (!is.null(attr(fit, "label")))
    tempTable$addFootnote(attr(fit, "label"))

  tempTable$setData(data)
  tempTable$showSpecifiedColumnsOnly <- TRUE

  return(tempTable)
}
.sapLifeTimeTableWrapper         <- function(fit, options, type, timeSequence) {

  tempData           <- summary(fit, type = type, t = timeSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]])
  if (length(tempData) > 1)
    stop(errorCondition(gettext("Life time tables cannot be merged when a model produces multiple predictions. Disable 'Merge tables across measures'."), class = "sapMultiplePredictionsError"))
  tempData           <- tempData[[1]][,-1]
  colnames(tempData) <- c("estimate", "lCi", "uCi")

  # transform survival to failure if requested
  if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
    tempData$estimate <- 1 - tempData$estimate
    tempData[c("lCi", "uCi")] <- 1 - tempData[c("uCi", "lCi")]
  }

  return(tempData)
}
.sapLifeTimePredictionError <- function(prediction, message) {

  condition <- attr(prediction, "condition")
  if (inherits(condition, "sapMultiplePredictionsError"))
    return(conditionMessage(condition))

  return(message)
}

.sapSurvivalTimeTableFun         <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "quantile")
  return(tempTable)
}
.sapSurvivalProbabilityTableFun  <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "survival")
  return(tempTable)
}
.sapHazardTableFun               <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "hazard")
  return(tempTable)
}
.sapCumHazardTableFun            <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "cumhaz")
  return(tempTable)
}
.sapRmstTableFun                 <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "rmst")
  return(tempTable)
}
.sapLifeTimeTableFun             <- function(fit, options) {

  tempTable <- createJaspTable()
  .sapAddColumnSubgroup(     tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        tempTable, options, output = "coefficientsCovarianceMatrix")
  tempTable$addColumnInfo(name = "at", title = gettext("Time"), type = "number")

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(tempTable)

  timeSequence <- .sapOptions2PredictionTime(options, fit)
  data <- list()

  # add dots to 'estimateName' since cbind merges names with collapse = "."

  if (options[["survivalProbabilityTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"), estimateName = "survivalProbability.")
    data[["survivalProbability"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "survival", timeSequence = timeSequence))

    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["survivalProbability"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(.sapLifeTimePredictionError(data[["survivalProbability"]], gettext("The model failed to produce survival predictions. Consider simplifying the model.")))
      return(tempTable)
    }
  }

  if (options[["hazardTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Hazard"), estimateName = "hazard.")
    data[["hazard"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "hazard", timeSequence = timeSequence))


    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["hazard"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(.sapLifeTimePredictionError(data[["hazard"]], gettext("The model failed to produce hazard predictions. Consider simplifying the model.")))
      return(tempTable)
    }
  }

  if (options[["cumulativeHazardTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Cumulative Hazard"), estimateName = "cumulativeHazard.")
    data[["cumulativeHazard"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "cumhaz", timeSequence = timeSequence))


    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["cumulativeHazard"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(.sapLifeTimePredictionError(data[["cumulativeHazard"]], gettext("The model failed to produce cumulative predictions. Consider simplifying the model.")))
      return(tempTable)
    }
  }

  if (options[["restrictedMeanSurvivalTimeTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Restricted Mean Survival Time"), estimateName = "restrictedMeanSurvivalTime.")
    data[["restrictedMeanSurvivalTime"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "rmst", timeSequence = timeSequence))


    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["restrictedMeanSurvivalTime"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(.sapLifeTimePredictionError(data[["restrictedMeanSurvivalTime"]], gettext("The model failed to produce restricted mean survival time predictions. Consider simplifying the model.")))
      return(tempTable)
    }
  }

  data <- do.call(cbind, data)

  data$at              <- timeSequence
  data$subgroup        <- NA
  data$distribution    <- NA
  data$components      <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$components[1]   <- attr(fit, "components")
  data$model[1]        <- attr(fit, "modelTitle")

  if (!is.null(attr(fit, "label")))
    tempTable$addFootnote(attr(fit, "label"))

  tempTable$setData(data)
  tempTable$showSpecifiedColumnsOnly <- TRUE

  return(tempTable)
}

.sapCreatePredictionPlotWrapper <- function(fit, options, type) {

  if (type == "quantile") {
    atTitle <- gettext("Quantile")
  } else {
    atTitle <- gettext("Time")
  }

  estimateTitle <- switch(
    type,
    "quantile"  = gettext("Survival Time"),
    "survival"  = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"),
    "hazard"    = gettext("Hazard"),
    "cumhaz"    = gettext("Cumulative Hazard"),
    "rmst"      = gettext("Restricted Mean Survival Time")
  )

  checkFit <- sapply(fit, jaspBase::isTryError)
  if (!.saSurvivalReady(options) || all(checkFit)) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    return(tempPlot)
  }

  # extract an example fit & dataset (make sure the fit converged)
  tempFit  <- fit[[which.min(checkFit)]]
  tempData <- attr(tempFit, "dataset")

  if (type == "quantile") {
    optionsSequence <- .sapOptions2PredictionQuantile(options)
  } else {
    optionsSequence <- .sapOptions2PredictionTime(options, tempFit, type, plot = TRUE)
  }

  out <- list()
  for (i in seq_along(fit)) {

    # skip model on error
    if (jaspBase::isTryError(fit[[i]]))
      next

    if (type == "quantile") {
      data  <- try(summary(fit[[i]], type = type, quantiles = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
    } else {
      data  <- try(summary(fit[[i]], type = type, t = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
    }

    # error handling for divergent integrals
    if (jaspBase::isTryError(data)) {
      tempPlot <- createJaspPlot(title = estimateTitle)
      tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
      return(tempPlot)
    }

    # deal with potentially multiple predictions
    for (j in seq_along(data)) {

      # rename output
      colnames(data[[j]]) <- c("at", "estimate", "lCi", "uCi")

      if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
        data[[j]]$estimate <- 1 - data[[j]]$estimate
        data[[j]][c("lCi", "uCi")] <- 1 - data[[j]][c("uCi", "lCi")]
      }

      # add factor level
      if (length(data) > 1) {
        data[[j]]$Level <- decodeColNames(names(data)[j])
      } else {
        data[[j]]$Level <- NA
      }

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

  # check how to distribute legend
  hasDistribution <- length(unique(out[["Distribution"]])) > 1
  hasLevel        <- length(unique(out[["Level"]])) > 1
  hasSeries       <- hasDistribution || hasLevel

  if (!hasSeries) {
    options[["plotLegend"]]   <- "none"
    options[["colorPalette"]] <- "colorblind"
  }

  # compute Kaplan-Meier if needed
  if (type == "survival" && isTRUE(options[["survivalProbabilityPlotKaplanMeier"]]) && options[["censoringType"]] == "right")
    kmTable <- .sapKaplanMeierStepData(tempData, options, failureProbability = options[["survivalProbabilityAsFailureProbability"]])

  # create a plot
  plot <- ggplot2::ggplot(data = out)

  # add censoring observations if requested
  if (type == "survival" && isTRUE(options[["survivalProbabilityPlotCensoringEvents"]]) && options[["censoringType"]] == "right") {
    plot <- plot + ggplot2::geom_rug(
      data    = data.frame(censoring = tempData[[options[["timeToEvent"]]]][!tempData[[options[["eventStatus"]]]]]),
      mapping = ggplot2::aes(x = censoring),
      sides = "b", color = "darkblue", alpha = 0.5, size = 0.5
    )
  }

  # add Kaplan-Meier if needed
  if (type == "survival" && isTRUE(options[["survivalProbabilityPlotKaplanMeier"]]) && options[["censoringType"]] == "right") {

    if (options[["predictionsConfidenceInterval"]]) {
      aesCall <- list(
        x        = as.name("at"),
        ymin     = as.name("lCi"),
        ymax     = as.name("uCi")
      )
      geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = kmTable, fill = "grey60",  color = "grey60", alpha = 0.10)
      plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
    }

    aesCall <- list(
      x        = as.name("at"),
      y        = as.name("estimate")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = kmTable, color = "grey60")
    plot <- plot + do.call(jaspGraphs::geom_line, geomCall)

  }

  # add CI
  if (options[["predictionsConfidenceInterval"]]) {
    aesCall <- list(
      x        = as.name("at"),
      ymin     = as.name("lCi"),
      ymax     = as.name("uCi"),
      fill     = if (hasDistribution) as.name("Distribution") else if (hasLevel) as.name("Level"),
      linetype = if (hasDistribution && hasLevel) as.name("Level")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), alpha = 0.30)
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)

  }

  # add line
  aesCall <- list(
    x        = as.name("at"),
    y        = as.name("estimate"),
    color    = if (hasDistribution) as.name("Distribution") else if (hasLevel) as.name("Level"),
    linetype = if (hasDistribution && hasLevel) as.name("Level")
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
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    out[["estimate"]],
    if (options[["predictionsConfidenceInterval"]]) out[["lCi"]],
    if (options[["predictionsConfidenceInterval"]]) out[["uCi"]]), na.rm = TRUE))

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

  tempPlot <- createJaspPlot(width = if (hasDistribution || hasLevel) 550 else 400, height = 320)
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


.sapSurvivalTimePlotFun         <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "quantile")
  return(tempPlot)
}
.sapSurvivalProbabilityPlotFun  <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "survival")
  return(tempPlot)
}
.sapHazardPlotFun               <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "hazard")
  return(tempPlot)
}
.sapCumHazardPlotFun            <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "cumhaz")
  return(tempPlot)
}
.sapRmstPlotFun                 <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "rmst")
  return(tempPlot)
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
.sapOptions2PredictionTime      <- function(options, fit, type = "uknown", plot = FALSE) {

  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  minTime <- min(time[time > 0])
  maxTime <- max(time[time < Inf])

  # plotting preset which makes the plots look smoother than the generated tables
  if (plot) {
    options[["predictionsLifeTimeStepsNumber"]] <- 101
  }

  if (options[["predictionsLifeTimeStepsType"]] == "quantiles") {

    # special treatment for setting limits when survival plot with transformation is used
    if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
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

    # special treatment for setting limits when survival plot with transformation is used
    if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
      if (stepFrom == 0) {
        stepFrom <- minTime
      }
      if (plot) {
        setTime <- seq(stepFrom, stepTo, length.out = options[["predictionsLifeTimeStepsNumber"]])
      } else {
        setTime <- seq(stepFrom, stepTo, stepSize)
      }
    } else {
      if (plot) {
        if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
          setTime <- exp(seq(log(stepFrom), log(stepTo), length.out = options[["predictionsLifeTimeStepsNumber"]]))
        } else {
          setTime <- seq(stepFrom, stepTo, length.out = options[["predictionsLifeTimeStepsNumber"]])
        }
      } else {
        setTime <- seq(stepFrom, stepTo, stepSize)
        if (options[["predictionsLifeTimeRoundSteps"]])
          setTime <- unique(round(setTime))
      }
    }

  } else if (options[["predictionsLifeTimeStepsType"]] == "custom") {

    setTime <- options[["predictionsLifeTimeCustom"]]
    setTime <- .sapCleanCustomOptions(setTime, gettext("Custom steps for predicted survival time were specified in an incorrect format. Try '0.25, 0.50, 0.75'."))
    setTime <- sort(setTime)
    if (any(setTime < 0))
      .quitAnalysis(gettext("Custom steps for predicted survival time must be greater than or equal to 0."))

    # special treatment for setting limits when survival plot with transformation is used
    if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
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
