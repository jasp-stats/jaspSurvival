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

ParametricSurvivalAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  options[["analysisType"]] <- "parametric"
  .sapRun(jaspResults, dataset, options)

  return()
}

ParametricMixtureSurvivalAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  options[["analysisType"]] <- "mixture"
  .sapRun(jaspResults, dataset, options)

  return()
}

# Shared entry point: validate, update retained fits, then build requested outputs.
.sapRun <- function(jaspResults, dataset, options) {

  # Prepare the option contract before validation and dependency comparisons.
  options <- .sapPrepareModelTerms(options)
  .sapValidateModelPredictionEvaluations(options)
  # Preserve rows omitted from fitting for missing-outcome predictions and exports.
  predictionDataset <- dataset

  if (.saSurvivalReady(options)) {
    dataset <- .saCheckDataset(dataset, options, type = "parametric")
    if (options[["analysisType"]] == "mixture") {
      .sapmCheckDataset(dataset, options)
    }
    .sapValidateParameterRestrictions(dataset, options)
    .sapValidatePredictionReferences(dataset, options)
  }

  # Fit the models

  # Changes to retained inference require rebuilding outputs that depend on its bounds.
  informationChanged <- .sapFit(jaspResults, dataset, options)
  if (informationChanged) {
    for (key in c("fitErrors", "censoringSummaryTable", "summaryTable", "sequentialModelComparisonTable", "coefficientsTable", "coefficientsCovarianceMatrixTable", "modelEquations",
      "survivalTimeTable", "survivalTimePlot", "survivalProbabilityTable", "survivalProbabilityPlot",
      "failureProbabilityTable", "failureProbabilityPlot",
      "relativeSurvivalTimeTable", "relativeSurvivalTimePlot",
      "timeRatioTable", "timeRatioPlot", "timeRatioMeanSurvivalTimeTable", "timeRatioMeanSurvivalTimePlot", "timeRatioMedianSurvivalTimeTable", "timeRatioMedianSurvivalTimePlot",
      "meanSurvivalTimeTable", "meanSurvivalTimePlot", "medianSurvivalTimeTable", "medianSurvivalTimePlot",
      "relativeMeanSurvivalTimeTable", "relativeMeanSurvivalTimePlot", "relativeMedianSurvivalTimeTable", "relativeMedianSurvivalTimePlot",
      "hazardTable", "hazardPlot", "cumHazardTable", "cumulativeHazardPlot", "rmstTable",
      "restrictedMeanSurvivalTimePlot", "lifeTimeTable", "probabilityPlot", "densityPlot", "mixtureComponentPlot", "customPredictionsResult", "kaplanMeierAvailability",
      "residualsVsTimePlot", "residualsVsPredictorsPlot", "residualVsPredictedPlot", "residualHistogram",
      "mixtureComponentsTable", "mixtureClassificationTable", "mixtureDiagnosticsTable"))
      jaspResults[[key]] <- NULL
  }
  if (options[["censoringSummary"]])
    .saCensoringSummaryTable(jaspResults, dataset, options)

  # Share draws across all outputs, then release transient storage on exit.
  options[["simulationContext"]] <- .sapSimulationContext(jaspResults, options)
  on.exit(.sapReleaseSimulationContext(options[["simulationContext"]]), add = TRUE)
  .sapFitErrorsTable(jaspResults, options)
  .sapKaplanMeierAvailability(jaspResults, options)

  # Statistics
  if (options[["modelSummary"]])
    .sapSummaryTable(jaspResults, options)
  if (options[["sequentialModelComparison"]])
    .sapSequentialModelComparisonTable(jaspResults, options)
  if (options[["coefficients"]])
    .sapCoefficientsTable(jaspResults, options)
  if (options[["coefficientsCovarianceMatrix"]])
    .sapCoefficientsCovarianceMatrixTable(jaspResults, options)
  if (options[["fittedModelEquation"]])
    .sapModelEquations(jaspResults, options)


  # Predictions
  .sapModelPredictions(jaspResults, options)
  customExports <- .sapCustomPredictions(jaspResults, predictionDataset, dataset, options)

  # Diagnostics
  .sapResidualPlots(jaspResults, options)
  if (options[["densityPlot"]])
    .sapDensityPlot(jaspResults, options)
  if (options[["probabilityPlot"]])
    .sapProbabilityPlot(jaspResults, options)

  # Mixture
  if (options[["analysisType"]] == "mixture") {
    if (options[["mixtureMeanAndMedian"]])
      .sapmComponentsTable(jaspResults, options)
    if (options[["mixtureClassification"]])
      .sapmClassificationTable(jaspResults, options)
    if (options[["mixtureEstimationDiagnostics"]])
      .sapmDiagnosticsTable(jaspResults, options)
    if (options[["mixtureComponentPlot"]])
      .sapmComponentPlot(jaspResults, options)
  }

  .saExportColumns(jaspResults, options, additional = customExports, informationChanged = informationChanged)

  return()
}

.sapDistributions <- c(
  exponential              = "exp", gamma = "gamma", generalizedF = "genf", generalizedGamma = "gengamma",
  gompertz                 = "gompertz", logLogistic = "llogis", logNormal = "lnorm", weibull = "weibull",
  generalizedGammaOriginal = "gengamma.orig", generalizedFOriginal = "genf.orig"
)
.sapDistributionOptions <- paste0("selectedParametricDistributions",
  toupper(substring(names(.sapDistributions), 1, 1)), substring(names(.sapDistributions), 2))

.sapDependencies <- c(
  "intervalStart", "intervalEnd", "timeToEvent", "eventStatus", "eventIndicator", "censoringType",
  "factors", "covariates", "weights", "subgroup", "distribution", "outputFormattingIncludeFullDatasetInSubgroupAnalysis",
  "restrictParameters", "restrictParametersValues",
  .sapDistributionOptions, "modelTerms", "modelAdvancedSpecification", "modelAdvancedSpecificationTerms",
  "modelAncillaryParameterRegression", paste0("modelAncillaryParameter", 1:3, "ModelTerms"),
  paste0("modelAncillaryParameter", 1:3, "AdvancedSpecification"), paste0("modelAncillaryParameter", 1:3, "AdvancedSpecificationTerms")
)
.sapFitExcludedDependencies <- c(
  "modelTerms", "modelAdvancedSpecificationTerms", "outputFormattingIncludeFullDatasetInSubgroupAnalysis", "mixtureComponents", "mixtureMaximumComponents",
  "outputFormattingCompareModelsAcrossComponents", paste0("modelAncillaryParameter", 1:3, "ModelTerms"), paste0("modelAncillaryParameter", 1:3, "AdvancedSpecificationTerms")
)
.sapOutputDependencies         <- c("outputFormattingCompareModelsAcrossDistributions", "outputFormattingAlwaysDisplayModelInformation")
.sapSelectedOutputDependencies <- c(.sapOutputDependencies, "interpretModel")
.sapSimulationDependencies     <- c("confidenceIntervalSimulationDraws", "setSeed", "seed")
.sapPredictionCiDependencies   <- c("predictionsDisplayConfidenceIntervals", "predictionsDisplayConfidenceIntervalsLevel", "confidenceIntervalBounds", .sapSimulationDependencies)
.sapPredictionTypes <- c(
  survivalTime       = "quantile", timeRatio = "quantile", relativeSurvivalTime = "quantile", survivalProbability = "survival",
  failureProbability = "survival", hazard = "hazard", cumulativeHazard = "cumhaz", restrictedMeanSurvivalTime = "rmst"
)
.sapModelPredictionOptions <- paste0("predictionsOutcome",
  toupper(substring(names(.sapPredictionTypes), 1L, 1L)), substring(names(.sapPredictionTypes), 2L))
.sapModelPredictionTimeOptions <- setdiff(.sapModelPredictionOptions, paste0("predictionsOutcome", c("SurvivalTime", "TimeRatio", "RelativeSurvivalTime")))
.sapLifetimeSummaryOptions <- paste0("predictionsLifetime", c("Percentiles", "Median", "Mean"))
.sapPredictionQuantileDependencies <- paste0("predictionsLifetimePercentiles", c("Grid", "Intervals", "From", "Step", "To", "Values"))
.sapPredictionTimeDependencies <- c(paste0("predictionsTime", c("Grid", "Intervals", "From", "Step", "To", "Values", "Percentages")),
  "predictionsRoundTimeTableValues")
.sapProbabilityCiDependencies  <- c("probabilityPlotConfidenceIntervals", "probabilityPlotConfidenceIntervalsLevel", "confidenceIntervalBounds", .sapSimulationDependencies)
.sapGetDependencies <- function(options) {

  if (options[["analysisType"]] == "mixture")
    return(c(.sapDependencies, .sapmDependencies))

  return(.sapDependencies)
}
