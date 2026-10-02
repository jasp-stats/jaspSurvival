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

.sapRun <- function(jaspResults, dataset, options) {

  if (.saSurvivalReady(options)) {
    dataset <- .saCheckDataset(dataset, options, type = "parametric")
    if (options[["analysisType"]] == "mixture") {
      .sapmCheckDataset(dataset, options)
    }
  }

  # Censoring summary table
  if (options[["censoringSummary"]])
    .saCensoringSummaryTable(jaspResults, dataset, options)

  # Fit the models
  .sapFit(jaspResults, dataset, options)

  # Statistics
  if (options[["modelSummary"]])
    .sapSummaryTable(jaspResults, options)
  if (options[["sequentialModelComparison"]])
    .sapSequentialModelComparisonTable(jaspResults, options)
  if (options[["coefficients"]])
    .sapCoefficientsTable(jaspResults, options)
  if (options[["coefficientsCovarianceMatrix"]])
    .sapCoefficientsCovarianceMatrixTable(jaspResults, options)


  # Predictions use the same output contract for every measure.
  measures <- c("survivalTime", "survivalProbability", "hazard", "cumulativeHazard", "restrictedMeanSurvivalTime")
  for (measure in measures) {
    if (options[[paste0(measure, "Table")]] && (measure == "survivalTime" || !options[["lifeTimeMergeTablesAcrossMeasures"]]))
      .sapPredictionOutput(jaspResults, options, measure)
  }
  if (options[["lifeTimeMergeTablesAcrossMeasures"]])
    .sapLifeTimeTable(jaspResults, options)
  for (measure in measures) {
    if (options[[paste0(measure, "Plot")]])
      .sapPredictionOutput(jaspResults, options, measure, plot = TRUE)
  }

  # Diagnostics
  .sapResidualPlots(jaspResults, options)
  if (options[["probabilityPlot"]])
    .sapProbabilityPlot(jaspResults, options)

  # Mixture
  if (options[["analysisType"]] == "mixture") {
    if (options[["mixtureComponentsTable"]])
      .sapmComponentsTable(jaspResults, options)
    if (options[["mixtureClassificationTable"]])
      .sapmClassificationTable(jaspResults, options)
    if (options[["mixtureDiagnosticsTable"]])
      .sapmDiagnosticsTable(jaspResults, options)
    if (options[["mixtureComponentPlot"]])
      .sapmComponentPlot(jaspResults, options)
  }

  .saExportColumns(jaspResults, options)

  return()
}

.sapDependencies <- c(
  "intervalStart", "intervalEnd", "timeToEvent", "eventStatus", "eventIndicator", "censoringType", "subgroup",
  "factors", "covariates", "weights", "subgroup", "distribution", "includeFullDatasetInSubgroupAnalysis",
  "selectedParametricDistributionExponential" ,"selectedParametricDistributionGamma" ,"selectedParametricDistributionGeneralizedF" ,
  "selectedParametricDistributionGeneralizedGamma" ,"selectedParametricDistributionGompertz" ,"selectedParametricDistributionLogLogistic" ,
  "selectedParametricDistributionLogNormal" ,"selectedParametricDistributionWeibull" ,"selectedParametricDistributionGeneralizedGammaOriginal" ,
  "selectedParametricDistributionGeneralizedFOriginal",
  "modelTerms",
  "includeFullDatasetInSubgroupAnalysis",
  # the CIs are not a simple multiplier of the standard error
  # as such, they need to be changed during the fitting process
  "coefficientsConfidenceIntervalLevel"
)
.sapGetDependencies <- function(options) {

  if (options[["analysisType"]] == "mixture")
    return(c(.sapDependencies, .sapmDependencies))

  return(.sapDependencies)
}
