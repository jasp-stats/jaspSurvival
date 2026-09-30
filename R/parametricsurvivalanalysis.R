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

  if (.saSurvivalReady(options))
    dataset <- .saCheckDataset(dataset, options, type = "parametric")

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


  # Prediction Tables
  if (options[["survivalTimeTable"]])
    .sapSurvivalTimeTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["survivalProbabilityTable"]])
    .sapSurvivalProbabilityTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["hazardTable"]])
    .sapHazardTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["cumulativeHazardTable"]])
    .sapCumHazardTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["restrictedMeanSurvivalTimeTable"]])
    .sapRmstTable(jaspResults, options)
  if (options[["lifeTimeMergeTablesAcrossMeasures"]])
    .sapLifeTimeTable(jaspResults, options)

  # Prediction Plots
  if (options[["survivalTimePlot"]])
    .sapSurvivalTimePlot(jaspResults, options)
  if (options[["survivalProbabilityPlot"]])
    .sapSurvivalProbabilityPlot(jaspResults, options)
  if (options[["hazardPlot"]])
    .sapHazardPlot(jaspResults, options)
  if (options[["cumulativeHazardPlot"]])
    .sapCumHazardPlot(jaspResults, options)
  if (options[["restrictedMeanSurvivalTimePlot"]])
    .sapRmstPlot(jaspResults, options)

  # Diagnostics
  if (options[["residualPlotResidualVsTime"]])
    .sapResidualsVsTimePlot(jaspResults, options)
  if (options[["residualPlotResidualVsPredictors"]])
    .sapResidualsVsPredictorsPlot(jaspResults, options)
  if (options[["residualPlotResidualVsPredicted"]])
    .sapResidualsVsPredictedPlot(jaspResults, options)
  if (options[["residualPlotResidualHistogram"]])
    .sapResidualHistogramPlot(jaspResults, options)
  if (isTRUE(options[["probabilityPlot"]]))
    .sapProbabilityPlot(jaspResults, options)

  return()
}

.sapDependencies <- c(
  "intervalStart", "intervalEnd", "timeToEvent", "eventStatus", "eventIndicator", "censoringType", "subgroup",
  "factors", "covariates", "weights", "subgroup", "distribution", "includeFullDatasetInSubgroupAnalysis",
  "selectedParametricDistributionExponential" ,"selectedParametricDistributionGamma" ,"selectedParametricDistributionGeneralizedF" ,
  "selectedParametricDistributionGeneralizedGamma" ,"selectedParametricDistributionGompertz" ,"selectedParametricDistributionLogLogistic" ,
  "selectedParametricDistributionLogNormal" ,"selectedParametricDistributionWeibull" ,"selectedParametricDistributionGeneralizedGammaOriginal" ,
  "selectedParametricDistributionGeneralizedFOriginal",
  "modelTerms", "includeIntercept",
  "includeFullDatasetInSubgroupAnalysis",
  # the CIs are not a simple multiplier of the standard error
  # as such, they need to be changed during the fitting process
  "coefficientsConfidenceIntervalLevel"
)
