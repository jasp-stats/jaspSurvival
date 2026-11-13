#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Parametric Survival Analysis
#'
#' This analysis performs a parametric survival analysis.
#'
#' @param censoringSummary, Create a summary table with information about the censoring status of the data.
#'    Defaults to \code{FALSE}.
#' @param censoringType, Specify whether the data are right-censored, interval-censored or include delayed entry.
#' \itemize{
#'   \item \code{"right"}
#'   \item \code{"counting"}
#'   \item \code{"interval"}: If interval censoring is selected, the following coding needs to be used: left-censored data is represented as (NA, t2), right-censored data as (t1, NA), exact data as (t, t), and interval-censored data as (t1, t2).
#' }
#' @param coefficients, Include a table with coefficient estimates.
#'    Defaults to \code{TRUE}.
#' @param coefficientsConfidenceIntervals, Include confidence intervals for the coefficients.
#'    Defaults to \code{TRUE}.
#' @param coefficientsConfidenceIntervalsLevel, Set the confidence level for the confidence intervals.
#' @param coefficientsCovarianceMatrix, Include a table with the covariance matrix of the coefficient estimates.
#'    Defaults to \code{FALSE}.
#' @param colorPalette, Colors for prediction series.
#' @param confidenceIntervalBounds, Choose two-sided confidence intervals or one-sided lower or upper bounds.
#' @param confidenceIntervalSimulationDraws, Set the number of simulations used for confidence intervals. More simulations improve precision but take longer.
#' @param covariates, Add continuous predictors to the survival model.
#' @param densityPlot, Compare the fitted distribution of survival times with the observed data.
#'    Defaults to \code{FALSE}.
#' @param densityPlotConfidenceIntervals, Show confidence intervals around the fitted density curve.
#'    Defaults to \code{FALSE}.
#' @param densityPlotConfidenceIntervalsLevel, Set the confidence level for the fitted density.
#' @param densityPlotMergePlotsAcrossDistributions, Overlay fitted density curves for the selected distributions.
#'    Defaults to \code{FALSE}.
#' @param densityPlotObservedData, Add a histogram of the observed survival times to the fitted density plot.
#'    Defaults to \code{TRUE}.
#' @param densityPlotObservedDataBinWidthType, Choose the histogram bin width rule. Select Manual to specify the number of bins.
#' @param densityPlotObservedDataNumberOfBins, Set the number of histogram bins.
#' @param densityPlotStratifyBy, Show separate groups of observed data for the selected predictors.
#'    Defaults to \code{TRUE}.
#' @param densityPlotStratifyByGroupUniqueLevels, Use individual values instead of quantile groups when the number of distinct values is within the specified limit.
#'    Defaults to \code{TRUE}.
#' @param densityPlotStratifyByQuantiles, Set the number of quantile groups for numeric predictors.
#' @param densityPlotStratifyByVariables, Choose the predictors used to group observations. Factors use their levels; numeric predictors use quantile groups.
#' @param densityPlotXAxisTransformation, Choose the transformation for the x-axis. Log shows the distribution of log survival times.
#' @param distribution, Choose a distribution, show all selected distributions, or select the best using AIC or BIC.
#' @param eventIndicator, Specify the value in the Event Status variable that indicates the occurrence of the event.
#' @param eventStatus, Choose the variable that indicates the event status, specifying whether each observation is an event or censored.
#' @param exportColumnPrefix, Set a prefix for the names of exported columns.
#' @param exportFittedValuesLinearPredictor, Export the fitted log relative risk, centered at mean covariate values within each stratum.
#'    Defaults to \code{FALSE}.
#' @param exportFittedValuesMeanSurvivalTime, Export the fitted mean survival time for each observation.
#'    Defaults to \code{FALSE}.
#' @param exportFittedValuesMedianSurvivalTime, Export the fitted median survival time for each observation.
#'    Defaults to \code{FALSE}.
#' @param exportFittedValuesRelativeRisk, Export the fitted relative risk, centered at mean covariate values within each stratum.
#'    Defaults to \code{FALSE}.
#' @param exportMixtureClassificationComponentIndex, Export the most likely mixture component for each observation.
#'    Defaults to \code{FALSE}.
#' @param exportMixtureClassificationComponentProbabilities, Export the estimated probability that each observation belongs to each mixture component.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsCoxSnell, Export Cox-Snell residuals. For counting-process data, these are conditional on survival to entry. Unavailable for interval-censored data.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsDeviance, Export deviance residuals from the fitted Cox model for each observation.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsMartingale, Export martingale residuals for each observation.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsResponse, Export observed minus predicted mean survival time, treating censored times as observed. Unavailable for interval-censored data.
#'    Defaults to \code{FALSE}.
#' @param factors, Add categorical predictors to the survival model.
#' @param fittedModelEquation, Show the fitted model equations in a table.
#'    Defaults to \code{FALSE}.
#' @param fittedModelEquationDecimals, Set the number of decimal places displayed in the fitted equations.
#' @param interpretModel, Select a specified model, show all models, or select the best using AIC or BIC.
#' @param intervalEnd, Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Interval or Counting.
#' @param intervalStart, Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Interval or Counting.
#' @param modelAdvancedSpecification, Specify each model independently, and set coefficient restrictions, covariate transformations or factor contrasts.
#'    Defaults to \code{FALSE}.
#' @param modelAdvancedSpecificationTerms, Set coefficient restrictions, covariate transformations and factor contrasts for each model.
#' @param modelAncillaryParameter1AdvancedSpecification, Set coefficient restrictions, transformations and factor contrasts for this parameter's models.
#'    Defaults to \code{FALSE}.
#' @param modelAncillaryParameter1AdvancedSpecificationTerms, Set coefficient restrictions, covariate transformations and factor contrasts for each model.
#' @param modelAncillaryParameter2AdvancedSpecification, Set coefficient restrictions, transformations and factor contrasts for this parameter's models.
#'    Defaults to \code{FALSE}.
#' @param modelAncillaryParameter2AdvancedSpecificationTerms, Set coefficient restrictions, covariate transformations and factor contrasts for each model.
#' @param modelAncillaryParameter3AdvancedSpecification, Set coefficient restrictions, transformations and factor contrasts for this parameter's models.
#'    Defaults to \code{FALSE}.
#' @param modelAncillaryParameter3AdvancedSpecificationTerms, Set coefficient restrictions, covariate transformations and factor contrasts for each model.
#' @param modelAncillaryParameterRegression, Use predictors to model additional distribution parameters, such as shape or scale.
#'    Defaults to \code{FALSE}.
#' @param modelSummary, Include a table with information about the model fit.
#'    Defaults to \code{TRUE}.
#' @param modelSummaryAicWeights, Include AIC weights in the model summary.
#'    Defaults to \code{FALSE}.
#' @param modelSummaryBicWeights, Include BIC weights in the model summary.
#'    Defaults to \code{FALSE}.
#' @param modelSummaryRankModels, Rank models based on the selected criterion.
#'    Defaults to \code{FALSE}.
#' @param outputFormattingAlwaysDisplayModelInformation, Always display model information (distribution and model name) in output tables.
#'    Defaults to \code{FALSE}.
#' @param outputFormattingCompareModelsAcrossDistributions, Compare models across distributions. This option is only available when the multiple models and parametric distributions are specified.
#'    Defaults to \code{TRUE}.
#' @param outputFormattingIncludeFullDatasetInSubgroupAnalysis, Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.
#'    Defaults to \code{FALSE}.
#' @param plotLegend, Choose the legend position for prediction plots.
#' @param plotTheme, Choose the theme for prediction plots.
#' @param predictionsAssignedCovariateValues, Choose quantile groups or custom values for numeric predictors used in separate lines or plots.
#' @param predictionsAssignedCovariateValuesCustomValues, Enter values for each numeric predictor used in separate lines or plots. Their combinations define the prediction groups.
#' @param predictionsAssignedCovariateValuesGroupUniqueLevels, Use individual values instead of quantile groups when the number of distinct values is within the specified limit.
#'    Defaults to \code{TRUE}.
#' @param predictionsAssignedCovariateValuesQuantiles, Set the number of quantile groups for numeric predictors.
#' @param predictionsContourSeparateLines, Draw separate contour lines for the levels or groups of the selected predictors.
#' @param predictionsContourSeparatePlots, Create separate contour plots for the levels or groups of the selected predictors.
#' @param predictionsContourXAxis, Select the numeric predictor for the horizontal contour axis.
#' @param predictionsContourYAxis, Select the numeric predictor for the vertical contour axis.
#' @param predictionsContoursCommonAcrossFigures, Use the same contour levels across figures for each selected measure.
#'    Defaults to \code{FALSE}.
#' @param predictionsContoursLevels, Choose automatic contour levels or enter custom levels for each selected measure.
#' @param predictionsContoursLevelsCumulativeHazard, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsContoursLevelsFailureProbability, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsContoursLevelsHazard, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsContoursLevelsNumber, Set the approximate number of rounded contour levels.
#' @param predictionsContoursLevelsRelativeSurvivalTime, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsContoursLevelsRestrictedMeanSurvivalTime, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsContoursLevelsSurvivalProbability, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsContoursLevelsSurvivalTime, Enter contour levels in the outcome's original units, separated by commas.
#' @param predictionsCovariateXAxisSettings, Set the axis transformation and plotting range for each numeric predictor on the x-axis.
#' @param predictionsCovariateXAxisSettingsXAxisFrom, Set the first predictor value to display, in original units. Leave blank to use the observed minimum.
#' @param predictionsCovariateXAxisSettingsXAxisTo, Set the last predictor value to display, in original units. Leave blank to use the observed maximum.
#' @param predictionsCovariateXAxisSettingsXAxisTransformation, Choose the axis transformation: the original scale, a logarithmic scale or the transformation used in the model.
#' @param predictionsCovariateXAxisSettingsYAxisFrom, Set the first predictor value to display, in original units. Leave blank to use the observed minimum.
#' @param predictionsCovariateXAxisSettingsYAxisTo, Set the last predictor value to display, in original units. Leave blank to use the observed maximum.
#' @param predictionsCovariateXAxisSettingsYAxisTransformation, Choose the axis transformation: the original scale, a logarithmic scale or the transformation used in the model.
#' @param predictionsCsvFile, Select a CSV file with one observation per row. Column names and factor levels must match the dataset.
#' @param predictionsDisplayCensoringEvents, Show censored observations as marks along the time axis.
#'    Defaults to \code{FALSE}.
#' @param predictionsDisplayCombinePredictorPlots, Combine the plots for the selected predictors in one figure.
#'    Defaults to \code{FALSE}.
#' @param predictionsDisplayConfidenceIntervals, Include confidence intervals for the figures and tables.
#'    Defaults to \code{TRUE}.
#' @param predictionsDisplayConfidenceIntervalsLevel, Set the confidence level for the confidence intervals.
#' @param predictionsDisplayConnectXAxisLevels, Connect predicted values across factor levels on the x-axis.
#'    Defaults to \code{TRUE}.
#' @param predictionsDisplayKaplanMeier, Overlay Kaplan-Meier curves for comparison with the model predictions, where available.
#'    Defaults to \code{FALSE}.
#' @param predictionsDisplayObservedData, Add observed event times to predictor plots, or observed predictor values to contour plots.
#'    Defaults to \code{FALSE}.
#' @param predictionsDisplayPredictionInterval, Show a range for individual survival times at the selected predictor values.
#'    Defaults to \code{FALSE}.
#' @param predictionsDisplayPredictionIntervalLevel, Set the coverage of the prediction interval.
#' @param predictionsHeatmapScaleCumulativeHazardFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleCumulativeHazardTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleCumulativeHazardTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsHeatmapScaleFailureProbabilityFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleFailureProbabilityTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleFailureProbabilityTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsHeatmapScaleHazardFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleHazardTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleHazardTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsHeatmapScaleRelativeSurvivalTimeFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleRelativeSurvivalTimeTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleRelativeSurvivalTimeTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsHeatmapScaleRestrictedMeanSurvivalTimeFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleRestrictedMeanSurvivalTimeTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleRestrictedMeanSurvivalTimeTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsHeatmapScaleSurvivalProbabilityFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleSurvivalProbabilityTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleSurvivalProbabilityTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsHeatmapScaleSurvivalTimeFrom, Set the lower colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleSurvivalTimeTo, Set the upper colour limit in the outcome's original units. Leave blank for an automatic limit.
#' @param predictionsHeatmapScaleSurvivalTimeTransformation, Choose the transformation for the heatmap colour scale.
#' @param predictionsLifetimeMean, Predict the expected survival time.
#'    Defaults to \code{FALSE}.
#' @param predictionsLifetimeMedian, Predict the median survival time (the 50th percentile).
#'    Defaults to \code{FALSE}.
#' @param predictionsLifetimePercentiles, Predict survival times at the selected percentiles.
#'    Defaults to \code{TRUE}.
#' @param predictionsLifetimePercentilesFrom, Set the first percentile in the sequence. Leave blank to start at 0.
#' @param predictionsLifetimePercentilesGrid, Choose how to specify lifetime percentiles: equal intervals, a sequence or custom values.
#' @param predictionsLifetimePercentilesIntervals, Set the number of equal intervals between 0 and 100.
#' @param predictionsLifetimePercentilesStep, Set the percentile increment. Leave blank to use an increment of 10.
#' @param predictionsLifetimePercentilesTo, Set the last percentile in the sequence. Leave blank to end at 100.
#' @param predictionsLifetimePercentilesValues, Enter lifetime percentiles from 0 to 100, separated by commas.
#' @param predictionsManualInput, Enter predictor values for one observation per row. Leave cells empty to use the sample mean or factor reference level.
#' @param predictionsMergeSeriesAcrossComponents, Overlay selected numbers of mixture components.
#'    Defaults to \code{FALSE}.
#' @param predictionsMergeSeriesAcrossDistributions, Overlay selected distributions.
#'    Defaults to \code{FALSE}.
#' @param predictionsMergeSeriesAcrossSubgroups, Overlay predictions for the separately fitted subgroups.
#'    Defaults to \code{FALSE}.
#' @param predictionsMergeTablesAcrossMeasures, Combine the selected time-based prediction measures in one table.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeCumulativeHazard, Predict the accumulated hazard up to each selected time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeFailureProbability, Predict the probability of the event occurring by each selected time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeHazard, Predict the event rate at each selected time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeRelativeSurvivalTime, Reference lifetime divided by current lifetime. Values above 1 indicate shorter survival.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeRestrictedMeanSurvivalTime, Predict the average survival time up to each selected time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeSurvivalProbability, Predict the probability of surviving beyond each selected time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeSurvivalTime, Predict lifetime percentiles, median or mean from the fitted model.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeTimeRatio, Current lifetime divided by reference lifetime. Values above 1 indicate longer survival.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutputExportPredictionsToDataset, Add the selected predictions as new columns in the dataset.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutputExportPredictionsToDatasetColumnPrefix, Set a prefix for the names of exported prediction columns.
#' @param predictionsOutputPlot, Plot every selected measure in its own panel. For new observations, use observation numbers on the x-axis and separate lines for times or lifetime percentiles.
#'    Defaults to \code{TRUE}.
#' @param predictionsOutputPlotType, Choose predictor plots for individual predictors, or contour plots for two numeric predictors.
#' @param predictionsOutputTable, Show a table of the selected predictions.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutputTableFormat, Show times, horizons and lifetime percentiles in rows (Long) or separate column groups (Wide).
#' @param predictionsPredictFor, Choose predictions for selected variable values, manually entered observations, dataset rows with missing outcomes, or observations from a CSV file.
#' @param predictionsPredictorValuesInTable, Set the number of numeric predictor values used in prediction tables.
#' @param predictionsProbabilityYAxisScale, Choose the y-axis scale for survival or failure probabilities.
#' @param predictionsRoundTimeTableValues, Round prediction times in tables to the nearest integer.
#'    Defaults to \code{TRUE}.
#' @param predictionsSeparateLines, Draw separate lines for predictor levels or groups, or for the selected times or percentiles.
#' @param predictionsSeparatePlots, Create separate plots for predictor levels or groups, or for the selected times or percentiles.
#' @param predictionsSurvivalTimeYAxisScale, Choose the y-axis scale for survival time, time ratios or acceleration factors.
#' @param predictionsTimeFrom, Set the first time in the sequence. Leave blank to start at 0.
#' @param predictionsTimeGrid, Choose how to specify prediction times: equal intervals, a sequence or custom values.
#' @param predictionsTimeIntervals, Set the number of equal intervals between zero and the maximum follow-up time.
#' @param predictionsTimePercentages, Enter prediction times as percentages of the maximum follow-up time, separated by commas.
#' @param predictionsTimeStep, Set the time increment. Leave blank to use the default increment.
#' @param predictionsTimeTo, Set the last time in the sequence. Leave blank to use the maximum follow-up time.
#' @param predictionsTimeValues, Enter non-negative prediction times, separated by commas. Leave blank to use the default times.
#' @param predictionsUnassignedReferenceValues, Choose a design average, a population average, default reference values or custom values.
#' @param predictionsUnassignedReferenceValuesCustomValues, Enter reference values for unassigned predictors. Leave cells empty to use design averages.
#' @param predictionsXAxisTransformation, Choose the x-axis scale for lifetime percentiles or time.
#' @param predictionsXAxisVariables, Select predictors for the x-axis, or choose Lifetime / Time to plot predictions over percentiles or time.
#' @param probabilityPlot, Compare observed failure times with the fitted distribution to assess model fit. Available for right-censored data.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotCanvas, Choose the reference distribution for the probability plot's axes.
#' @param probabilityPlotCensoringEvents, Show censored observations as rug marks at the bottom of the plot. Only available when Censoring Type is set to Right.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotColorPalette, Choose the colours used in the probability plot.
#' @param probabilityPlotConfidenceIntervals, Include confidence interval bounds for the fitted probability curve.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotConfidenceIntervalsLevel, Set the confidence level for the probability plot confidence intervals.
#' @param probabilityPlotEmpiricalPoints, Show points representing the observed failure probabilities.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotEmpiricalPointsCoordinates, Display the time and failure probability next to each empirical point.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotFittedCurve, Plot the fitted curve from the selected parametric model.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotGrid, Display probability-paper grid lines.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotLegend, Choose the legend position for the probability plot.
#' @param probabilityPlotMergePlotsAcrossDistributions, Merge the probability plots across distributions into a single plot. Only available when no model selection is being performed.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotMergePlotsAcrossStrata, Overlay the observed groups in one probability plot, or show each group in a separate plot.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotMergePlotsAcrossSubgroups, Overlay probability plots for separately fitted subgroups.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotPlottingPosition, Select the method used to compute empirical failure probability plotting positions.
#' @param probabilityPlotRankAdjustment, Choose how to adjust the ranks of observations when some survival times are censored.
#' @param probabilityPlotStratifyBy, Show separate groups of observed data for the selected predictors.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotStratifyByGroupUniqueLevels, Use individual values instead of quantile groups when the number of distinct values is within the specified limit.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotStratifyByQuantiles, Set the number of quantile groups for numeric predictors.
#' @param probabilityPlotStratifyByVariables, Choose the predictors used to group observations. Factors use their levels; numeric predictors use quantile groups.
#' @param probabilityPlotTheme, Select the theme for the probability plot's appearance.
#' @param probabilityPlotTies, Select how tied failure times are handled when computing empirical plotting positions.
#' @param residualPlotResidualHistogram, Display a histogram of residuals to assess their distribution.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualType, Select the type of residuals to plot
#' @param residualPlotResidualVsPredicted, Plot residuals versus predicted mean survival times.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualVsPredictors, Plot residuals versus predictors to assess model fit. Available when model terms are specified.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualVsTime, Plot residuals versus time. Only available when Censoring Type is set to Right.
#'    Defaults to \code{FALSE}.
#' @param restrictParameters, Fix baseline distribution parameters at specified values. Leave a field empty to estimate the parameter.
#'    Defaults to \code{FALSE}.
#' @param sequentialModelComparison, Include a table with the results of the sequential model comparison.
#'    Defaults to \code{FALSE}.
#' @param subgroup, Fit separate models for each combination of the selected factors' levels.
#' @param timeToEvent, Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right.
#' @param weights, Select a variable of positive integer case frequencies. Each value gives the number of observations represented by that row.
ParametricSurvivalAnalysis <- function(
          data = NULL,
          version = "1",
          censoringSummary = FALSE,
          censoringType = "right",
          coefficients = TRUE,
          coefficientsConfidenceIntervals = TRUE,
          coefficientsConfidenceIntervalsLevel = 0.95,
          coefficientsCovarianceMatrix = FALSE,
          colorPalette = "colorblind",
          confidenceIntervalBounds = "twoSided",
          confidenceIntervalSimulationDraws = 10000,
          covariates = list(types = list(), value = list()),
          densityPlot = FALSE,
          densityPlotConfidenceIntervals = FALSE,
          densityPlotConfidenceIntervalsLevel = 0.95,
          densityPlotMergePlotsAcrossDistributions = FALSE,
          densityPlotObservedData = TRUE,
          densityPlotObservedDataBinWidthType = "fd",
          densityPlotObservedDataNumberOfBins = 30,
          densityPlotStratifyBy = TRUE,
          densityPlotStratifyByGroupUniqueLevels = TRUE,
          densityPlotStratifyByGroupUniqueLevelsLimit = 5,
          densityPlotStratifyByQuantiles = 3,
          densityPlotStratifyByVariables = list(),
          densityPlotXAxisTransformation = "log",
          distribution = "weibull",
          eventIndicator = "",
          eventStatus = list(types = list(), value = ""),
          exportColumnPrefix = "",
          exportFittedValuesLinearPredictor = FALSE,
          exportFittedValuesMeanSurvivalTime = FALSE,
          exportFittedValuesMedianSurvivalTime = FALSE,
          exportFittedValuesRelativeRisk = FALSE,
          exportMixtureClassificationComponentIndex = FALSE,
          exportMixtureClassificationComponentProbabilities = FALSE,
          exportResidualsCoxSnell = FALSE,
          exportResidualsDeviance = FALSE,
          exportResidualsMartingale = FALSE,
          exportResidualsResponse = FALSE,
          factors = list(types = list(), value = list()),
          fittedModelEquation = FALSE,
          fittedModelEquationDecimals = 3,
          interpretModel = "all",
          intervalEnd = list(types = list(), value = ""),
          intervalStart = list(types = list(), value = ""),
          modelAdvancedSpecification = FALSE,
          modelAdvancedSpecificationTerms = list(),
          modelAncillaryParameter1AdvancedSpecification = FALSE,
          modelAncillaryParameter1AdvancedSpecificationTerms = list(),
          modelAncillaryParameter1ModelTerms = list(list(components = list(), name = "ancillary1model1", title = "Model 1")),
          modelAncillaryParameter2AdvancedSpecification = FALSE,
          modelAncillaryParameter2AdvancedSpecificationTerms = list(),
          modelAncillaryParameter2ModelTerms = list(list(components = list(), name = "ancillary2model1", title = "Model 1")),
          modelAncillaryParameter3AdvancedSpecification = FALSE,
          modelAncillaryParameter3AdvancedSpecificationTerms = list(),
          modelAncillaryParameter3ModelTerms = list(list(components = list(), name = "ancillary3model1", title = "Model 1")),
          modelAncillaryParameterRegression = FALSE,
          modelSummary = TRUE,
          modelSummaryAicWeights = FALSE,
          modelSummaryBicWeights = FALSE,
          modelSummaryRankModels = FALSE,
          modelSummaryRankModelsBy = "aic",
          modelTerms = list(list(components = list(), name = "model1", title = "Model 1")),
          outputFormattingAlwaysDisplayModelInformation = FALSE,
          outputFormattingCompareModelsAcrossDistributions = TRUE,
          outputFormattingIncludeFullDatasetInSubgroupAnalysis = FALSE,
          plotHeight = 320,
          plotLegend = "right",
          plotTheme = "jasp",
          plotWidth = 480,
          predictionsAssignedCovariateValues = "quantiles",
          predictionsAssignedCovariateValuesCustomValues = list(list(levels = "Row 0", name = "data 1", values = "")),
          predictionsAssignedCovariateValuesGroupUniqueLevels = TRUE,
          predictionsAssignedCovariateValuesGroupUniqueLevelsLimit = 5,
          predictionsAssignedCovariateValuesQuantiles = 3,
          predictionsContourSeparateLines = list(types = list(), value = list()),
          predictionsContourSeparatePlots = list(types = list(), value = list()),
          predictionsContourXAxis = list(types = list(), value = ""),
          predictionsContourYAxis = list(types = list(), value = ""),
          predictionsContoursCommonAcrossFigures = FALSE,
          predictionsContoursLevels = "number",
          predictionsContoursLevelsCumulativeHazard = "",
          predictionsContoursLevelsFailureProbability = "",
          predictionsContoursLevelsHazard = "",
          predictionsContoursLevelsNumber = 5,
          predictionsContoursLevelsRelativeSurvivalTime = "",
          predictionsContoursLevelsRestrictedMeanSurvivalTime = "",
          predictionsContoursLevelsSurvivalProbability = "",
          predictionsContoursLevelsSurvivalTime = "",
          predictionsCovariateXAxisSettings = list(),
          predictionsCovariateXAxisSettingsXAxisFrom = "",
          predictionsCovariateXAxisSettingsXAxisTo = "",
          predictionsCovariateXAxisSettingsXAxisTransformation = "none",
          predictionsCovariateXAxisSettingsYAxisFrom = "",
          predictionsCovariateXAxisSettingsYAxisTo = "",
          predictionsCovariateXAxisSettingsYAxisTransformation = "none",
          predictionsCsvFile = "",
          predictionsDisplayCensoringEvents = FALSE,
          predictionsDisplayCombinePredictorPlots = FALSE,
          predictionsDisplayConfidenceIntervals = TRUE,
          predictionsDisplayConfidenceIntervalsLevel = 0.95,
          predictionsDisplayConnectXAxisLevels = TRUE,
          predictionsDisplayKaplanMeier = FALSE,
          predictionsDisplayObservedData = FALSE,
          predictionsDisplayPredictionInterval = FALSE,
          predictionsDisplayPredictionIntervalLevel = 0.95,
          predictionsHeatmapScaleCumulativeHazardFrom = "",
          predictionsHeatmapScaleCumulativeHazardTo = "",
          predictionsHeatmapScaleCumulativeHazardTransformation = "none",
          predictionsHeatmapScaleFailureProbabilityFrom = "",
          predictionsHeatmapScaleFailureProbabilityTo = "",
          predictionsHeatmapScaleFailureProbabilityTransformation = "none",
          predictionsHeatmapScaleHazardFrom = "",
          predictionsHeatmapScaleHazardTo = "",
          predictionsHeatmapScaleHazardTransformation = "none",
          predictionsHeatmapScaleRelativeSurvivalTimeFrom = "",
          predictionsHeatmapScaleRelativeSurvivalTimeTo = "",
          predictionsHeatmapScaleRelativeSurvivalTimeTransformation = "none",
          predictionsHeatmapScaleRestrictedMeanSurvivalTimeFrom = "",
          predictionsHeatmapScaleRestrictedMeanSurvivalTimeTo = "",
          predictionsHeatmapScaleRestrictedMeanSurvivalTimeTransformation = "none",
          predictionsHeatmapScaleSurvivalProbabilityFrom = "",
          predictionsHeatmapScaleSurvivalProbabilityTo = "",
          predictionsHeatmapScaleSurvivalProbabilityTransformation = "none",
          predictionsHeatmapScaleSurvivalTimeFrom = "",
          predictionsHeatmapScaleSurvivalTimeTo = "",
          predictionsHeatmapScaleSurvivalTimeTransformation = "none",
          predictionsLifetimeMean = FALSE,
          predictionsLifetimeMedian = FALSE,
          predictionsLifetimePercentiles = TRUE,
          predictionsLifetimePercentilesFrom = "",
          predictionsLifetimePercentilesGrid = "quantiles",
          predictionsLifetimePercentilesIntervals = 10,
          predictionsLifetimePercentilesStep = "",
          predictionsLifetimePercentilesTo = "",
          predictionsLifetimePercentilesValues = "25, 50, 75",
          predictionsLifetimeTimeId = "Lifetime / Time",
          predictionsManualInput = list(list(levels = "Row 0", name = "data 1", values = "")),
          predictionsMergeSeriesAcrossComponents = FALSE,
          predictionsMergeSeriesAcrossDistributions = FALSE,
          predictionsMergeSeriesAcrossSubgroups = FALSE,
          predictionsMergeTablesAcrossMeasures = FALSE,
          predictionsOutcomeCumulativeHazard = FALSE,
          predictionsOutcomeFailureProbability = FALSE,
          predictionsOutcomeHazard = FALSE,
          predictionsOutcomeRelativeSurvivalTime = FALSE,
          predictionsOutcomeRestrictedMeanSurvivalTime = FALSE,
          predictionsOutcomeSurvivalProbability = FALSE,
          predictionsOutcomeSurvivalTime = FALSE,
          predictionsOutcomeTimeRatio = FALSE,
          predictionsOutputExportPredictionsToDataset = FALSE,
          predictionsOutputExportPredictionsToDatasetColumnPrefix = "Predicted",
          predictionsOutputPlot = TRUE,
          predictionsOutputPlotType = "standard",
          predictionsOutputTable = FALSE,
          predictionsOutputTableFormat = "wide",
          predictionsPredictFor = "variables",
          predictionsPredictorValuesInTable = 10,
          predictionsProbabilityYAxisScale = "none",
          predictionsRolesAutomatic = TRUE,
          predictionsRoundTimeTableValues = TRUE,
          predictionsSeparateLines = list(types = list(), value = list()),
          predictionsSeparatePlots = list(types = list(), value = list()),
          predictionsSurvivalTimeYAxisScale = "none",
          predictionsTimeFrom = "",
          predictionsTimeGrid = "quantiles",
          predictionsTimeIntervals = 10,
          predictionsTimePercentages = "25, 50, 75",
          predictionsTimeStep = "",
          predictionsTimeTo = "",
          predictionsTimeValues = 0,
          predictionsUnassignedReferenceValues = "average",
          predictionsUnassignedReferenceValuesCustomValues = list(list(levels = "Row 0", name = "data 1", values = "")),
          predictionsXAxisTransformation = "none",
          predictionsXAxisVariables = list(types = list(), value = list()),
          probabilityPlot = FALSE,
          probabilityPlotCanvas = "weibull",
          probabilityPlotCensoringEvents = FALSE,
          probabilityPlotColorPalette = "colorblind",
          probabilityPlotConfidenceIntervals = TRUE,
          probabilityPlotConfidenceIntervalsLevel = 0.9,
          probabilityPlotEmpiricalPoints = TRUE,
          probabilityPlotEmpiricalPointsCoordinates = FALSE,
          probabilityPlotFittedCurve = TRUE,
          probabilityPlotGrid = TRUE,
          probabilityPlotLegend = "right",
          probabilityPlotMergePlotsAcrossDistributions = FALSE,
          probabilityPlotMergePlotsAcrossStrata = TRUE,
          probabilityPlotMergePlotsAcrossSubgroups = FALSE,
          probabilityPlotPlottingPosition = "median",
          probabilityPlotRankAdjustment = "johnson",
          probabilityPlotStratifyBy = TRUE,
          probabilityPlotStratifyByGroupUniqueLevels = TRUE,
          probabilityPlotStratifyByGroupUniqueLevelsLimit = 5,
          probabilityPlotStratifyByQuantiles = 3,
          probabilityPlotStratifyByVariables = list(),
          probabilityPlotTheme = "jasp",
          probabilityPlotTies = "none",
          residualPlotResidualHistogram = FALSE,
          residualPlotResidualType = "response",
          residualPlotResidualVsPredicted = FALSE,
          residualPlotResidualVsPredictors = FALSE,
          residualPlotResidualVsTime = FALSE,
          restrictParameters = FALSE,
          restrictParametersValues = list(list(restriction = "exponential:1"), list(restriction = "exponential:2"), list(restriction = "exponential:3"), list(restriction = "exponential:4"), list(restriction = "gamma:1"), list(restriction = "gamma:2"), list(restriction = "gamma:3"), list(restriction = "gamma:4"), list(restriction = "generalizedF:1"), list(restriction = "generalizedF:2"), list(restriction = "generalizedF:3"), list(restriction = "generalizedF:4"), list(restriction = "generalizedGamma:1"), list(restriction = "generalizedGamma:2"), list(restriction = "generalizedGamma:3"), list(restriction = "generalizedGamma:4"), list(restriction = "gompertz:1"), list(restriction = "gompertz:2"), list(restriction = "gompertz:3"), list(restriction = "gompertz:4"), list(restriction = "logLogistic:1"), list(restriction = "logLogistic:2"), list(restriction = "logLogistic:3"), list(restriction = "logLogistic:4"), list(restriction = "logNormal:1"), list(restriction = "logNormal:2"), list(restriction = "logNormal:3"), list(restriction = "logNormal:4"), list(restriction = "weibull:1"), list(restriction = "weibull:2"), list(restriction = "weibull:3"), list(restriction = "weibull:4"), list(restriction = "generalizedGammaOriginal:1"), list(restriction = "generalizedGammaOriginal:2"), list(restriction = "generalizedGammaOriginal:3"), list(restriction = "generalizedGammaOriginal:4"), list(restriction = "generalizedFOriginal:1"), list(restriction = "generalizedFOriginal:2"), list(restriction = "generalizedFOriginal:3"), list(restriction = "generalizedFOriginal:4")),
          seed = 1,
          selectedParametricDistributionsExponential = TRUE,
          selectedParametricDistributionsGamma = TRUE,
          selectedParametricDistributionsGeneralizedF = TRUE,
          selectedParametricDistributionsGeneralizedFOriginal = FALSE,
          selectedParametricDistributionsGeneralizedGamma = TRUE,
          selectedParametricDistributionsGeneralizedGammaOriginal = FALSE,
          selectedParametricDistributionsGompertz = TRUE,
          selectedParametricDistributionsLogLogistic = TRUE,
          selectedParametricDistributionsLogNormal = TRUE,
          selectedParametricDistributionsWeibull = TRUE,
          sequentialModelComparison = FALSE,
          setSeed = FALSE,
          subgroup = list(types = list(), value = list()),
          timeToEvent = list(types = list(), value = ""),
          weights = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorPalette", "confidenceIntervalBounds", "covariates", "densityPlotObservedDataBinWidthType", "densityPlotStratifyByVariables", "densityPlotXAxisTransformation", "distribution", "eventIndicator", "eventStatus", "factors", "interpretModel", "intervalEnd", "intervalStart", "modelAdvancedSpecificationTerms", "modelAncillaryParameter1AdvancedSpecificationTerms", "modelAncillaryParameter1ModelTerms", "modelAncillaryParameter2AdvancedSpecificationTerms", "modelAncillaryParameter2ModelTerms", "modelAncillaryParameter3AdvancedSpecificationTerms", "modelAncillaryParameter3ModelTerms", "modelTerms", "plotLegend", "plotTheme", "predictionsAssignedCovariateValues", "predictionsAssignedCovariateValuesCustomValues", "predictionsContourSeparateLines", "predictionsContourSeparatePlots", "predictionsContourXAxis", "predictionsContourYAxis", "predictionsContoursLevels", "predictionsCovariateXAxisSettings", "predictionsCovariateXAxisSettingsXAxisTransformation", "predictionsCovariateXAxisSettingsYAxisTransformation", "predictionsHeatmapScaleCumulativeHazardTransformation", "predictionsHeatmapScaleFailureProbabilityTransformation", "predictionsHeatmapScaleHazardTransformation", "predictionsHeatmapScaleRelativeSurvivalTimeTransformation", "predictionsHeatmapScaleRestrictedMeanSurvivalTimeTransformation", "predictionsHeatmapScaleSurvivalProbabilityTransformation", "predictionsHeatmapScaleSurvivalTimeTransformation", "predictionsLifetimePercentilesGrid", "predictionsManualInput", "predictionsOutputPlotType", "predictionsOutputTableFormat", "predictionsPredictFor", "predictionsProbabilityYAxisScale", "predictionsSeparateLines", "predictionsSeparatePlots", "predictionsSurvivalTimeYAxisScale", "predictionsTimeGrid", "predictionsUnassignedReferenceValues", "predictionsUnassignedReferenceValuesCustomValues", "predictionsXAxisTransformation", "predictionsXAxisVariables", "probabilityPlotCanvas", "probabilityPlotColorPalette", "probabilityPlotLegend", "probabilityPlotPlottingPosition", "probabilityPlotRankAdjustment", "probabilityPlotStratifyByVariables", "probabilityPlotTheme", "probabilityPlotTies", "residualPlotResidualType", "restrictParametersValues", "subgroup", "timeToEvent", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspSurvival", "ParametricSurvivalAnalysis", "ParametricSurvivalAnalysis.qml", options, version, TRUE))
}