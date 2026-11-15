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

#' Recurrent Event Analysis
#'
#' Model recurrent events from event counts in successive observation intervals. Fit each independent test sequence separately using Subgroup.
#'
#' @param coefficients, Include a table with coefficient estimates.
#'    Defaults to \code{TRUE}.
#' @param coefficientsConfidenceIntervals, Include confidence intervals for the coefficients.
#'    Defaults to \code{TRUE}.
#' @param coefficientsConfidenceIntervalsLevel, Set the confidence level for the confidence intervals.
#' @param coefficientsCovarianceMatrix, Include a table with the covariance matrix of the coefficient estimates.
#'    Defaults to \code{FALSE}.
#' @param colorPalette, Colors for prediction series.
#' @param confidenceIntervalBounds, Choose two-sided confidence intervals or one-sided lower or upper bounds.
#' @param dataSummary, Show observation intervals, total events, follow-up time and observed cumulative mean time between events for each subgroup.
#'    Defaults to \code{FALSE}.
#' @param estimationMethod, Maximum likelihood uses counts in observation intervals. Least squares fits a regression on logarithms of cumulative quantities.
#' @param eventCount, Select the number of events within each interval, not cumulative counts. Leave empty for one event per interval. ReliaGrowR currently requires positive counts in every interval.
#' @param exportColumnPrefix, Set a non-empty prefix for exported column names.
#' @param exportFittedValuesCumulativeEvents, Export fitted cumulative events at each interval endpoint.
#'    Defaults to \code{FALSE}.
#' @param exportFittedValuesIntervalEvents, Export expected events within each observation interval, obtained by differencing the fitted cumulative mean.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsDeviance, Export signed Poisson deviance residuals for observation intervals.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsPearson, Export interval count differences divided by the square root of the fitted interval mean.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsQuantile, Export the same cached normal quantile residuals used in diagnostic and probability plots. Set a seed under Advanced to reproduce them.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsRegression, Export the package's log-scale regression residuals. Available for least-squares fits.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsResponse, Export observed minus fitted interval event counts.
#'    Defaults to \code{FALSE}.
#' @param fittedModelEquation, Show the fitted model equations in a table.
#'    Defaults to \code{FALSE}.
#' @param fittedModelEquationDecimals, Set the number of decimal places displayed in the fitted equations.
#' @param model, Choose the recurrent-event model. Crow–AMSAA is a power-law non-homogeneous Poisson process (NHPP).
#' @param modelSummary, Include a table with information about the model fit.
#'    Defaults to \code{TRUE}.
#' @param piecewiseChangePoints, Show estimated change-point times in the original time units and native standard errors on the log-time scale.
#'    Defaults to \code{TRUE}.
#' @param piecewiseSegmentEstimates, Show segment slopes, their standard errors, growth rates and scales returned by ReliaGrowR.
#'    Defaults to \code{TRUE}.
#' @param piecewiseStartingBreakpoints, Provide initial locations for estimated change points. When unchecked, the package initializes one change point automatically. These locations are starting values, not fixed change points.
#'    Defaults to \code{FALSE}.
#' @param piecewiseStartingBreakpointsValues, Enter starting change-point times in the original time units, separated by commas.
#' @param plotLegend, Choose the legend position for prediction plots.
#' @param plotTheme, Choose the theme for prediction plots.
#' @param predictionsDisplayConfidenceIntervals, Include pointwise confidence intervals for the figures and tables. These are not prediction intervals for future counts. Rate-based intervals are unavailable for piecewise models.
#'    Defaults to \code{TRUE}.
#' @param predictionsDisplayConfidenceIntervalsLevel, Set the confidence level for the confidence intervals.
#' @param predictionsDisplayObservedData, Add observed cumulative events or cumulative mean time between events to the corresponding plots.
#'    Defaults to \code{TRUE}.
#' @param predictionsMergeSeriesAcrossSubgroups, Overlay predictions for the separately fitted subgroups.
#'    Defaults to \code{FALSE}.
#' @param predictionsMergeTablesAcrossMeasures, Combine the selected time-based prediction measures in one table.
#'    Defaults to \code{FALSE}.
#' @param predictionsMissionDuration, Set a positive mission duration in the original time units. The time grid specifies mission start times.
#' @param predictionsOutcomeCumulativeEvents, Predict the expected cumulative number of events since time zero.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeCumulativeMeanTimeBetweenEvents, Predict cumulative time divided by expected cumulative events. This is an average time per event up to the selected time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeEventIntensity, Predict the instantaneous event rate per unit time.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeFailureProbability, Predict the probability of at least one event from time zero to each selected time, 1 - exp(-M(t)), assuming an NHPP. For Duane, this assumes an NHPP interpretation of its fitted growth curve. This is the complement of event-free survival.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeInstantaneousMeanTimeBetweenEvents, Predict the reciprocal of instantaneous event intensity. This is a local rate-based measure, not the expected waiting time to the next event when the rate changes.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeMissionReliability, Predict the probability of no events during a mission starting at each selected time: exp(-(M(t + d) - M(t))), where d is the mission duration. This assumes an NHPP, including for the Duane growth curve.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutcomeSurvivalProbability, Predict event-free survival: the probability of no events from time zero to each selected time, exp(-M(t)), assuming an NHPP. For Duane, this assumes an NHPP interpretation of its fitted growth curve. This does not add patient-level censoring support.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutputPlot, Plot every selected measure in its own panel.
#'    Defaults to \code{TRUE}.
#' @param predictionsOutputTable, Show a table of the selected predictions.
#'    Defaults to \code{FALSE}.
#' @param predictionsOutputTableFormat, Show times in rows (Long) or separate column groups (Wide).
#' @param predictionsRoundTimeTableValues, Round evaluation times in tables to the nearest integer. Rounded times must remain positive.
#'    Defaults to \code{FALSE}.
#' @param predictionsTimeFrom, Set a positive first time in the sequence. Leave blank to use the first observed endpoint in each subgroup.
#' @param predictionsTimeGrid, Choose how to specify prediction times: equal intervals, a sequence or custom values.
#' @param predictionsTimeIntervals, Set the number of equal intervals between the first observed endpoint and maximum follow-up in each subgroup.
#' @param predictionsTimePercentages, Enter prediction times as percentages of the maximum follow-up time, separated by commas.
#' @param predictionsTimeStep, Set a positive time increment. Leave blank to use (To - From) / 10, or From when both endpoints are equal.
#' @param predictionsTimeTo, Set the last prediction time. Use a time beyond observed follow-up to forecast. Leave blank to use the maximum follow-up in each subgroup.
#' @param predictionsTimeValues, Enter positive prediction times separated by commas, in the original time units. Leave blank to use 25%, 50% and 75% of follow-up.
#' @param predictionsXAxisTransformation, Choose the x-axis scale for time.
#' @param predictionsYAxisScale, Choose the y-axis scale for the predicted quantities.
#' @param probabilityPlot, Check Poisson interval counts using randomized quantile residuals from fitted interval means. Normal Q-Q and uniform P-P plots should follow the reference line under an NHPP. This is a count-model diagnostic, not a lifetime probability plot. Set a seed under Advanced to reproduce it.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotCanvas, Compare quantile residuals with standard normal quantiles, or their cumulative probabilities with uniform plotting positions.
#' @param probabilityPlotColorPalette, Choose colors for subgroup probability diagnostics.
#' @param probabilityPlotEmpiricalPoints, Show one randomized quantile residual for each observation interval.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotEmpiricalPointsCoordinates, Display the theoretical and observed coordinates beside each point.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotGrid, Show grid lines on the probability plot.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotLegend, Choose the legend position for merged subgroup plots.
#' @param probabilityPlotMergePlotsAcrossSubgroups, Overlay probability diagnostics for separately fitted subgroups.
#'    Defaults to \code{FALSE}.
#' @param probabilityPlotPlottingPosition, Choose the theoretical plotting positions for the ordered interval residuals.
#' @param probabilityPlotReferenceEnvelope, Show pointwise order-statistic bounds for the reference distribution, treating fitted parameters as known. This exploratory envelope does not account for parameter estimation and is not a calibrated goodness-of-fit test.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotReferenceEnvelopeLevel, Set the pointwise reference-envelope coverage.
#' @param probabilityPlotReferenceLine, Show the identity line expected under the fitted NHPP count model.
#'    Defaults to \code{TRUE}.
#' @param probabilityPlotTheme, Choose the probability plot theme.
#' @param residualPlotResidualHistogram, Display the distribution of the selected residuals.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualType, Regression residuals use the package's log-scale least-squares regression. Response, Pearson, deviance and randomized quantile residuals use Poisson interval event counts with means obtained from the fitted cumulative curve. Count residuals assume an NHPP, including for Duane. Set a seed under Advanced to reproduce randomized quantile residuals.
#' @param residualPlotResidualVsPredicted, Plot residuals against fitted values on the regression scale, or fitted interval event counts for count residuals.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualVsTime, Plot the selected residuals against cumulative interval endpoints.
#'    Defaults to \code{FALSE}.
#' @param subgroup, Fit each combination of factor levels as an independent observation sequence. Do not split one sequence by changing operating conditions.
#' @param time, Select cumulative interval endpoints or interval durations. Rows must follow the observation order within each subgroup.
#' @param timeType, Choose how Time describes the observation intervals. Each sequence starts at zero. Maximum likelihood uses an interval-count likelihood rather than an exact-event-time likelihood.
RecurrentEventAnalysis <- function(
          data = NULL,
          version = "1",
          coefficients = TRUE,
          coefficientsConfidenceIntervals = TRUE,
          coefficientsConfidenceIntervalsLevel = 0.95,
          coefficientsCovarianceMatrix = FALSE,
          colorPalette = "colorblind",
          confidenceIntervalBounds = "twoSided",
          dataSummary = FALSE,
          estimationMethod = "mle",
          eventCount = list(types = list(), value = ""),
          exportColumnPrefix = "RecurrentEvents",
          exportFittedValuesCumulativeEvents = FALSE,
          exportFittedValuesIntervalEvents = FALSE,
          exportResidualsDeviance = FALSE,
          exportResidualsPearson = FALSE,
          exportResidualsQuantile = FALSE,
          exportResidualsRegression = FALSE,
          exportResidualsResponse = FALSE,
          fittedModelEquation = FALSE,
          fittedModelEquationDecimals = 3,
          model = "crowAmsaa",
          modelSummary = TRUE,
          piecewiseChangePoints = TRUE,
          piecewiseSegmentEstimates = TRUE,
          piecewiseStartingBreakpoints = FALSE,
          piecewiseStartingBreakpointsValues = "",
          plotHeight = 320,
          plotLegend = "right",
          plotTheme = "jasp",
          plotWidth = 480,
          predictionsDisplayConfidenceIntervals = TRUE,
          predictionsDisplayConfidenceIntervalsLevel = 0.95,
          predictionsDisplayObservedData = TRUE,
          predictionsMergeSeriesAcrossSubgroups = FALSE,
          predictionsMergeTablesAcrossMeasures = FALSE,
          predictionsMissionDuration = 1,
          predictionsOutcomeCumulativeEvents = FALSE,
          predictionsOutcomeCumulativeMeanTimeBetweenEvents = FALSE,
          predictionsOutcomeEventIntensity = FALSE,
          predictionsOutcomeFailureProbability = FALSE,
          predictionsOutcomeInstantaneousMeanTimeBetweenEvents = FALSE,
          predictionsOutcomeMissionReliability = FALSE,
          predictionsOutcomeSurvivalProbability = FALSE,
          predictionsOutputPlot = TRUE,
          predictionsOutputTable = FALSE,
          predictionsOutputTableFormat = "wide",
          predictionsRoundTimeTableValues = FALSE,
          predictionsTimeFrom = "",
          predictionsTimeGrid = "quantiles",
          predictionsTimeIntervals = 10,
          predictionsTimePercentages = "25, 50, 75",
          predictionsTimeStep = "",
          predictionsTimeTo = "",
          predictionsTimeValues = 0,
          predictionsXAxisTransformation = "none",
          predictionsYAxisScale = "none",
          probabilityPlot = FALSE,
          probabilityPlotCanvas = "normal",
          probabilityPlotColorPalette = "colorblind",
          probabilityPlotEmpiricalPoints = TRUE,
          probabilityPlotEmpiricalPointsCoordinates = FALSE,
          probabilityPlotGrid = TRUE,
          probabilityPlotLegend = "right",
          probabilityPlotMergePlotsAcrossSubgroups = FALSE,
          probabilityPlotPlottingPosition = "median",
          probabilityPlotReferenceEnvelope = TRUE,
          probabilityPlotReferenceEnvelopeLevel = 0.9,
          probabilityPlotReferenceLine = TRUE,
          probabilityPlotTheme = "jasp",
          residualPlotResidualHistogram = FALSE,
          residualPlotResidualType = "pearson",
          residualPlotResidualVsPredicted = FALSE,
          residualPlotResidualVsTime = FALSE,
          seed = 1,
          setSeed = FALSE,
          subgroup = list(types = list(), value = list()),
          time = list(types = list(), value = ""),
          timeType = "cumulative") {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorPalette", "confidenceIntervalBounds", "estimationMethod", "eventCount", "model", "plotLegend", "plotTheme", "predictionsOutputTableFormat", "predictionsTimeGrid", "predictionsXAxisTransformation", "predictionsYAxisScale", "probabilityPlotCanvas", "probabilityPlotColorPalette", "probabilityPlotLegend", "probabilityPlotPlottingPosition", "probabilityPlotTheme", "residualPlotResidualType", "subgroup", "time", "timeType")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspSurvival", "RecurrentEventAnalysis", "RecurrentEventAnalysis.qml", options, version, TRUE))
}