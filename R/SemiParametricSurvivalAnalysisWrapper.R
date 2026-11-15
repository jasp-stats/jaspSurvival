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

#' Semi-Parametric Survival Analysis
#'
#' This analysis performs a Cox proportional hazards regression, a statistical method used in survival analysis to examine the relationship between the time until an event occurs (such as death or failure) and one or more predictor variables. It helps you understand how different factors influence the risk or hazard of the event happening at any given time. The Cox model is particularly useful because it can handle censored data (when the event has not occurred for some subjects during the study period) and does not require assumptions about the baseline hazard function.
#'
#' @param censoringSummary, Create a summary table with information about the censoring status of the data.
#'    Defaults to \code{FALSE}.
#' @param censoringType, Select the type of censoring in your data: Right censoring or Counting process.
#' \itemize{
#'   \item \code{"right"}
#'   \item \code{"counting"}
#' }
#' @param cluster, Select a variable to define clusters of correlated observations for robust variance estimation. Disabled when a Frailty variable is specified or Method is set to Exact.
#' @param coefficientEstimate, Display the estimated coefficients in the model output.
#'    Defaults to \code{TRUE}.
#' @param coefficientHazardRatioEstimates, Display the hazard ratio estimates (exponentiated coefficients).
#'    Defaults to \code{TRUE}.
#' @param coefficientHazardRatioEstimatesIncludeFrailty, Include the effect of frailty in hazard ratio estimates. Only available when a Frailty variable is specified.
#'    Defaults to \code{FALSE}.
#' @param coefficientsConfidenceIntervals, Include confidence intervals for the coefficients.
#'    Defaults to \code{TRUE}.
#' @param coefficientsConfidenceIntervalsLevel, Set the confidence level (e.g., 95%) for the confidence intervals.
#' @param colorPalette, Customize the color palette used in the plot. Only available when Strata variables are specified.
#' @param covariates, Add continuous variables as covariates to include them in the Cox regression model.
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
#' @param factors, Add categorical variables as factors to include them in the Cox regression model.
#' @param frailty, Select a variable for frailty to model unobserved heterogeneity using random effects. Disabled when a Cluster variable is specified.
#' @param frailtyDistribution, Choose the distribution for the frailty term: Gamma, Gaussian, or T distribution. Only available when a Frailty variable is specified.
#' @param frailtyMethod, Select the estimation method for the frailty distribution. Options vary based on the chosen Distribution.
#' @param frailtyMethodFixed, Choose the parameter to fix in the frailty model when Method is set to Fixed: Theta or Df.
#' @param frailtyMethodFixedDf, Specify the degrees of freedom (Df) to fix in the frailty model. Only visible when Fix is set to Df.
#' @param frailtyMethodFixedTheta, Specify the value of Theta to fix in the frailty model. Only visible when Fix is set to Theta.
#' @param frailtyMethodTDf, Set the degrees of freedom (Df) for the T frailty distribution. Only visible when Distribution is set to T.
#' @param intervalEnd, Select the variable that represents the end time of the observation interval. Only available when Censoring Type is set to Counting.
#' @param intervalStart, Select the variable that represents the start time of the observation interval. Only available when Censoring Type is set to Counting.
#' @param method, Choose the method for handling tied event times: Efron, Breslow, or Exact.
#' @param modelFit, Include overall model fit statistics in the output.
#'    Defaults to \code{FALSE}.
#' @param plot, This option generates a customizable survival plot to visualize the estimated survival probabilities over time from your survival analysis. You can choose different transformations of the survival probabilities to display, such as Survival, Risk, Cumulative Hazard, or Complementary Log-Log functions, depending on your analytical needs. The plot allows you to include confidence intervals to assess the precision of the estimates and add a risk table to provide detailed information about the number of subjects at risk and events over time. You can further enhance the plot by adding quantile lines to highlight specific survival times (e.g., median survival time) and adjust the legend position, color palette, and overall theme to suit your preferences.
#'    Defaults to \code{FALSE}.
#' @param plotAddQuantile, Add a vertical line on the plot at a specified quantile (e.g., median survival time).
#'    Defaults to \code{FALSE}.
#' @param plotAddQuantileValue, Specify the quantile value (between 0 and 1) at which to add the vertical line on the plot.
#' @param plotCi, Include confidence intervals in the survival plot to show the variability around the estimates.
#'    Defaults to \code{TRUE}.
#' @param plotLegend, Choose the position of the legend on the plot: Bottom, Right, Left, Top, or None. Only available when Strata variables are specified.
#' @param plotRiskTable, Add a risk table below the plot to display additional information about the number of subjects at risk over time.
#'    Defaults to \code{FALSE}.
#' @param plotRiskTableAsASingleLine, Combine all selected risk table statistics into a single line for a more compact display.
#'    Defaults to \code{FALSE}.
#' @param plotRiskTableCumulativeNumberOfCensoredObservations, Display the cumulative number of censored observations at each time point in the risk table.
#'    Defaults to \code{FALSE}.
#' @param plotRiskTableCumulativeNumberOfObservedEvents, Display the cumulative number of observed events (e.g., deaths) at each time point in the risk table.
#'    Defaults to \code{TRUE}.
#' @param plotRiskTableNumberAtRisk, Display the number of subjects at risk at each time point in the risk table.
#'    Defaults to \code{TRUE}.
#' @param plotRiskTableNumberOfCensoredObservationsInTimeInterval, Display the number of censored observations within each time interval in the risk table.
#'    Defaults to \code{FALSE}.
#' @param plotRiskTableNumberOfEventsInTimeInterval, Display the number of events that occurred within each time interval in the risk table.
#'    Defaults to \code{FALSE}.
#' @param plotTheme, Select the theme for the plot's appearance: JASP for the default look or ggsurvfit for the ggsurvfit package style.
#' @param plotType, Select the type of statistic to plot: Survival, Risk, Cumulative Hazard, or Complementary Log-Log.
#' @param proportionalHazardsPlot, Generate plots to assess the proportional hazards assumption.
#'    Defaults to \code{FALSE}.
#' @param proportionalHazardsTable, Provide a table to test the proportional hazards assumption.
#'    Defaults to \code{FALSE}.
#' @param proportionalHazardsTestTerms, Test the proportional hazards assumption for individual model terms (instead of coefficients).
#'    Defaults to \code{FALSE}.
#' @param proportionalHazardsTransformation, Select the transformation for time in proportional hazards testing: KM, Rank, or Identity.
#' @param residualPlotResidualHistogram, Display a histogram of residuals to assess their distribution. Available only for Martingale or Deviance residuals.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualType, Select the type of residuals to plot: Martingale, Deviance, Score, Schoenfeld, or Scaled Schoenfeld.
#' @param residualPlotResidualVsPredicted, Plot residuals versus predicted survival probabilities. Available only for Martingale or Deviance residuals.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualVsPredictors, Plot residuals versus predictors to assess model fit. Available when model terms are specified.
#'    Defaults to \code{FALSE}.
#' @param residualPlotResidualVsTime, Plot residuals versus time to detect non-proportional hazards. Available only for Martingale or Deviance residuals.
#'    Defaults to \code{FALSE}.
#' @param strata, Select variables to define strata, allowing separate baseline hazard functions for each stratum.
#' @param testsLikelihoodRatio, Include the Likelihood Ratio Test in the model summary.
#'    Defaults to \code{FALSE}.
#' @param testsScore, Include the Score (Log-Rank) Test in the model summary. Disabled when a Frailty variable is specified.
#'    Defaults to \code{FALSE}.
#' @param testsWald, Include the Wald Test in the model summary. Disabled when a Frailty variable is specified.
#'    Defaults to \code{FALSE}.
#' @param timeToEvent, Select the variable that represents the time until the event or censoring occurs. Only available when Censoring Type is set to Right.
#' @param vovkSellke, Include the Vovk-Sellke maximum p-ratio for each coefficient.
#'    Defaults to \code{FALSE}.
#' @param weights, Select a variable for case weights, weighting each observation accordingly in the model.
SemiParametricSurvivalAnalysis <- function(
          data = NULL,
          version = "1",
          censoringSummary = FALSE,
          censoringType = "right",
          cluster = list(types = list(), value = ""),
          coefficientEstimate = TRUE,
          coefficientHazardRatioEstimates = TRUE,
          coefficientHazardRatioEstimatesIncludeFrailty = FALSE,
          coefficientsConfidenceIntervals = TRUE,
          coefficientsConfidenceIntervalsLevel = 0.95,
          colorPalette = "colorblind",
          covariates = list(types = list(), value = list()),
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
          frailty = list(types = list(), value = ""),
          frailtyDistribution = "gamma",
          frailtyMethod = "em",
          frailtyMethodFixed = "theta",
          frailtyMethodFixedDf = 0,
          frailtyMethodFixedTheta = 0,
          frailtyMethodTDf = 5,
          intervalEnd = list(types = list(), value = ""),
          intervalStart = list(types = list(), value = ""),
          method = "efron",
          modelFit = FALSE,
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          plot = FALSE,
          plotAddQuantile = FALSE,
          plotAddQuantileValue = 0.5,
          plotCi = TRUE,
          plotHeight = 320,
          plotLegend = "bottom",
          plotRiskTable = FALSE,
          plotRiskTableAsASingleLine = FALSE,
          plotRiskTableCumulativeNumberOfCensoredObservations = FALSE,
          plotRiskTableCumulativeNumberOfObservedEvents = TRUE,
          plotRiskTableNumberAtRisk = TRUE,
          plotRiskTableNumberOfCensoredObservationsInTimeInterval = FALSE,
          plotRiskTableNumberOfEventsInTimeInterval = FALSE,
          plotTheme = "jasp",
          plotType = "survival",
          plotWidth = 480,
          proportionalHazardsPlot = FALSE,
          proportionalHazardsTable = FALSE,
          proportionalHazardsTestTerms = FALSE,
          proportionalHazardsTransformation = "km",
          residualPlotResidualHistogram = FALSE,
          residualPlotResidualType = "martingale",
          residualPlotResidualVsPredicted = FALSE,
          residualPlotResidualVsPredictors = FALSE,
          residualPlotResidualVsTime = FALSE,
          strata = list(types = list(), value = list()),
          testsLikelihoodRatio = FALSE,
          testsScore = FALSE,
          testsWald = FALSE,
          timeToEvent = list(types = list(), value = ""),
          vovkSellke = FALSE,
          weights = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("cluster", "colorPalette", "covariates", "eventIndicator", "eventStatus", "factors", "frailty", "frailtyDistribution", "frailtyMethod", "frailtyMethodFixed", "intervalEnd", "intervalStart", "method", "modelTerms", "plotLegend", "plotTheme", "plotType", "proportionalHazardsTransformation", "residualPlotResidualType", "strata", "timeToEvent", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspSurvival", "SemiParametricSurvivalAnalysis", "SemiParametricSurvivalAnalysis.qml", options, version, TRUE))
}