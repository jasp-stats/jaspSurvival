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

#' Non-Parametric Survival Analysis
#'
#' This analysis computes a survival curve for censored data using the Kaplan-Meier estimator for single-event survival data. It estimates the probability of survival over time, allowing you to understand and visualize the distribution of survival times within your data. The analysis accommodates censored observations (subjects for whom the event has not occurred during the study period) and enables comparison of survival curves across different groups using statistical tests such as the Log-Rank test, Peto and Peto test, and Fleming-Harrington test. Additionally, you can generate life tables to summarize survival data at specified intervals.
#'
#' @param censoringSummary, Create a summary table with information about the censoring status of the data.
#'    Defaults to \code{FALSE}.
#' @param colorPalette, Customize the color palette used in the plot. Only available when Strata variables are specified.
#' @param eventIndicator, Specify the value in the Event Status variable that indicates the occurrence of the event.
#' @param eventStatus, Choose the variable that indicates the event status, specifying whether each observation is an event or censored.
#' @param lifeTable, Generate a life table summarizing survival data at specified time intervals.
#'    Defaults to \code{FALSE}.
#' @param lifeTableRoundSteps, Round the interval boundaries to the nearest integer when using Quantiles steps.
#'    Defaults to \code{TRUE}.
#' @param lifeTableStepsFrom, Set the starting time for intervals when using Fixed size steps.
#' @param lifeTableStepsNumber, Specify the number of intervals when using Quantiles as the steps type.
#' @param lifeTableStepsSize, Define the size of each interval when using Fixed size steps.
#' @param lifeTableStepsTo, Set the ending time for intervals when using Fixed size steps.
#' @param lifeTableStepsType, Choose Default, Quantiles or Fixed size intervals. Quantiles uses frequency-weighted observed times, including censored times, rather than the estimated survival distribution.
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
#' @param strata, Select variables to define strata, allowing separate survival curves for each stratum.
#' @param testsFlemmingHarrington, Include the Fleming-Harrington test to compare survival curves across strata., with a customizable rho parameter.
#'    Defaults to \code{FALSE}.
#' @param testsFlemmingHarringtonRho, Set the rho parameter for the Fleming-Harrington test, controlling the weight given to different time points (values between 0 and 1).
#' @param testsLogRank, Include the Log-Rank (Mantel-Haenszel) test to compare survival curves across strata.
#'    Defaults to \code{FALSE}.
#' @param testsPetoAndPeto, Include the Peto and Peto modification of the Gehan-Wilcoxon test to compare survival curves across strata.
#'    Defaults to \code{FALSE}.
#' @param timeToEvent, Select the variable that represents the time until the event or censoring occurs.
#' @param weights, Select a variable for case weights, weighting each observation accordingly in the model.
NonParametricSurvivalAnalysis <- function(
          data = NULL,
          version = "1",
          censoringSummary = FALSE,
          colorPalette = "colorblind",
          eventIndicator = "",
          eventStatus = list(types = list(), value = ""),
          lifeTable = FALSE,
          lifeTableRoundSteps = TRUE,
          lifeTableStepsFrom = 0,
          lifeTableStepsNumber = 10,
          lifeTableStepsSize = 1,
          lifeTableStepsTo = 0,
          lifeTableStepsType = "default",
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
          strata = list(types = list(), value = list()),
          testsFlemmingHarrington = FALSE,
          testsFlemmingHarringtonRho = 0.5,
          testsLogRank = FALSE,
          testsPetoAndPeto = FALSE,
          timeToEvent = list(types = list(), value = ""),
          weights = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorPalette", "eventIndicator", "eventStatus", "lifeTableStepsType", "plotLegend", "plotTheme", "plotType", "strata", "timeToEvent", "weights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspSurvival", "NonParametricSurvivalAnalysis", "NonParametricSurvivalAnalysis.qml", options, version, TRUE))
}