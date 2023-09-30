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

NonParametricSurvivalAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  # non-parametric allows only for right censored data -- set the options for generic functions downstream
  options[["censoringType"]] <- "right"

  if (.saSurvivalReady(options))
    dataset <- .saReadDataset(dataset, options)

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  if (.saSurvivalReady(options))
    .saFitKaplanMeier(jaspResults, dataset, options)

  .saSummaryTable(jaspResults, dataset, options)

  return()
}

.saNonParametricDependencies <- c("timeToEvent", "eventStatus", "eventIndicator", "factors")

.saFitKaplanMeier <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fit"]]))
    return()

  fitContainer <- createJaspState()
  fitContainer$dependOn(.saNonParametricDependencies)
  jaspResults[["fit"]] <- fitContainer

  fit <- try(survival::survfit(
    formula = .saGetFormula(.saGetSurv(options), options[["factors"]]),
    type    = "kaplan-meier",
    data    = .saAddSurvData(dataset, options)
  ))

  jaspResults[["fit"]]$object <- fit

  return()
}
.saSummaryTable   <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Kaplan-Meier Summary Table"))
  summaryTable$dependOn(.saNonParametricDependencies)
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  overtitleCi <- gettextf("%s%% CI", 95)

  if (length(options[["factors"]]) != 0)
    summaryTable$addColumnInfo(name = "strata",     title = "Strata",          type = "string")

  summaryTable$addColumnInfo(name = "n",            title = "N",               type = "integer")
  summaryTable$addColumnInfo(name = "events",       title = "Events",          type = "integer")
  summaryTable$addColumnInfo(name = "median",       title = "Median Survival", type = "number")
  summaryTable$addColumnInfo(name = "lowerCI",      title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  summaryTable$addColumnInfo(name = "upperCI",      title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (is.null(jaspResults[["fit"]]))
    return()

  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit)) {
    summaryTable$setError(fit)
    return()
  }

  fitSummary <- .saKaplanMeierFitSummary(fit)
  summaryTable$setData(fitSummary)

  if (!is.null(attr(dataset, "na.action")))
    summaryTable$addFootnote(gettextf("%1$i observations ommited due to missing values.", length(attr(dataset, "na.action"))))

  return()
}

.saKaplanMeierFitSummary <- function(fit) {

  # there is no output from the print function unfortunately
  # this function formats a data.frame that can be displayed in JASP

  fitSummary <- survival:::survmean(fit, rmean = "none")$matrix

  if(is.null(dim(fitSummary)))
    fitSummary <- data.frame(t(fitSummary))
  else
    fitSummary <- data.frame(fitSummary)

  fitSummary <- fitSummary[,c("records", "events", "median", "X0.95LCL", "X0.95UCL")]
  colnames(fitSummary) <- c("n", "events", "median", "lowerCI", "upperCI")

  if(nrow(fitSummary) > 1)
    fitSummary$strata <- rownames(fitSummary)

  return(fitSummary)
}
