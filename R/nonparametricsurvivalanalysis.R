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
    .sanpFitKaplanMeier(jaspResults, dataset, options)

  .sanpSummaryTable(jaspResults, dataset, options)
  .sanpLifeTable(jaspResults, dataset, options)

  return()
}

.sanpDependencies <- c("timeToEvent", "eventStatus", "eventIndicator", "factors")

.sanpFitKaplanMeier <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fit"]]))
    return()

  fitContainer <- createJaspState()
  fitContainer$dependOn(.sanpDependencies)
  jaspResults[["fit"]] <- fitContainer

  fit <- try(survival::survfit(
    formula = .saGetFormula(options, type = "KM"),
    type    = "kaplan-meier",
    data    = dataset
  ))

  jaspResults[["fit"]]$object <- fit

  return()
}
.sanpSummaryTable   <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Kaplan-Meier Summary Table"))
  summaryTable$dependOn(.sanpDependencies)
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  overtitleCi <- gettextf("%s%% CI", 95)

  if (length(options[["factors"]]) != 0)
    summaryTable$addColumnInfo(name = "strata",     title = "Strata",          type = "string")

  summaryTable$addColumnInfo(name = "n",                title = "N",               type = "integer")
  summaryTable$addColumnInfo(name = "events",           title = "Events",          type = "integer")
  summaryTable$addColumnInfo(name = "restrictedMean",   title = "Restricted Mean", type = "number")
  summaryTable$addColumnInfo(name = "restrictedMeanSe", title = "Standard Error",  type = "number")
  summaryTable$addColumnInfo(name = "median",           title = "Median Survival", type = "number")
  summaryTable$addColumnInfo(name = "lowerCI",          title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  summaryTable$addColumnInfo(name = "upperCI",          title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (is.null(jaspResults[["fit"]]))
    return()

  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit)) {
    summaryTable$setError(fit)
    return()
  }

  fitSummary <- .sanpKaplanMeierFitSummary(fit)
  summaryTable$setData(fitSummary)

  if (!is.null(attr(dataset, "na.action")))
    summaryTable$addFootnote(gettextf("%1$i observations ommited due to missing values.", length(attr(dataset, "na.action"))))

  return()
}
.sanpLifeTable      <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["LifeTableContainer"]]))
    return()

  LifeTableContainer <- createJaspContainer(title = gettext("Life Table"))
  LifeTableContainer$dependOn(c(.sanpDependencies, "lifeTable", "lifeTableRoundSteps", "lifeTableStepsType",
                       "lifeTableStepsNumber", "lifeTableStepsFrom", "lifeTableStepsSize", "lifeTableStepsTo"))
  LifeTableContainer$position <- 3
  jaspResults[["LifeTableContainer"]] <- LifeTableContainer

  if (is.null(jaspResults[["fit"]])) {
    jaspResults[["LifeTableContainer"]][["emptyTable"]] <- .sanpEmptyLifeTable()
    return()
  }

  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit)) {
    jaspResults[["LifeTableContainer"]][["emptyTable"]] <- .sanpEmptyLifeTable()
    return()
  }

  fitLifeTable <- try(.sanpKaplanMeierFitLifeTable(fit, dataset, options))

  if (jaspBase::isTryError(fitLifeTable)) {
    jaspResults[["LifeTableContainer"]][["emptyTable"]] <- .sanpEmptyLifeTable()
    jaspResults[["LifeTableContainer"]][["emptyTable"]]$setError(fitLifeTable)
    return()
  }


  if (length(options[["factors"]]) == 0) {

    tempTable <- .sanpEmptyLifeTable()
    tempTable$setData(fitLifeTable)

    jaspResults[["LifeTableContainer"]][["table"]] <- tempTable

  } else {

    i <- 0
    for (strata in unique(attr(fitLifeTable, "strata"))) {

      i <- i + 1
      tempTable <- .sanpEmptyLifeTable(title = strata, position = i)
      tempTable$setData(fitLifeTable[attr(fitLifeTable, "strata") == strata, ])

      jaspResults[["LifeTableContainer"]][[paste0("table", i)]] <- tempTable

    }
  }

  return()
}


.sanpKaplanMeierFitSummary   <- function(fit) {

  fitSummary <- summary(fit)$table

  if(is.null(dim(fitSummary)))
    fitSummary <- data.frame(t(fitSummary))
  else
    fitSummary <- data.frame(fitSummary)

  fitSummary <- fitSummary[,c("records", "events", "rmean", "se.rmean.",  "median", "X0.95LCL", "X0.95UCL")]
  colnames(fitSummary) <- c("n", "events", "restrictedMean", "restrictedMeanSe", "median", "lowerCI", "upperCI")

  if(nrow(fitSummary) > 1)
    fitSummary$strata <- rownames(fitSummary)

  return(fitSummary)
}
.sanpKaplanMeierFitLifeTable <- function(fit, dataset, options) {

  summaryFit <- summary(fit, times = .saLifeTableTimes(dataset, options))

  lifeTable <- with(summaryFit, data.frame(
    time          = time,
    atRisk        = n.risk,
    events        = n.event,
    survival      = surv,
    standardError = std.err,
    lowerCI       = lower,
    upperCI       = upper
  ))

  attr(lifeTable, "strata") <- summaryFit$strata

  return(lifeTable)
}
.sanpEmptyLifeTable          <- function(title = "", position = 1) {

  tempTable <- createJaspTable(title = title)
  tempTable$position <- position

  overtitleCi <- gettextf("%s%% CI", 95)
  tempTable$addColumnInfo(name = "time",           title = "Time",            type = "number")
  tempTable$addColumnInfo(name = "atRisk",         title = "N",               type = "integer")
  tempTable$addColumnInfo(name = "events",         title = "At Risk",         type = "integer")
  tempTable$addColumnInfo(name = "survival",       title = "Survival",        type = "number")
  tempTable$addColumnInfo(name = "standardError",  title = "Standard Error",  type = "number")
  tempTable$addColumnInfo(name = "lowerCI",        title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  tempTable$addColumnInfo(name = "upperCI",        title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  return(tempTable)
}

