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

  if (.saSurvivalReady(options)) {
    .sanpFitKaplanMeier(jaspResults, dataset, options)
    .sanpFitTests(jaspResults, dataset, options)
  }

  saveRDS(jaspResults[["fit"]]$object, file = "C:/JASP/fit.RDS")

  .sanpSummaryTable(jaspResults, dataset, options)
  .sanpTestsTable(jaspResults, dataset, options)

  if (options[["plot"]])
    .sanpSurvivalPlot(jaspResults, dataset, options)

  if (options[["lifeTable"]])
    .sanpLifeTable(jaspResults, dataset, options)

  return()
}

.sanpDependencies <- c("timeToEvent", "eventStatus", "eventIndicator", "factors")

.sanpFitKaplanMeier  <- function(jaspResults, dataset, options) {

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
  # fix scoping in ggsurvplot
  fit$call$formula <- eval(fit$call$formula)


  jaspResults[["fit"]]$object <- fit

  return()
}
.sanpFitTests        <- function(jaspResults, dataset, options) {

  if (length(options[["factors"]]) == 0)
    return()

  if (options[["testsLogRank"]] && is.null(jaspResults[["testLogRank"]])) {

    tempContainer <- createJaspState()
    tempContainer$dependOn(c(.sanpDependencies, "testsLogRank"))
    jaspResults[["testLogRank"]] <- tempContainer

    fit <- try(survival::survdiff(
      formula = .saGetFormula(options, type = "KM"),
      data    = dataset
    ))

    jaspResults[["testLogRank"]]$object <- fit
  }

  if (options[["testsPetoAndPeto"]] && is.null(jaspResults[["testPetoAndPeto"]])) {

    tempContainer <- createJaspState()
    tempContainer$dependOn(c(.sanpDependencies, "testsPetoAndPeto"))
    jaspResults[["testPetoAndPeto"]] <- tempContainer

    fit <- try(survival::survdiff(
      formula = .saGetFormula(options, type = "KM"),
      data    = dataset,
      rho     = 1
    ))

    jaspResults[["testPetoAndPeto"]]$object <- fit
  }

  if (options[["testsFlemmingHarrington"]] && is.null(jaspResults[["testFlemmingHarrington"]])) {

    tempContainer <- createJaspState()
    tempContainer$dependOn(c(.sanpDependencies, "testsFlemmingHarrington", "testsFlemmingHarringtonRho"))
    jaspResults[["testFlemmingHarrington"]] <- tempContainer

    fit <- try(survival::survdiff(
      formula = .saGetFormula(options, type = "KM"),
      data    = dataset,
      rho     = options[["testsFlemmingHarringtonRho"]]
    ))

    jaspResults[["testFlemmingHarrington"]]$object <- fit
  }

  return()
}
.sanpSummaryTable    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Kaplan-Meier Summary Table"))
  summaryTable$dependOn(.sanpDependencies)
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  overtitleCi <- gettextf("%s%% CI", 95)

  if (length(options[["factors"]]) != 0)
    summaryTable$addColumnInfo(name = "strata",     title = gettext("Strata"),          type = "string")

  summaryTable$addColumnInfo(name = "n",                title = gettext("N"),               type = "integer")
  summaryTable$addColumnInfo(name = "events",           title = gettext("Events"),          type = "integer")
  summaryTable$addColumnInfo(name = "restrictedMean",   title = gettext("Restricted Mean"), type = "number")
  summaryTable$addColumnInfo(name = "restrictedMeanSe", title = gettext("Standard Error"),  type = "number")
  summaryTable$addColumnInfo(name = "median",           title = gettext("Median Survival"), type = "number")
  summaryTable$addColumnInfo(name = "lowerCI",          title = gettext("Lower"),           type = "number", overtitle = overtitleCi)
  summaryTable$addColumnInfo(name = "upperCI",          title = gettext("Upper"),           type = "number", overtitle = overtitleCi)

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
.sanpTestsTable      <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["testsTable"]]))
    return()

  if (!(options[["testsLogRank"]] || options[["testsPetoAndPeto"]] || options[["testsFlemmingHarrington"]]))
    return()

  testsTable <- createJaspTable(title = gettext("Tests Table"))
  testsTable$dependOn(c(.sanpDependencies, "testsLogRank", "testsPetoAndPeto", "testsFlemmingHarrington", "testsFlemmingHarringtonRho"))
  testsTable$position <- 2
  jaspResults[["testsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test",     title = gettext("Test"),         type = "string")
  testsTable$addColumnInfo(name = "chiSqr",   title = gettext("Chi Square"),   type = "number")
  testsTable$addColumnInfo(name = "df",       title = gettext("df"),           type = "integer")
  testsTable$addColumnInfo(name = "p",        title = gettext("p"),            type = "pvalue")

  if (length(options[["factors"]]) == 0) {
    testsTable$addFootnote(gettext("At least one factor needs to be specified"))
    return()
  }

  if (options[["testsLogRank"]]) {
    fit <- jaspResults[["testLogRank"]]$object
    if (jaspBase::isTryError(fit))
      testsTable$addFootnote(fit, symbol = "Warning:")
    else
      testsTable$addRows(.sanpExtractTest(fit, gettext("Log-rank (Mantel-Haenszel)")))
  }

  if (options[["testsPetoAndPeto"]]) {
    fit <- jaspResults[["testPetoAndPeto"]]$object
    if (jaspBase::isTryError(fit))
      testsTable$addFootnote(fit, symbol = "Warning:")
    else
      testsTable$addRows(.sanpExtractTest(fit, gettext("Peto and Peto")))
  }

  if (options[["testsFlemmingHarrington"]]) {
    fit <- jaspResults[["testFlemmingHarrington"]]$object
    if (jaspBase::isTryError(fit))
      testsTable$addFootnote(fit, symbol = "Warning:")
    else
      testsTable$addRows(.sanpExtractTest(fit, gettext("Flemming-Harrington")))
  }

  return()
}
.sanpSurvivalPlot    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["surivalPlot"]]))
    surivalPlot <- jaspResults[["surivalPlot"]]
  else {
    surivalPlot <- createJaspPlot(switch(
      options[["plotType"]],
      "survival"             = gettext("Kaplan-Meier Survival Plot"),
      "risk"                 = gettext("Kaplan-Meier Risk Plot"),
      "cumulativeHazard"     = gettext("Kaplan-Meier Cumulative Hazard Plot"),
      "complementaryLogLog"  = gettext("Kaplan-Meier Complementary Log-Log Plot")
    ))
    surivalPlot$dependOn(c(.sanpDependencies, "plot", "plotType", "plotConfidenceInterval", "plotRiskTable",
                            "plotRiskTableValue", "plotAddQuantile", "plotAddQuantileValue",
                            "colorPalette", "plotLegend", "plotTheme"))

    surivalPlot$position <- 3
    jaspResults[["surivalPlot"]] <- surivalPlot
  }

  if (is.null(jaspResults[["fit"]])) {
    return()
  }


  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit)) {
    return()
  }

  ## old code: manually creating survival plot
  #fitLifeTable <- try(.sanpKaplanMeierFitLifeTable(fit, dataset, options, plot = TRUE))
  #
  #if (jaspBase::isTryError(fitLifeTable)) {
  #  surivalPlot$setError(fitLifeTable)
  #  return()
  #}
  #
  #tempPlot <- try(.sanpPlotLifeTable(fitLifeTable, options))


  ## old code: using survminer
  # tempPlot  <- try(survminer::ggsurvplot(
  #   fit,
  #   data = dataset,
  #
  #   palette  = jaspGraphs::JASPcolors(palette = options[["colorPalette"]]),
  #   conf.int = options[["survivalCurvePlotConfidenceInterval"]],
  #
  #   title = gettext("Survival curves"),
  #
  #   risk.table    = options[["survivalCurvePlotRiskTable"]],
  #   cumevents     = options[["survivalCurvePlotCumulativeEventsTable"]],
  #   ncensor.plot  = options[["survivalCurveCensoringPlot"]],
  #   cumcensor     = options[["survivalCurveCensoringPlotCumulative"]]
  # ))

  if (jaspBase::isTryError(tempPlot)) {
    surivalCurvePlot$setError(tempPlot)
    return()
  }


  return()
}
.sanpLifeTable       <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["LifeTableContainer"]]))
    return()

  LifeTableContainer <- createJaspContainer(title = gettext("Life Table"))
  LifeTableContainer$dependOn(c(.sanpDependencies, "lifeTable", "lifeTableRoundSteps", "lifeTableStepsType",
                       "lifeTableStepsNumber", "lifeTableStepsFrom", "lifeTableStepsSize", "lifeTableStepsTo"))
  LifeTableContainer$position <- 4
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

  if (is.null(dim(fitSummary)))
    fitSummary <- data.frame(t(fitSummary))
  else
    fitSummary <- data.frame(fitSummary)

  fitSummary <- fitSummary[,c("records", "events", "rmean", "se.rmean.",  "median", "X0.95LCL", "X0.95UCL")]
  colnames(fitSummary) <- c("n", "events", "restrictedMean", "restrictedMeanSe", "median", "lowerCI", "upperCI")

  if (nrow(fitSummary) > 1)
    fitSummary$strata <- rownames(fitSummary)

  return(fitSummary)
}
.sanpKaplanMeierFitLifeTable <- function(fit, dataset, options, plot = FALSE) {

  if (plot || options[["lifeTableStepsType"]] == "default")
    summaryFit <- summary(fit)
  else
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

  if (plot && !is.null(summaryFit$strata)) {
    lifeTable$strata <- summaryFit$strata
    lifeTable <- split(lifeTable, lifeTable$strata)
    lifeTable <- lapply(lifeTable, .sanpPlotLifeTableFun)
    lifeTable <- do.call(rbind, lifeTable)
  } else if (plot) {
    lifeTable <- .sanpPlotLifeTableFun(lifeTable)
  } else if (!is.null(summaryFit$strata)) {
    attr(lifeTable, "strata") <- summaryFit$strata
  }

  return(lifeTable)
}
.sanpPlotLifeTableFun        <- function(lifeTable) {
  # modifies the life table such that the resulting plot line is straight from one time to another
  lifeTable2      <- lifeTable[-nrow(lifeTable),]
  lifeTable2$time <- lifeTable$time[-1] - 1e-6

  lifeTable0               <- lifeTable[c(1, 1),]
  lifeTable0$time[1]       <- 0
  lifeTable0$events        <- 0
  lifeTable0$survival      <- 1
  lifeTable0$standardError <- NA
  lifeTable0$lowerCI       <- NA
  lifeTable0$upperCI       <- NA

  return(rbind(lifeTable0, lifeTable, lifeTable2))
}
.sanpPlotLifeTable           <- function(fitLifeTable, options) {

  if (length(options[["factors"]]) == 0) {

    tempPlot <- ggplot2::ggplot(fitLifeTable) +
      jaspGraphs::geom_line(mapping = ggplot2::aes(x = time, y = survival))

    if (options[["survivalCurvePlotConfidenceInterval"]])
      tempPlot <- tempPlot + ggplot2::geom_ribbon(mapping = ggplot2::aes(x = time, ymin = lowerCI, ymax = upperCI), alpha = 0.1, size = 1)

    if (options[["survivalCurvePlotDataRug"]])
      tempPlot <- tempPlot + ggplot2::geom_rug(ggplot2::aes(x = time, y = survival), sides = "b", alpha = 0.5)

  } else {

    tempPlot <- ggplot2::ggplot(fitLifeTable) +
      jaspGraphs::geom_line(mapping = ggplot2::aes(x = time, y = survival, group = strata, color = strata))

    if (options[["survivalCurvePlotConfidenceInterval"]])
      tempPlot <- tempPlot + ggplot2::geom_ribbon(mapping = ggplot2::aes(x = time, ymin = lowerCI, ymax = upperCI, group = strata, fill = strata), alpha = 0.1, size = 1)

    if (options[["survivalCurvePlotDataRug"]])
      tempPlot <- tempPlot + ggplot2::geom_rug(ggplot2::aes(x = time, y = survival, color = strata), sides = "b", alpha = 0.5)

    tempPlot <- tempPlot + jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]]) +
      jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
      ggplot2::labs(fill = gettext("Strata"), color = gettext("Strata"))

  }

  tempPlot <- tempPlot +
    jaspGraphs::scale_x_continuous(name = gettext("Time")) +
    jaspGraphs::scale_y_continuous(name = gettext("Survival")) +
    jaspGraphs::geom_rangeframe(sides = 'bl') +
    jaspGraphs::themeJaspRaw(legend.position = if (length(options[["factors"]]) != 0) options[["survivalCurvePlotLegend"]])

  return(tempPlot)
}
.sanpEmptyLifeTable          <- function(title = "", position = 1) {

  tempTable <- createJaspTable(title = title)
  tempTable$position <- position

  overtitleCi <- gettextf("%s%% CI", 95)
  tempTable$addColumnInfo(name = "time",           title = gettext("Time"),            type = "number")
  tempTable$addColumnInfo(name = "atRisk",         title = gettext("At Risk"),         type = "integer")
  tempTable$addColumnInfo(name = "events",         title = gettext("Events"),          type = "integer")
  tempTable$addColumnInfo(name = "survival",       title = gettext("Survival"),        type = "number")
  tempTable$addColumnInfo(name = "standardError",  title = gettext("Standard Error"),  type = "number")
  tempTable$addColumnInfo(name = "lowerCI",        title = gettext("Lower"),           type = "number", overtitle = overtitleCi)
  tempTable$addColumnInfo(name = "upperCI",        title = gettext("Upper"),           type = "number", overtitle = overtitleCi)

  return(tempTable)
}
.sanpExtractTest             <- function(fit, title) {
  return(list(
    test   = title,
    chiSqr = fit$chisq,
    df     = length(fit$n) - 1,
    p      = fit$pvalue
  ))
}
