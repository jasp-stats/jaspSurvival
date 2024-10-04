# TODO:
# qml:
# - make the event indicator be the second lvl of the event status variable
# - make "lifeTableStepsTo" correspond to the maximum time
# - make "lifeTableStepsSize" correspond to the maxiumum time/10
# R:
# - convenient function for making factor level parameter names nice (in regression tables)?
# - add frailty to coxph

.saSurvivalReady      <- function(options) {

  ready <- switch(
    options[["censoringType"]],
    "counting" = options[["eventStatus"]] != "" && options[["intervalStart"]] != "" && options[["intervalEnd"]] != "",
    "right"    = options[["eventStatus"]] != "" && options[["timeToEvent"]] != ""
  )

  return(ready)
}
.saReadDataset        <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  # load the data
  eventVariable <- options[["eventStatus"]]
  timeVariable  <- switch(
    options[["censoringType"]],
    "counting" = c(options[["intervalStart"]], options[["intervalEnd"]]),
    "right"    = options[["timeToEvent"]]
  )

  factorsVariable    <- Filter(function(s) s != "", options[["factors"]])
  # only for (semi)parametric
  covariatesVariable <- Filter(function(s) s != "", options[["covariates"]])
  strataVariable     <- Filter(function(s) s != "", options[["strata"]])
  # idVariable         <- Filter(function(s) s != "", options[["id"]])
  clusterVariable    <- Filter(function(s) s != "", options[["cluster"]])
  weightsVariable    <- Filter(function(s) s != "", options[["weights"]])
  frailtyVariable    <- Filter(function(s) s != "", options[["frailty"]])

  dataset <- .readDataSetToEnd(
    columns.as.numeric = c(timeVariable, covariatesVariable, weightsVariable),
    columns.as.factor  = c(eventVariable, factorsVariable, strataVariable, clusterVariable, frailtyVariable),
  )

  # clean from NAs
  dataset <- na.omit(dataset)

  # recode the event status variable
  dataset[[eventVariable]] <- .saRecodeEventStatus(dataset, options)

  # check of errors
  .hasErrors(
    dataset                      = dataset,
    type                         = c("negativeValues"),
    negativeValues.target        = c(timeVariable, weightsVariable),
    exitAnalysisIfErrors         = TRUE
  )

  # check that interval start < end
  if (options[["censoringType"]] == "counting") {
    if (any(dataset[[options[["intervalStart"]]]] > dataset[[options[["intervalEnd"]]]]))
      .quitAnalysis(gettextf("The end time must be larger than start time."))
  }

  if (!is.null(covariatesVariable))
    .hasErrors(
      dataset                      = dataset,
      type                         = c("infinity", "observations", "variance", "varCovData"),
      all.target                   = covariatesVariable,
      varCovData.corFun            = stats::cov,
      observations.amount          = "< 2",
      exitAnalysisIfErrors         = TRUE
    )

  if (!is.null(options[["modelTerms"]]))
    .hasErrors(
      dataset                      = dataset,
      type                         = c("modelInteractions"),
      modelInteractions.modelTerms = options[["modelTerms"]],
      exitAnalysisIfErrors         = TRUE
    )

  return(dataset)
}
.saRecodeEventStatus  <- function(dataset, options) {

  if (options[["censoringType"]] == "right") {

    event <- dataset[[options[["eventStatus"]]]] == options[["eventIndicator"]]

  } else if (options[["censoringType"]] == "counting") {

    event <- dataset[[options[["eventStatus"]]]] == options[["eventIndicator"]]

  }

  return(event)
}
.saGetSurv            <- function(options) {
  return(switch(
    options[["censoringType"]],
    "counting" = sprintf("Surv(
      time  = %1$s,
      time2 = %2$s,
      event = %3$s,
      type  = 'counting')",
      options[["intervalStart"]],
      options[["intervalEnd"]],
      options[["eventStatus"]]
      ),
    "right"    = sprintf("Surv(
      time  = %1$s,
      event = %2$s,
      type  = 'right')",
      options[["timeToEvent"]],
      options[["eventStatus"]]
      )
  ))
}
.saGetFormula         <- function(options, type, null = FALSE) {

  if (type == "KM") {
    # nonparametric (Kaplan-Meier) only stratifies by factors
    predictors    <- options[["factors"]]
    interceptTerm <- TRUE
  } else if (type == "Cox") {
    # Cox proportional hazards always includes intercept
    predictors    <- c(.saGetPredictors(options, null = null), .saGetFrailty(options))
    interceptTerm <- TRUE
  }

  survival <- .saGetSurv(options)

  if (length(predictors) == 0 && !interceptTerm)
    stop(gettext("We need at least one predictor, or an intercept to make a formula"))

  if (length(predictors) == 0)
    formula <- paste(survival, "~", "1")
  else if (interceptTerm)
    formula <- paste(survival, "~", paste(predictors, collapse = "+"))
  else
    formula <- paste(survival, "~", paste(predictors, collapse = "+"), "-1")

  return(as.formula(formula, env = parent.frame(1)))
}
.saGetPredictors      <- function(options, null = FALSE) {
  # modified from jaspRegression::.createGlmFormula
  # this function outputs a formula name with base64 values as varnames
  modelTerms    <- options[["modelTerms"]]

  # add "strata" calls
  for (i in seq_along(options[["strata"]])) {
    for (j in seq_along(modelTerms)) {
      modelTerms[[j]]$components <- gsub(
        options[["strata"]][[i]],
        paste0("strata(", options[["strata"]][[i]], ")"),
        modelTerms[[j]]$components
      )
    }
  }

  t <- NULL
  for (i in seq_along(modelTerms)) {

    if (null)
      nui <- modelTerms[[i]][["isNuisance"]]
    else
      nui <- TRUE

    if (!is.null(nui) && nui) {
      term <- modelTerms[[i]][["components"]]
      if (length(term) == 1)
        t <- c(t, term)
      else
        t <- c(t, paste(unlist(term), collapse = ":"))
    }
  }

  return(t)
}
.saGetFrailty         <- function(options) {

  if (options[["frailty"]] == "")
    return()

  frailty <- sprintf(
    "frailty(%1$s, distribution = '%2$s', method = '%3$s'%4$s%5$s)",
    options[["frailty"]],
    options[["frailtyDistribution"]],
    options[["frailtyMethod"]],
    if (options[["frailtyMethod"]] != "fixed") ""
    else if (options[["frailtyMethodFixed"]] == "df")    paste0("df = ",    options[["frailtyMethodFixedDf"]])
    else if (options[["frailtyMethodFixed"]] == "theta") paste0("theta = ", options[["frailtyMethodFixedTheta"]]),
    if (options[["frailtyMethod"]] == "t")  paste0("tdf = ", options[["frailtyMethodTDf"]]) else ""
  )

  return(frailty)
}
.saLifeTableTimes     <- function(dataset, options) {

  times <- dataset[[switch(
    options[["censoringType"]],
    "right"    = options[["timeToEvent"]],
    "interval" = options[["intervalEnd"]]
  )]]

  timeSteps <- switch(
    options[["lifeTableStepsType"]],
    "quantiles" = seq(from = min(times), to = max(times), length.out = options[["lifeTableStepsNumber"]]),
    "fixedSize" = seq(from = options[["lifeTableStepsFrom"]], to = options[["lifeTableStepsTo"]], by = options[["lifeTableStepsSize"]])
  )

  if (options[["lifeTableStepsType"]] == "quantiles" && options[["lifeTableRoundSteps"]])
    timeSteps <- round(timeSteps)

  return(unique(timeSteps))
}
.saTermNames          <- function(varName, variables) {
  # based on jaspMixedModels::.mmVariableNames

  if (varName == "(Intercept)")
    return("Intercept")
  if (varName == "Global")
    return("Global")

  for (vn in variables) {
    inf <- regexpr(vn, varName, fixed = TRUE)

    if (inf[1] != -1) {
      varName <- paste0(
        substr(varName, 0, inf[1] - 1),
        substr(varName, inf[1], inf[1] + attr(inf, "match.length") - 1),
        " (",
        substr(varName, inf[1] + attr(inf, "match.length"), nchar(varName))
      )
    }

  }

  varName <- gsub(":", paste0(")", jaspBase::interactionSymbol), varName, fixed = TRUE)
  varName <- paste0(varName, ")")
  varName <- gsub(" ()", "", varName, fixed = TRUE)

  return(varName)
}

.saGetSurvivalPlotHeight <- function(options) {
  if (!options[["plotRiskTable"]])
    return(400)
  else if (!options[["plotRiskTableAsASingleLine"]])
    return(450)
  else
    return(400 + 50 * sum(c(
      options[["plotRiskTableNumberAtRisk"]],
      options[["plotRiskTableCumulativeNumberOfObservedEvents"]],
      options[["plotRiskTableCumulativeNumberOfCensoredObservations"]],
      options[["plotRiskTableNumberOfEventsInTimeInterval"]],
      options[["plotRiskTableNumberOfCensoredObservationsInTimeInterval"]]
    )))
}
.saSurvivalPlot          <- function(jaspResults, dataset, options, type) {

  if (!is.null(jaspResults[["surivalPlot"]]))
    surivalPlot <- jaspResults[["surivalPlot"]]
  else {
    surivalPlot <- createJaspPlot(title = switch(
      options[["plotType"]],
      "survival"             = gettext("Survival Plot"),
      "risk"                 = gettext("Risk Plot"),
      "cumulativeHazard"     = gettext("Cumulative Hazard Plot"),
      "complementaryLogLog"  = gettext("Complementary Log-Log Plot")
    ), width = 450, height = .saGetSurvivalPlotHeight(options))
    surivalPlot$dependOn(c(.sanpDependencies, "plot", "plotType", "plotStrata", "plotConfidenceInterval", "plotRiskTable",
                           "plotRiskTableNumberAtRisk", "plotRiskTableCumulativeNumberOfObservedEvents",
                           "plotRiskTableCumulativeNumberOfCensoredObservations", "plotRiskTableNumberOfEventsInTimeInterval",
                           "plotRiskTableNumberOfCensoredObservationsInTimeInterval", "plotRiskTableAsASingleLine",
                           "plotAddQuantile", "plotAddQuantileValue",
                           "colorPalette", "plotLegend", "plotTheme"))
    surivalPlot$position <- switch(
      type,
      "KM"  = 3,
      "Cox" = 7
    )
    jaspResults[["surivalPlot"]] <- surivalPlot
  }

  if (is.null(jaspResults[["fit"]]))
    return()

  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit))
    return()

  .ggsurvfit2JaspPlot <- function(x) {
    grDevices::png(f <- tempfile())
    on.exit({
      grDevices::dev.off()
      if (file.exists(f))
        file.remove(f)
    })
    return(ggsurvfit:::ggsurvfit_build(tempPlot))
  }

  if (type == "KM")
    tempPlot <- ggsurvfit::survfit2(.saGetFormula(options, type = type), data = dataset) |>
      ggsurvfit::ggsurvfit(
        type      = switch(
          options[["plotType"]],
          "survival"             = "survival",
          "risk"                 = "risk",
          "cumulativeHazard"     = "cumhaz",
          "complementaryLogLog"  = "cloglog"
        ),
        linewidth = 1
      )
  else if (type == "Cox")
    tempPlot <- ggsurvfit::survfit2(fit) |>
      ggsurvfit::ggsurvfit(
        type      = switch(
          options[["plotType"]],
          "survival"             = "survival",
          "risk"                 = "risk",
          "cumulativeHazard"     = "cumhaz",
          "complementaryLogLog"  = "cloglog"
        ),
        linewidth = 1
      )


  if (options[["plotConfidenceInterval"]])
    tempPlot <- tempPlot + ggsurvfit::add_confidence_interval()

  if (options[["plotRiskTable"]]) {
    riskTableStatistics <-  c(
      if (options[["plotRiskTableNumberAtRisk"]])                               "n.risk",
      if (options[["plotRiskTableCumulativeNumberOfObservedEvents"]])           "cum.event",
      if (options[["plotRiskTableCumulativeNumberOfCensoredObservations"]])     "cum.censor",
      if (options[["plotRiskTableNumberOfEventsInTimeInterval"]])               "n.event",
      if (options[["plotRiskTableNumberOfCensoredObservationsInTimeInterval"]]) "n.censor"
    )

    if (length(riskTableStatistics) > 0) {
      if (options[["plotRiskTableAsASingleLine"]])
        riskTableStatistics <- paste0("{", riskTableStatistics, "}", collapse = ", ")

      tempPlot <- tempPlot + ggsurvfit::add_risktable(risktable_stats = riskTableStatistics)
    }
  }

  if (options[["plotAddQuantile"]])
    tempPlot <- tempPlot + ggsurvfit::add_quantile(y_value = options[["plotAddQuantileValue"]], color = "gray50", linewidth = 0.75)

  if (options[["plotTheme"]] == "jasp")
    tempPlot <- tempPlot +
      jaspGraphs::geom_rangeframe(sides = "bl") +
      jaspGraphs::themeJaspRaw(legend.position = options[["plotLegend"]])
  else
    tempPlot <- tempPlot + ggplot2::theme(legend.position = options[["plotLegend"]])

  # scaling and formatting
  tempPlot <- tempPlot +
    jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])

  if (options[["plotType"]] == "complementaryLogLog")
    tempPlot <- tempPlot + ggplot2::scale_x_continuous(transform = "log") + ggplot2::xlab(gettext("log(Time)"))
  else
    tempPlot <- tempPlot + ggsurvfit::scale_ggsurvfit()

  if (jaspBase::isTryError(tempPlot)) {
    surivalCurvePlot$setError(tempPlot)
    return()
  }

  surivalPlot$plotObject <- .ggsurvfit2JaspPlot(tempPlot)

  return()
}
