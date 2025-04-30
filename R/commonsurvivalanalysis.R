# TODO:
# qml:
# - make "lifeTableStepsTo" correspond to the maximum time
# - make "lifeTableStepsSize" correspond to the maxiumum time/10

.saSurvivalReady      <- function(options) {

  ready <- switch(
    options[["censoringType"]],
    "counting" = options[["eventStatus"]]   != "" && options[["intervalStart"]] != "" && options[["intervalEnd"]] != "",
    "interval" = options[["intervalStart"]] != "" && options[["intervalEnd"]] != "",
    "right"    = options[["eventStatus"]]   != "" && options[["timeToEvent"]] != "",
    "left"     = options[["eventStatus"]]   != "" && options[["timeToEvent"]] != ""
  )

  # check whether Cox regression is waiting for frailty
  coxWaitingForFrailty <- .saspCoxWaitingForFrailty(options)

  ready <-  ready && !coxWaitingForFrailty

  return(ready)
}
.saCheckDataset       <- function(dataset, options, type) {

  # load the data
  eventVariable <- if (options[["censoringType"]] != "interval") options[["eventStatus"]]
  timeVariable  <- switch(
    options[["censoringType"]],
    "counting" = c(options[["intervalStart"]], options[["intervalEnd"]]),
    "interval" = c(options[["intervalStart"]], options[["intervalEnd"]]),
    "right"    = options[["timeToEvent"]],
    "left"     = options[["timeToEvent"]]
  )

  strataVariable     <- Filter(function(s) s != "", options[["strata"]])
  weightsVariable    <- if (options[["weights"]] != "") options[["weights"]]
  # only for (semi)parametric
  covariatesVariable <- Filter(function(s) s != "", options[["covariates"]])
  factorsVariable    <- Filter(function(s) s != "", options[["factors"]])
  # only for parametric
  subgroupVariable   <- if (!is.null(options[["subgroup"]]) && options[["subgroup"]] != "") options[["subgroup"]]

  # clean from NAs
  if (options[["censoringType"]] == "interval") {
    # !!! interval data use NA's in the interval indication !!!
    keep <- stats::complete.cases(dataset[setdiff(colnames(dataset), timeVariable)])
    dataset <- dataset[keep, ]
    dataset <- droplevels(dataset)

    # recode the time interval
    dataset[[options[["intervalStart"]]]][is.nan(dataset[[options[["intervalStart"]]]]) | is.infinite(dataset[[options[["intervalStart"]]]])] <- NA
    dataset[[options[["intervalEnd"]]]][is.nan(dataset[[options[["intervalEnd"]]]])     | is.infinite(dataset[[options[["intervalEnd"]]]])] <- NA
  } else {
    dataset <- na.omit(dataset)
    dataset <- droplevels(dataset)

    # recode the event status variable
    dataset[[eventVariable]] <- .saRecodeEventStatus(dataset, options)
  }

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
      .quitAnalysis(gettextf("The end time must be larger than the start time."))
  }

  if (!is.null(covariatesVariable) && length(covariatesVariable) > 0)
    .hasErrors(
      dataset                      = dataset,
      type                         = c("infinity", "observations", "variance", "varCovData"),
      all.target                   = covariatesVariable,
      varCovData.corFun            = stats::cov,
      observations.amount          = "< 2",
      exitAnalysisIfErrors         = TRUE
    )

  if (!is.null(subgroupVariable))
    .hasErrors(
      dataset                      = dataset,
      type                         = c("observations", "variance"),
      all.target                   = subgroupVariable,
      observations.amount          = "< 2",
      exitAnalysisIfErrors         = TRUE
    )

  if (!is.null(options[["modelTerms"]]) && type == "Cox")
    .hasErrors(
      dataset                      = dataset,
      type                         = c("modelInteractions"),
      modelInteractions.modelTerms = options[["modelTerms"]],
      exitAnalysisIfErrors         = TRUE
    )

  if (!is.null(weightsVariable)) {
    if (!all(isWholenumber(dataset[[weightsVariable]])))
      .quitAnalysis(gettextf("The weights variable must be an integer."))
    if (anyNA(dataset[[weightsVariable]]))
      .quitAnalysis(gettextf("The weights variable must not contain missing values."))
    if (any(dataset[[weightsVariable]] < 1))
      .quitAnalysis(gettextf("The weights variable must be positive."))
  }

  return(dataset)
}
.saRecodeEventStatus  <- function(dataset, options) {

  event <- dataset[[options[["eventStatus"]]]] == options[["eventIndicator"]]

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
    "interval" = sprintf("Surv(
      time  = %1$s,
      time2 = %2$s,
      type  = 'interval2')",
      options[["intervalStart"]],
      options[["intervalEnd"]]
      ),
    "right"    = sprintf("Surv(
      time  = %1$s,
      event = %2$s,
      type  = 'right')",
      options[["timeToEvent"]],
      options[["eventStatus"]]
      ),
    "left"    = sprintf("Surv(
      time  = %1$s,
      event = %2$s,
      type  = 'left')",
      options[["timeToEvent"]],
      options[["eventStatus"]]
      )
  ))
}
.saExtractSurvTimes   <- function(dataset, options) {

  if (options[["censoringType"]] == "counting") {
    times <- c(dataset[[options[["intervalStart"]]]], dataset[[options[["intervalEnd"]]]])
  } else if (options[["censoringType"]] == "interval") {
    times <- na.omit(c(dataset[[options[["intervalStart"]]]], dataset[[options[["intervalEnd"]]]]))
  } else if (options[["censoringType"]] == "right") {
    times <- dataset[[options[["timeToEvent"]]]]
  } else if (options[["censoringType"]] == "left") {
    times <- dataset[[options[["timeToEvent"]]]]
  }

  return(times)
}
.saGetFormula         <- function(options, type, null = FALSE) {

  if (type == "KM") {
    # nonparametric (Kaplan-Meier) only stratifies by strata
    predictors    <- options[["strata"]]
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
.sapGetFormula        <- function(options, modelTerms) {

  predictors    <- .sapGetPredictors(modelTerms)
  interceptTerm <- options[["includeIntercept"]]

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
.sapGetPredictors     <- function(modelTerms) {

  t <- NULL
  for (i in seq_along(modelTerms[["components"]])) {
    term <- modelTerms[["components"]][[i]]
    if (length(term) == 1)
      t <- c(t, term)
    else
      t <- c(t, paste(unlist(term), collapse = ":"))
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
    # simplifying GUI for fixed (instead of having fixed.theta and fixed.df)
    if (options[["frailtyMethod"]] == "fixed" && options[["frailtyMethodFixed"]] == "theta")  "fixed"
    else if (options[["frailtyMethod"]] == "fixed" && options[["frailtyMethodFixed"]] == "df") "df"
    else options[["frailtyMethod"]],
    # adding the fixed part
    if (options[["frailtyMethod"]] != "fixed") ""
    else if (options[["frailtyMethodFixed"]] == "df")    paste0(", df = ",    options[["frailtyMethodFixedDf"]])
    else if (options[["frailtyMethodFixed"]] == "theta") paste0(", theta = ", options[["frailtyMethodFixedTheta"]]),
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

  # deal with non-standard columns names
  if (!grepl("JaspColumn", varName)) {
    if (varName == "(Intercept)")
      return("Intercept")
    if (varName == "Global")
      return("Global")
    if (grepl("gamma:", varName, fixed = TRUE) || grepl("gauss:", varName, fixed = TRUE) || grepl("t:", varName, fixed = TRUE))
      return(paste0("(frailty) ", varName))
  }

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
    surivalPlot$dependOn(c(.sanpDependencies, "plot", "plotType", "plotStrata", "plotCi", "plotRiskTable",
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
    tempPlot <- try(ggsurvfit::ggsurvfit(
      x    = ggsurvfit::survfit2(.saGetFormula(options, type = type), data = dataset),
      type = switch(
        options[["plotType"]],
        "survival"             = "survival",
        "risk"                 = "risk",
        "cumulativeHazard"     = "cumhaz",
        "complementaryLogLog"  = "cloglog"
      ),
      linewidth = 1
    ))
  else if (type == "Cox")
    tempPlot <- try(ggsurvfit::ggsurvfit(
      x    = ggsurvfit::survfit2(fit),
      type = switch(
        options[["plotType"]],
        "survival"             = "survival",
        "risk"                 = "risk",
        "cumulativeHazard"     = "cumhaz",
        "complementaryLogLog"  = "cloglog"
      ),
      linewidth = 1
    ))

  if (jaspBase::isTryError(tempPlot)) {
    surivalPlot$setError(tempPlot)
    return()
  }

  if (options[["plotCi"]])
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

.saSafeRbind     <- function(dfs) {

  # this function allows combining data.frames with different columns
  # the main issue is that some models might be missing coefficients/terms,
  # or complete fit failure, as such, simple rbind might misaligned the grouped output
  # importantly, the order of the output data.frame
  # does not matter as order is determined by the table itself

  dfs <- dfs[!sapply(dfs, function(x) is.null(x) || length(x) == 0 || nrow(x) == 0)]
  if (length(dfs) == 0)
    return(NULL)

  # gather all colnames
  colnamesUnique <- unique(unlist(lapply(dfs, colnames)))

  # add missing columns and reorder
  for (i in seq_along(dfs)) {
    colnamesMissing <- setdiff(colnamesUnique, colnames(dfs[[i]]))
    if (length(colnamesMissing) > 0) {
      for (col in colnamesMissing) {
        dfs[[i]][[col]] <- NA
      }
    }
    dfs[[i]] <- dfs[[i]][,colnamesUnique,drop=FALSE]
  }

  df <- do.call(rbind, dfs)
  return(df)
}
.saSafeSimplify  <- function(df) {

  # transform into factors to keep the order during splitting
  df[["model"]]        <- factor(df[["model"]],        levels = unique(df[["model"]]))
  df[["distribution"]] <- factor(df[["distribution"]], levels = unique(df[["distribution"]]))
  df[["subgroup"]]     <- factor(df[["subgroup"]],     levels = unique(df[["subgroup"]]))

  # simplifying output tables
  df <- do.call(rbind, lapply(split(df, df[["subgroup"]]), function(x) {
    x <- do.call(rbind, lapply(split(x, x[["distribution"]]), function(xx) {
      xx[["model"]][duplicated(xx[["model"]])] <- NA
      return(xx)
    }))
    x[["distribution"]][duplicated(x[["distribution"]])] <- NA
    return(x)
  }))
  df[["subgroup"]][duplicated(df[["subgroup"]])] <- NA

  # transform back to character
  df[["model"]]        <- as.character(df[["model"]])
  df[["distribution"]] <- as.character(df[["distribution"]])
  df[["subgroup"]]     <- as.character(df[["subgroup"]])

  return(df)
}
isWholenumber            <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
