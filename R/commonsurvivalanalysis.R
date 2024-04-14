# TODO:
# qml:
# - make the event indicator be the second lvl of the event status variable
# - make "lifeTableStepsTo" correspond to the maximum time
# - make "lifeTableStepsSize" correspond to the maxiumum time/10
# R:
# - convenient function for making factor level parameter names nice (in regression tables)?
# - why legend names are so wide? Encoding/decoding problems?
#   (smth along the lines of: `levels(tempPlot$data.survplot$strata) <- jaspBase::decodeColNames(levels(tempPlot$data.survplot$strata), strict = FALSE)`
# - make sure x-axis are aligned in the survival plot
# - jaspTheme the plot

.saSurvivalReady      <- function(options) {

  ready <- switch(
    options[["censoringType"]],
    "interval" = options[["eventStatus"]] != "" && options[["intervalStart"]] != "" && options[["intervalEnd"]] != "",
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
    "interval" = c(options[["intervalStart"]], options[["intervalEnd"]]),
    "right"    = options[["timeToEvent"]]
  )

  factorsVariable    <- Filter(function(s) s != "", options[["factors"]])
  covariatesVariable <- Filter(function(s) s != "", options[["covariates"]]) # only for (semi)parametric

  dataset <- .readDataSetToEnd(
    columns.as.numeric = c(timeVariable, covariatesVariable),
    columns.as.factor  = c(eventVariable, factorsVariable)
  )

  # clean from NAs
  dataset <- na.omit(dataset)

  # recode the event status variable
  dataset[[eventVariable]] <- .saRecodeEventStatus(dataset, options)

  # check of errors
  .hasErrors(
    dataset                      = dataset,
    type                         = c("negativeValues"),
    negativeValues.target        = timeVariable,
    exitAnalysisIfErrors         = TRUE
  )

  # check that interval start < end
  if (options[["censoringType"]] == "interval") {
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

    # 0 = right censored, 1 = event at time
    event <- as.numeric(dataset[[options[["eventStatus"]]]] == options[["eventIndicator"]])

  } else if (options[["censoringType"]] == "interval") {

    if (anyDuplicated(c(
      options[["rightCensored"]],
      options[["eventIndicator"]],
      options[["leftCensored"]],
      options[["intervalCensored"]]
    )))
      .quitAnalysis(gettextf("Duplicated level mapping for interval censoring."))

    # 0 = right censored, 1 = event at time, 2 = left censored, 3 = interval censored
    event <- rep(NA, nrow(dataset))
    event[dataset[[options[["eventStatus"]]]] == options[["rightCensored"]]]    <- 0
    event[dataset[[options[["eventStatus"]]]] == options[["eventIndicator"]]]   <- 1
    event[dataset[[options[["eventStatus"]]]] == options[["leftCensored"]]]     <- 2
    event[dataset[[options[["eventStatus"]]]] == options[["intervalCensored"]]] <- 3

  }

  return(event)
}
.saGetSurv            <- function(options) {
  return(switch(
    options[["censoringType"]],
    "interval" = sprintf("survival::Surv(
      time  = %1$s,
      time2 = %2$s,
      event = %3$s)",
      options[["intervalStart"]],
      options[["intervalEnd"]],
      options[["eventStatus"]]
      ),
    "right"    = sprintf("survival::Surv(
      time  = %1$s,
      event = %2$s)",
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
    predictors    <- .saGetPredictors(options, null = null)
    interceptTerm <- TRUE
  }

  survival <- .saGetSurv(options)

  if (length(predictors) == 0 && interceptTerm == FALSE)
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
