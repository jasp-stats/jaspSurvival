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


.saGetSurvIntervalStatus <- function(dataset, options) {

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

  return(event)
}
.saAddSurvData           <- function(dataset, options) {

  if (options[["censoringType"]] == "right") {

    dataset$JASP__Computed__Time  <- dataset[[options[["timeToEvent"]]]]
    dataset$JASP__Computed__Event <- dataset[[options[["eventStatus"]]]] == options[["eventIndicator"]]

  } else if (options[["censoringType"]] == "interval") {

    dataset$JASP__Computed__Time  <-  dataset[[options[["intervalStart"]]]]
    dataset$JASP__Computed__Time2 <-  dataset[[options[["intervalEnd"]]]]
    dataset$JASP__Computed__Event <-  .saGetSurvIntervalStatus(dataset, options)
  }

  return(dataset)
}
.saGetSurv               <- function(options) {
  return(switch(
    options[["censoringType"]],
    "interval" = "survival::Surv(
      time  = JASP__Computed__Time,
      time2 = JASP__Computed__Time2,
      event = JASP__Computed__Event)",
    "right"    = "survival::Surv(
      time  = JASP__Computed__Time,
      event = JASP__Computed__Event)"
  ))
}
.saGetFormula            <- function(survival, predictors = NULL, includeConstant = TRUE) {

  if (is.null(predictors) && includeConstant == FALSE)
    stop(gettext("We need at least one predictor, or an intercept to make a formula"))

  if (length(predictors) == 0)
    formula <- paste(survival, "~", "1")
  else if (includeConstant)
    formula <- paste(survival, "~", paste(predictors, collapse = "+"))
  else
    formula <- paste(survival, "~", paste(predictors, collapse = "+"), "-1")

  return(as.formula(formula, env = parent.frame(1)))
}
