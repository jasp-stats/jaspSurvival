.saSurvivalReady      <- function(options) {

  ready <- switch(
    options[["censoringType"]],
    "left"     = options[["dataTimeStart"]] != "" && options[["dataTimeEnd"]] != "",
    "interval" = options[["dataTimeStart"]] != "" && options[["dataTimeEnd"]] != "",
    "right"    = options[["dataEvent"]] != "" && options[["dataTimeEnd"]] != ""
  )

  return(ready)
}
.saReadDataset        <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  ### load the data
  time                 <- Filter(function(s) s != "", c(options[["dataTimeStart"]], options[["dataTimeEnd"]]))
  event                <- if (options[["dataEvent"]] != "") options[["dataEvent"]]
  predictorsContinuous <- Filter(function(s) s != "", options[["predictorsContinuous"]])
  predictorsFactor     <- Filter(function(s) s != "", options[["predictorsFactors"]])

  dataset <- .readDataSetToEnd(
    columns.as.numeric = c(time, predictorsContinuous),
    columns.as.factor  = c(event, predictorsFactor)
  )

  ### check data set for errors
  # check that start < end
  if (options[["censoringType"]] %in% c("left", "interval")) {

    test <- dataset[, options[["dataTimeStart"]]] > dataset[, options[["dataTimeEnd"]]]
    test <- test[!is.na(test)]

    if (any(test))
      .quitAnalysis(gettextf("The end time must be larger than start time."))
  }

  .hasErrors(
    dataset                      = dataset,
    type                         = c("negativeValues"),
    negativeValues.target        = time,
    exitAnalysisIfErrors         = TRUE
  )
  .hasErrors(
    dataset                      = dataset,
    type                         = c("infinity", "observations", "variance", "varCovData"),
    all.target                   = predictorsContinuous,
    varCovData.corFun            = stats::cov,
    observations.amount          = "< 2",
    exitAnalysisIfErrors         = TRUE
  )
  .hasErrors(
    dataset                      = dataset,
    type                         = c("modelInteractions"),
    modelInteractions.modelTerms = options[["modelTerms"]],
    exitAnalysisIfErrors         = TRUE
  )

  return(dataset)
}
.saGetSurvivalFormula <- function(options) {



}
.saGetFormula         <- function(survival, predictors = NULL, includeConstant) {

  if (is.null(predictors) && includeConstant == FALSE)
    stop(gettext("We need at least one predictor, or an intercept to make a formula"))

  if (is.null(predictors))
    formula <- paste(survival, "~", "1")
  else if (includeConstant)
    formula <- paste(survival, "~", paste(predictors, collapse = "+"))
  else
    formula <- paste(survival, "~", paste(predictors, collapse = "+"), "-1")

  return(as.formula(formula, env = parent.frame(1)))
}
