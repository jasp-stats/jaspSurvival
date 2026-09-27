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

# summary tables
.sapSummaryTable                      <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "all")

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "alwaysDisplayModelInformation",
                          "modelSummary",
                          "modelSummaryRankModels", "modelSummaryRankModelsBy",
                          "modelSummaryAicWeighs", "modelSummaryBicWeighs")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSummaryTableFun,
    name          = "summaryTable",
    title         = gettext("Model Summary"),
    dependencies  = outputDependencies,
    position      = 1
  )

  return()
}
.sapSequentialModelComparisonTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["sequentialModelComparisonTable"]]) || length(options[["modelTerms"]]) < 2)
    return()

  # the extract function automatically groups models by subgroup / distribution
  fit <- .sapExtractFit(jaspResults, options, type = "byDistribution")

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "alwaysDisplayModelInformation",
                          "sequentialModelComparison")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSequentialModelComparisonTableFun,
    name          = "sequentialModelComparisonTable",
    title         = gettext("Sequential Model Comparison"),
    dependencies  = outputDependencies,
    position      = 1.1
  )

  return()
}
.sapCoefficientsTable                 <- function(jaspResults, options) {

  if (!is.null(jaspResults[["coefficientsTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "coefficients", "coefficientsConfidenceInterval", "coefficientsConfidenceIntervalLevel")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapCoefficientsTableFun,
    name          = "coefficientsTable",
    title         = gettext("Coefficients Summary"),
    dependencies  = outputDependencies,
    position      = 2
  )

  return()
}
.sapCoefficientsCovarianceMatrixTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["coefficientsCovarianceMatrixTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  # flatten the list - each model has to get its own matrix because they might differ in parameters etc...
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "coefficientsCovarianceMatrix")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapCoefficientsCovarianceMatrixTableFun,
    name          = "coefficientsCovarianceMatrixTable",
    title         = gettext("Coefficients Covariance Matrix"),
    dependencies  = outputDependencies,
    position      = 2.1
  )

  return()
}

.sapSummaryTableFun                      <- function(fit, options) {

  # create the table
  summaryTable <- createJaspTable()
  .sapAddColumnSubgroup(     summaryTable, options, output = "modelSummary")
  .sapAddColumnDistribution( summaryTable, options, output = "modelSummary")
  .sapAddColumnComponents(   summaryTable, options, output = "modelSummary")
  .sapAddColumnModel(        summaryTable, options, output = "modelSummary")
  summaryTable$addColumnInfo(name = "logLik",        title = gettext("Log Lik."),     type = "number")
  summaryTable$addColumnInfo(name = "df",            title = gettext("df"),           type = "integer")
  summaryTable$addColumnInfo(name = "aic",           title = gettext("AIC"),          type = "number", format="dp:3")
  summaryTable$addColumnInfo(name = "bic",           title = gettext("BIC"),          type = "number", format="dp:3")
  if (options[["modelSummaryAicWeighs"]] && length(fit) > 1)
    summaryTable$addColumnInfo(name = "aicWeight",     title = gettext("AIC Weight"),   type = "number", format="dp:3")
  if (options[["modelSummaryBicWeighs"]] && length(fit) > 1)
    summaryTable$addColumnInfo(name = "bicWeight",     title = gettext("BIC Weight"),   type = "number", format="dp:3")
  if (options[["modelSummaryRankModels"]] && length(fit) > 1)
    summaryTable$addColumnInfo(name = "rank",        title = gettext("Rank"),         type = "integer")

  if (!.saSurvivalReady(options))
    return(summaryTable)

  # extract the data
  data <- .saSafeRbind(lapply(fit, .sapRowSummaryTable))

  # add information criteria weights
  if (options[["modelSummaryAicWeighs"]] && length(fit) > 1 && !is.null(data[[options[["modelSummaryRankModelsBy"]]]])) {
    data$aicWeight <- .sapInformationCriteria2Weights(data[["aic"]])
  }
  if (options[["modelSummaryBicWeighs"]] && length(fit) > 1 && !is.null(data[[options[["modelSummaryRankModelsBy"]]]])) {
    data$bicWeight <- .sapInformationCriteria2Weights(data[["bic"]])
  }

  # add model rank
  if (options[["modelSummaryRankModels"]] && length(fit) > 1 && !is.null(data[[options[["modelSummaryRankModelsBy"]]]])) {
    data <- data[order(data[[options[["modelSummaryRankModelsBy"]]]], decreasing = options[["modelSummaryRankModelsBy"]] == "logLik", na.last = TRUE), ]
    data$rank <- seq_len(nrow(data))
    data$rank[is.na(data$rank)] <- NA
  }

  # add footnotes
  messages <- .sapSelectionFootnote(data, options)
  for (i in seq_along(messages))
    summaryTable$addFootnote(messages[[i]])

  errors <- .sapCollectFitErrors(fit, options)
  for (i in seq_along(errors))
    summaryTable$addFootnote(errors[[i]], symbol = gettext("Error: "))

  if (options[["analysisType"]] == "mixture") {
    mixtureMessages <- .sapmSummaryMessages(fit, options)
    for (i in seq_along(mixtureMessages[["notes"]]))
      summaryTable$addFootnote(mixtureMessages[["notes"]][[i]])
    for (i in seq_along(mixtureMessages[["warnings"]]))
      summaryTable$addFootnote(mixtureMessages[["warnings"]][[i]], symbol = gettext("Warning:"))
  }

  if (length(fit) > 0)
    .saAddMissingObservationsFootnote(summaryTable, attr(fit[[1]], "dataset", exact = TRUE))

  summaryTable$setData(data)
  summaryTable$showSpecifiedColumnsOnly <- TRUE

  return(summaryTable)
}
.sapSequentialModelComparisonTableFun    <- function(fit, options) {

  # create the table
  sequentialModelComparisonTable <- createJaspTable()
  .sapAddColumnSubgroup(     sequentialModelComparisonTable, options, output = "coefficients")
  .sapAddColumnDistribution( sequentialModelComparisonTable, options, output = "coefficients")
  .sapAddColumnComponents(   sequentialModelComparisonTable, options, output = "coefficients")
  sequentialModelComparisonTable$addColumnInfo(name = "model0", title = "H\U2080",      type = "string")
  sequentialModelComparisonTable$addColumnInfo(name = "model1", title = "H\U2081",      type = "string")
  sequentialModelComparisonTable$addColumnInfo(name = "chi2",   title = "\U03C7\U00B2", type = "number")
  sequentialModelComparisonTable$addColumnInfo(name = "df",     title = gettext("df"),           type = "integer")
  sequentialModelComparisonTable$addColumnInfo(name = "pValue", title = gettext("p"),            type = "pvalue")

  if (!.saSurvivalReady(options))
    return(sequentialModelComparisonTable)

  data <- list()
  for(i in 1:(length(fit) - 1)) {
    # models are compared only within the same distribution
    if (attr(fit[[i]], "family") != attr(fit[[i + 1]], "family") || attr(fit[[i]], "components") != attr(fit[[i + 1]], "components"))
      next
    data[[length(data) + 1]] <- .sapRowSequentialModelComparisonTable(fit[[i]], fit[[i + 1]])
  }
  data <- .saSafeRbind(data)

  # add footnotes
  sequentialModelComparisonTable$addFootnote(gettextf("Likelihood ratio test for nested models based on %s distribution.", "\U03C7\U00B2"))

  sequentialModelComparisonTable$setData(data)
  sequentialModelComparisonTable$showSpecifiedColumnsOnly <- TRUE

  return(sequentialModelComparisonTable)
}
.sapCoefficientsTableFun                 <- function(fit, options) {

  # create the table
  estimatesTable <- createJaspTable()
  .sapAddColumnSubgroup(     estimatesTable, options, output = "coefficients")
  .sapAddColumnDistribution( estimatesTable, options, output = "coefficients")
  .sapAddColumnComponents(   estimatesTable, options, output = "coefficients")
  .sapAddColumnModel(        estimatesTable, options, output = "coefficients")
  estimatesTable$addColumnInfo(name = "coefficient",    title = "",                         type = "string")
  estimatesTable$addColumnInfo(name = "est",            title = gettext("Estimate"),        type = "number")
  estimatesTable$addColumnInfo(name = "se",             title = gettext("Standard Error"),  type = "number")
  if (options[["coefficientsConfidenceInterval"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["coefficientsConfidenceIntervalLevel"]])
    estimatesTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    estimatesTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  if (!.saSurvivalReady(options))
    return(estimatesTable)

  # check whether any predictors are present
  anyRegression <- any(sapply(fit, function(x) {
    if (jaspBase::isTryError(x))
      return(FALSE)
    else
      return(length(attr(x, "modelTerms")[["components"]]) > 0)
  }))
  if (anyRegression) {
    estimatesTable$addColumnInfo(name = "z",              title = gettext("z"),               type = "number")
    estimatesTable$addColumnInfo(name = "pValue",         title = gettext("p"),               type = "pvalue")
  }

  # extract the data
  data <- .saSafeRbind(lapply(fit, .sapRowCoefficientsTable))
  data <- .saSafeSimplify(data)

  # add test statistics and p-values
  if (anyRegression) {

    # add z-values and p-values
    thisRegression <- data[["isRegressionCoefficient"]]
    thisRegression[is.na(thisRegression)] <- FALSE
    data$z      <- NA_real_
    data$pValue <- NA_real_
    if (any(thisRegression)) {
      data$z[thisRegression]      <- data$est[thisRegression] / data$se[thisRegression]
      data$pValue[thisRegression] <- 2 * pnorm(-abs(data$z[thisRegression]))
    }
    estimatesTable$addFootnote(gettext("P-values are based on a Wald test."))

    # fix coefficient names
    if (any(thisRegression))
      data[["coefficient"]][thisRegression] <- sapply(data[["coefficient"]][thisRegression], .saTermNames, variables = c(options[["covariates"]], options[["factors"]]))

    # add the mixture component of the regression coefficients
    if (!is.null(data[["mixtureComponent"]])) {
      thisMixtureRegression <- thisRegression & !is.na(data[["mixtureComponent"]])
      data[["coefficient"]][thisMixtureRegression] <- gettextf("%1$s (component %2$i)", data[["coefficient"]][thisMixtureRegression], data[["mixtureComponent"]][thisMixtureRegression])
    }
  }

  data[["isRegressionCoefficient"]] <- NULL
  data[["mixtureComponent"]]        <- NULL

  # add footnotes
  messages <- .sapSelectedModelMessage(fit, options)
  for (i in seq_along(messages))
    estimatesTable$addFootnote(messages[[i]])

  if (options[["analysisType"]] == "mixture") {
    mixtureWarnings <- .sapmSummaryMessages(fit, options)[["warnings"]]
    for (i in seq_along(mixtureWarnings))
      estimatesTable$addFootnote(mixtureWarnings[[i]], symbol = gettext("Warning:"))
  }

  if (any(sapply(fit, function(x) !jaspBase::isTryError(x) && attr(x, "components") > 1))) {
    estimatesTable$addFootnote(gettext("The standard errors and confidence intervals of the mixing probabilities are based on the delta method."))
    if (anyRegression)
      estimatesTable$addFootnote(gettext("The component parameters correspond to the reference level of factors and zero value of covariates."))
  }

  estimatesTable$setData(data)
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  return(estimatesTable)
}
.sapCoefficientsCovarianceMatrixTableFun <- function(fit, options) {

  # create the table
  covarianceMatrixTableTable <- createJaspTable()
  .sapAddColumnSubgroup(     covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  covarianceMatrixTableTable$addColumnInfo(name = "coefficient",    title = "", type = "string")

  if (!.saSurvivalReady(options))
    return(covarianceMatrixTableTable)

  # extract the data
  data <- .sapRowcovarianceMatrixTableTable(fit)
  data <- .saSafeSimplify(data)

  if (jaspBase::isTryError(fit))
    return(covarianceMatrixTableTable)

  # add columns for each parameter
  for (i in 1:nrow(data)) {
    covarianceMatrixTableTable$addColumnInfo(name = data[["coefficient"]][i], title = data[["coefficient"]][i], type = "number")
  }

  # add footnotes
  if (!is.null(attr(fit, "label")))
    covarianceMatrixTableTable$addFootnote(attr(fit, "label"))

  covarianceMatrixTableTable$setData(data)
  covarianceMatrixTableTable$showSpecifiedColumnsOnly <- TRUE

  return(covarianceMatrixTableTable)
}

# adding rows to the output
.sapRowModelInformation               <- function(fit) {
  return(data.frame(
    subgroup     = attr(fit, "subgroup"),
    model        = attr(fit, "modelTitle"),
    distribution = attr(fit, "distribution"),
    components   = attr(fit, "components")
  ))
}
.sapRowSummaryTable                   <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  return(data.frame(
    .sapRowModelInformation(fit),
    logLik = as.numeric(logLik(fit)),
    df     = attr(logLik(fit), "df"),
    aic    = AIC(fit),
    bic    = BIC(fit)
  ))
}
.sapRowSequentialModelComparisonTable <- function(fit0, fit1) {

  if (jaspBase::isTryError(fit0) || jaspBase::isTryError(fit1))
    return(data.frame(
      subgroup     = attr(fit0, "subgroup"),
      model0       = attr(fit0, "modelTitle"),
      model1       = attr(fit1, "modelTitle"),
      distribution = attr(fit0, "distribution"),
      components   = attr(fit0, "components")
    ))

  # flexsurv did not implement anova, so we compute the LRT manually
  ll0 <- fit0$loglik
  ll1 <- fit1$loglik

  chi2   <- 2 * (ll1 - ll0)
  df     <- fit1$npars - fit0$npars
  pValue <- pchisq(chi2, df = df, lower.tail = FALSE)

  return(data.frame(
    subgroup     = attr(fit0, "subgroup"),
    model0       = attr(fit0, "modelTitle"),
    model1       = attr(fit1, "modelTitle"),
    distribution = attr(fit0, "distribution"),
    components   = attr(fit0, "components"),
    chi2         = chi2,
    df           = df,
    pValue       = pValue
  ))
}
.sapRowCoefficientsTable              <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  coeffTable <- data.frame(
    .sapRowModelInformation(fit),
    coefficient  = rownames(fit[["res"]]),
    fit[["res"]]
  )

  # rename the CI columns
  colnames(coeffTable)[(ncol(coeffTable) - 2):(ncol(coeffTable)-1)] <- c("lower", "upper")
  coeffTable[["isRegressionCoefficient"]] <- seq_len(nrow(fit[["res"]])) %in% fit[["covpars"]]

  # label the parameters of mixture components
  if (!is.null(attr(fit, "mixture")))
    coeffTable <- .sapmCoefficientsNames(coeffTable, fit)

  return(coeffTable)
}
.sapRowcovarianceMatrixTableTable     <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  # one has recreate the matrix and use the names from the coefficients from the res table because
  # the the covariance matrix drops names if there is only a single parameter
  covMat <- data.frame(fit[["cov"]])
  colnames(covMat) <- rownames(fit[["res"]]) -> rownames(covMat)

  return(data.frame(
    .sapRowModelInformation(fit),
    coefficient  = rownames(fit[["res"]]),
    covMat,
    check.names  = FALSE
  ))
}

# adding columns to tables
.sapAddColumnSubgroup     <- function(tempTable, options, output) {

  if (output %in% c("modelSummary", "coefficients")) {
    if (options[["subgroup"]] != "" && !.sapMultipleFamilies(options) && !.sapMultipleComponents(options) && !.sapMultipleModels(options))
      tempTable$addColumnInfo(name = "subgroup", title = gettext("Subgroup"), type = "string")
    return()
  }

  if (output == "coefficientsCovarianceMatrix" && options[["subgroup"]] != "" && options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "subgroup", title = gettext("Subgroup"), type = "string")
    return()
  }
}
.sapAddColumnModel        <- function(tempTable, options, output) {

  if(options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
    return()
  }

  if (output == "modelSummary" && .sapMultipleModels(options)) {
    tempTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
    return()
  }

  if (output == "coefficients" && .sapMultipleModels(options) && options[["interpretModel"]] == "all") {
    tempTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
    return()
  }

  if (output == "coefficientsCovarianceMatrix" && options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
    return()
  }
}
.sapAddColumnComponents   <- function(tempTable, options, output) {

  # the number of components is displayed only in the mixture analysis
  if (options[["analysisType"]] != "mixture")
    return()

  if(options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "components", title = gettext("Components"), type = "integer")
    return()
  }

  if (output %in% c("modelSummary", "coefficients") && .sapMultipleComponents(options)) {
    tempTable$addColumnInfo(name = "components", title = gettext("Components"), type = "integer")
    return()
  }
}
.sapAddColumnDistribution <- function(tempTable, options, output) {

  if(options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }

  if (output == "modelSummary" && .sapMultipleFamilies(options)) {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }

  if (output == "coefficients" && .sapFamilySelection(options) == "all") {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }

  if (output == "coefficientsCovarianceMatrix" && options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }
}
.sapCreatePredictionTable     <- function(options, atTitle, estimateNames, estimateTitles) {

  tempTable <- createJaspTable()
  .sapAddColumnSubgroup(     tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        tempTable, options, output = "coefficientsCovarianceMatrix")
  tempTable$addColumnInfo(name = "at", title = atTitle, type = "number")

  for (i in seq_along(estimateNames)) {
    .sapAddColumnsPredictionTable(
      tempTable     = tempTable,
      options       = options,
      estimateName  = estimateNames[i],
      estimateTitle = estimateTitles[i],
      ciOvertitle   = length(estimateNames) == 1
    )
  }


  return(tempTable)
}
.sapAddColumnsPredictionTable <- function(tempTable, options, estimateTitle, estimateName = "", ciOvertitle = TRUE) {

  if (isTRUE(ciOvertitle)) {

    tempTable$addColumnInfo(name = paste0(estimateName, "estimate"), title = estimateTitle, type = "number")

    if (options[["predictionsConfidenceInterval"]]) {
      ciOvertitle <- gettextf("%s%% CI", 100 * options[["predictionsConfidenceIntervalLevel"]])
      tempTable$addColumnInfo(name = paste0(estimateName, "lCi"), title = gettext("Lower"), type = "number", overtitle = ciOvertitle)
      tempTable$addColumnInfo(name = paste0(estimateName, "uCi"), title = gettext("Upper"), type = "number", overtitle = ciOvertitle)
    }
  } else {

    tempTable$addColumnInfo(name = paste0(estimateName, "estimate"), title = gettext("Estimate"), type = "number", overtitle = estimateTitle)

    if (options[["predictionsConfidenceInterval"]]) {
      overtitleCi <- gettextf("%s%% CI", 100 * options[["predictionsConfidenceIntervalLevel"]])
      tempTable$addColumnInfo(name = paste0(estimateName, "lCi"), title = gettext("Lower CI"), type = "number", overtitle = estimateTitle)
      tempTable$addColumnInfo(name = paste0(estimateName, "uCi"), title = gettext("Upper CI"), type = "number", overtitle = estimateTitle)
    }

  }



  return()
}

# table messages
.sapCollectFitErrors      <- function(fit, options) {

  errors <- NULL

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]) && options[["analysisType"]] == "mixture")
      errors <- c(errors, gettextf(
        "%1$s failed with the following message: %2$s",
        .sapmCellLabel(fit[[i]], options),
        .sapmCleanError(fit[[i]])
      ))
    else if (jaspBase::isTryError(fit[[i]]))
      errors <- c(errors, gettextf(
        "%1$s model %2$s%3$s failed with the following message: %4$s.",
        distribution = attr(fit[[i]], "distribution"),
        model        = attr(fit[[i]], "modelTitle"),
        subgroup     = if (options[["subgroup"]] != "") paste0(" (", attr(fit[[i]], "subgroupLabel"), ")") else "",
        error        = fit[[i]]
      ))
  }

  return(errors)
}
.sapSelectionFootnote     <- function(data, options) {

  if (.sapMultipleComponents(options) && .sapComponentSelection(options) %in% c("bestAic", "bestBic") && !options[["interpretModel"]] %in% c("bestAic", "bestBic") && (options[["compareModelsAcrossComponents"]] || !.sapMultipleModels(options))) {

    if (.sapFamilySelection(options) %in% c("bestAic", "bestBic") && (options[["compareModelsAcrossDistributions"]] || !.sapMultipleModels(options))) {
      selectedDistribution <- data[["distribution"]][which.min(data[[.sapSelectionCriterion(.sapFamilySelection(options))]])]
      distributionData     <- data[data[["distribution"]] %in% selectedDistribution, , drop = FALSE]
      selected             <- which.min(distributionData[[.sapSelectionCriterion(.sapComponentSelection(options))]])
      message              <- gettextf("All following output is based on the best fitting %1$s distribution with %2$s.", distributionData[["distribution"]][selected], .sapComponentsLabel(distributionData[["components"]][selected]))
    } else if (!.sapMultipleFamilies(options)) {
      selected <- which.min(data[[.sapSelectionCriterion(.sapComponentSelection(options))]])
      message  <- gettextf("All following output is based on the best fitting number of components (%1$s).", .sapComponentsLabel(data[["components"]][selected]))
    } else {
      message  <- gettext("All following output is based on the best fitting number of components within each distribution.")
    }

  } else if (.sapFamilySelection(options) %in% c("bestAic", "bestBic") && !options[["interpretModel"]] %in% c("bestAic", "bestBic") && (options[["compareModelsAcrossDistributions"]] || !.sapMultipleModels(options))) {

    selected <- which.min(data[[.sapSelectionCriterion(.sapFamilySelection(options))]])
    message <- gettextf("All following output is based on the best fitting %1$s distribution.", data[["distribution"]][selected])

  } else {

    message <- NULL

  }

  return(message)

}
.sapSelectedModelMessage  <- function(fit, options) {

  message <- NULL

  # messages for the selection of the number of components
  if (.sapMultipleComponents(options) && .sapComponentSelection(options) %in% c("bestAic", "bestBic"))
    return(.sapSelectedComponentsMessage(fit, options))

  # check whether selection rules were applied
  multipleModels        <- .sapMultipleModels(options)
  multipleDistributions <- .sapMultipleFamilies(options)

  selectModels        <- multipleModels        && options[["interpretModel"]] != "all"
  selectDistributions <- multipleDistributions && .sapFamilySelection(options) %in% c("bestAic", "bestBic")

  if (!multipleModels) {
    # only a single model is specified

    if (selectDistributions) {
      message <- gettextf("Results are based on %1$s distribution which was the best fitting distribution.", attr(fit[[1]], "distribution"))
    } else {
      message <- NULL
    }

  } else {
    # multiple models are specified

    if (!options[["interpretModel"]] %in% c("all", "bestAic", "bestBic") && !selectDistributions) {
      # hand chosen model for all distributions is shown
      message <- gettextf("Results are based on %1$s.", attr(fit[[1]], "modelTitle"))
    } else if (!options[["interpretModel"]] %in% c("all", "bestAic", "bestBic") && selectDistributions) {
      # hand chosen model for the best distribution is shown
      message <- gettextf("Results are based on %1$s with distribution %2$s which is the best fitting distribution across models.", attr(fit[[1]], "modelTitle"), attr(fit[[1]], "distribution"))
    } else if (!selectModels && !selectDistributions) {
      # all models for all distributions are shown
      message <- NULL
    } else if (!selectModels && selectDistributions) {
      # all models for the best distribution are shown
      message <- gettextf("Results are based on %1$s distribution which was the best fitting distribution.", attr(fit[[1]], "distribution"))
    } else if (selectModels && !selectDistributions && .sapFamilySelection(options) != "all") {
      # best fitting model for the selected distribution is shown
      message <- gettext("Results are based on best fitting models.")
    }  else if (selectModels && !selectDistributions && .sapFamilySelection(options) == "all") {
      # best fitting model for all distributions is shown
      message <- gettext("Results are based on best fitting models within each distribution.")
    } else if (selectModels && selectDistributions) {
      # best fitting model for the best distribution is shown
      message <- gettextf("Results are based on %1$s with %2$s distribution which was the best fitting model across all models and distributions.", attr(fit[[1]], "modelTitle"), attr(fit[[1]], "distribution"))
    }

  }

  return(message)
}
.sapSelectedComponentsMessage <- function(fit, options) {

  # the fits were selected hierarchically: distribution - number of components - model
  selectDistributions <- .sapMultipleFamilies(options) && .sapFamilySelection(options) %in% c("bestAic", "bestBic")
  allDistributions    <- .sapMultipleFamilies(options) && .sapFamilySelection(options) == "all"
  multipleModels      <- .sapMultipleModels(options)
  distribution        <- attr(fit[[1]], "distribution")
  components          <- .sapComponentsLabel(attr(fit[[1]], "components"))
  model               <- attr(fit[[1]], "modelTitle")

  if (!multipleModels || options[["interpretModel"]] == "all") {

    if (selectDistributions)
      return(gettextf("Results are based on %1$s distribution with %2$s which was the best fitting combination of distribution and number of components.", distribution, components))
    else if (allDistributions)
      return(gettext("Results are based on the best fitting number of components within each distribution."))
    else
      return(gettextf("Results are based on %1$s which was the best fitting number of components.", components))

  } else if (!options[["interpretModel"]] %in% c("bestAic", "bestBic")) {

    if (selectDistributions)
      return(gettextf("Results are based on %1$s with %2$s distribution and %3$s which was the best fitting combination of distribution and number of components across models.", model, distribution, components))
    else if (allDistributions)
      return(gettextf("Results are based on %1$s with the best fitting number of components within each distribution.", model))
    else
      return(gettextf("Results are based on %1$s with %2$s which was the best fitting number of components across models.", model, components))

  } else {

    if (selectDistributions)
      return(gettextf("Results are based on %1$s with %2$s distribution and %3$s which was the best fitting model across all models, distributions, and numbers of components.", model, distribution, components))
    else if (allDistributions)
      return(gettext("Results are based on the best fitting number of components and model within each distribution."))
    else
      return(gettextf("Results are based on %1$s with %2$s which was the best fitting model across all models and numbers of components.", model, components))
  }
}

# additional helper functions
.sapInformationCriteria2Weights <- function(ic) {

  isValidIc <- !is.na(ic)
  validIc   <- ic[isValidIc]

  deltaIc     <- validIc - min(validIc)
  relativeIc  <- exp(-0.5 * deltaIc)
  sumIc       <- sum(relativeIc)
  icWeights   <- relativeIc/sumIc

  out            <- rep(0, length(ic))
  out[isValidIc] <- icWeights

  return(out)
}
