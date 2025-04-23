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

ParametricSurvivalAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  if (.saSurvivalReady(options))
    dataset <- .saCheckDataset(dataset, options, type = "parametric")

  # Fit the models
  .sapFit(jaspResults, dataset, options)

  # Statistics
  if (options[["modelSummary"]])
    .sapSummaryTable(jaspResults, options)
  if (options[["coefficients"]])
    .sapCoefficientsTable(jaspResults, options)
  if (options[["coefficientsCovarianceMatrix"]])
    .sapCoefficientsCovarianceMatrixTable(jaspResults, options)




  return()
}

.sapDependencies <- c(
  "timeToEvent", "eventStatus", "eventIndicator", "censoringType", "subgroup",
  "factors", "covariates", "weights", "subgroup", "distribution", "includeFullDatasetInSubgroupAnalysis",
  "selectedParametricDistributionExponential" ,"selectedParametricDistributionGamma" ,"selectedParametricDistributionGeneralizedF" ,
  "selectedParametricDistributionGeneralizedGamma" ,"selectedParametricDistributionGompertz" ,"selectedParametricDistributionLogLogistic" ,
  "selectedParametricDistributionLogNormal" ,"selectedParametricDistributionWeibull" ,"selectedParametricDistributionGeneralizedGammaOriginal" ,
  "selectedParametricDistributionGeneralizedFOriginal",
  "modelTerms", "includeIntercept",
  "includeFullDatasetInSubgroupAnalysis"
)

# model fitting functions
.sapFit                 <- function(jaspResults, dataset, options) {

  if (!.saSurvivalReady(options))
    return()

  # extract the container
  if (is.null(jaspResults[["fit"]])) {
    fitContainer <- createJaspState()
    fitContainer$dependOn(c(
      # this does not contain `modelTerms` as the fits are updated only if the corresponding model changes
      "timeToEvent", "eventStatus", "eventIndicator", "censoringType",
      "factors", "covariates", "weights", "subgroup",
      "distribution", "includeIntercept",
      "selectedParametricDistributionExponential" ,"selectedParametricDistributionGamma" ,"selectedParametricDistributionGeneralizedF" ,
      "selectedParametricDistributionGeneralizedGamma" ,"selectedParametricDistributionGompertz" ,"selectedParametricDistributionLogLogistic" ,
      "selectedParametricDistributionLogNormal" ,"selectedParametricDistributionWeibull" ,"selectedParametricDistributionGeneralizedGammaOriginal" ,
      "selectedParametricDistributionGeneralizedFOriginal"
    ))
    jaspResults[["fit"]] <- fitContainer
    out                  <- NULL
  } else {
    fitContainer <- jaspResults[["fit"]]
    out          <- fitContainer$object
  }

  # check whether anything in the model had changed
  if (!is.null(out) &&
      isTRUE(all.equal(attr(out, "modelTerms"), options[["modelTerms"]])) &&
      attr(out, "includeFullDatasetInSubgroupAnalysis") == options[["includeFullDatasetInSubgroupAnalysis"]])
    return()

  # structure the container following:
  # - subgroup
  #   - family
  #     - model

  distributions <- .sapGetDistributions(options)

  # fit the full dataset
  if (options[["subgroup"]] == "" || options[["includeFullDatasetInSubgroupAnalysis"]]) {

    attr(dataset, "subgroup")      <- gettext("Full dataset")
    attr(dataset, "subgroupLabel") <- gettext("Full dataset")

    for(i in seq_along(distributions)) {
      out[["fullDataset"]][[distributions[i]]] <- .sapFitDistribution(out[["fullDataset"]][[distributions[i]]], dataset, options, distributions[i])
    }

    attr(out[["fullDataset"]], "label")         <- gettext("Full dataset")
    attr(out[["fullDataset"]], "dataset")       <- dataset
    attr(out[["fullDataset"]], "isSubgroup")    <- FALSE
    attr(out[["fullDataset"]], "distributions") <- distributions
  }

  # fit the subgroups
  if (options[["subgroup"]] != "") {

    subgroupLevels <- unique(dataset[[options[["subgroup"]]]])

    for(i in seq_along(subgroupLevels)) {

      subgroupDataset <- dataset[dataset[[options[["subgroup"]]]] == subgroupLevels[i],,drop=FALSE]
      subgroupDataset <- droplevels(subgroupDataset)

      attr(subgroupDataset, "subgroup")      <- as.character(subgroupLevels[i])
      attr(subgroupDataset, "subgroupLabel") <- gettextf("Subgroup: %1$s", subgroupLevels[i])

      for(j in seq_along(distributions)) {
        out[[paste0("subgroup", subgroupLevels[i])]][[distributions[j]]] <- .sapFitDistribution(out[[paste0("subgroup", subgroupLevels[i])]][[distributions[j]]], subgroupDataset, options, distributions[j])
      }

      attr(out[[paste0("subgroup", subgroupLevels[i])]], "label")         <- gettextf("Subgroup: %1$s", subgroupLevels[i])
      attr(out[[paste0("subgroup", subgroupLevels[i])]], "dataset")       <- subgroupDataset
      attr(out[[paste0("subgroup", subgroupLevels[i])]], "isSubgroup")    <- TRUE
      attr(out[[paste0("subgroup", subgroupLevels[i])]], "distributions") <- distributions
    }
  }

  attr(out, "modelTerms")                           <- options[["modelTerms"]]
  attr(out, "includeFullDatasetInSubgroupAnalysis") <- options[["includeFullDatasetInSubgroupAnalysis"]]

  fitContainer$object <- out

  return()
}
.sapFitDistribution     <- function(out, dataset, options, distribution) {

  for (i in seq_along(options[["modelTerms"]])) {

    # check whether the model has already been fitted
    if (length(out) >= i) {

      previousTerms  <- attr(out[[i]], "modelTerms")
      curentTerms    <- options[["modelTerms"]][[i]]

      # check without a title - allow renaming without re-fitting
      curentTitle    <- curentTerms[["title"]]
      previousTerms$title <- ""
      curentTerms$title   <- ""

      if (isTRUE(all.equal(previousTerms, curentTerms))) {
        # everything but the title is the same -> relabel
        attr(out[[i]], "label") <- curentTitle
        next
      }

      # skip model if it is same as the previous one
      if (i > 1) {
        simplerTerms       <- attr(out[[i-1]], "modelTerms")
        simplerTerms$title <- ""
        if (isTRUE(all.equal(previousTerms, simplerTerms))) {
          # everything but the title is the same -> relabel
          out[[i]]                <- out[[i-1]]
          attr(out[[i]], "label") <- curentTitle
          next
        }
      }
    }

    # fit the model
    out[[i]] <- .sapFitModel(dataset, options, distribution, options[["modelTerms"]][[i]])

  }

  # remove models that are not selected
  if (length(out) > length(options[["modelTerms"]])) {
    out <- out[seq_along(options[["modelTerms"]])]
  }

  # store attributes
  attr(out, "distribution") <- distribution
  attr(out, "label")        <- .sapOption2DistributionName(distribution)

  return(out)
}
.sapFitModel            <- function(dataset, options, distribution, modelTerms) {

  fit <- try(flexsurv::flexsurvreg(
    formula = .sapGetFormula(options, modelTerms),
    data    = dataset,
    dist    = distribution,
    weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]]
  ))

  # store attributes
  attr(fit, "subgroup")       <- attr(dataset, "subgroup")
  attr(fit, "subgroupLabel")  <- attr(dataset, "subgroupLabel")
  attr(fit, "modelTitle")     <- modelTerms[["title"]]
  attr(fit, "modelId")        <- modelTerms[["name"]]
  attr(fit, "modelTerms")     <- modelTerms
  attr(fit, "distribution")   <- .sapOption2DistributionName(distribution)

  return(fit)
}
.sapExtractFit          <- function(jaspResults, options, type = "all") {

  if (!.saSurvivalReady(options))
    return()

  out <- jaspResults[["fit"]][["object"]]
  fit <- list()

  if (options[["subgroup"]] != "" && !options[["includeFullDatasetInSubgroupAnalysis"]]) {
    out <- out[names(out) != "fullDataset"]
  }

  # automatically extract models by subgroup
  # subgroups are collapsed only if the user specifies one distribution and one model
  # distributions are collapsed if the user specifies one model (or asks for joining distributions/models)
  if (!options[["compareModelsAcrossDistributions"]] && .sapMultipleModels(options) && .sapMultiplDistributions(options)) {

    # separate outputs for each distribution
    for (i in seq_along(out)) {
      for (j in seq_along(out[[i]])) {
        fit[[length(fit) + 1]] <- out[[i]][[j]]
        if (options[["subgroup"]] != "")
          attr(fit[[length(fit)]], "label") <- paste0(attr(out[[i]], "label"), " | ", attr(out[[i]][[j]], "label"))
        else
          attr(fit[[length(fit)]], "label") <- attr(out[[i]][[j]], "label")
      }
    }

  }else if (options[["compareModelsAcrossDistributions"]] && .sapMultipleModels(options) && .sapMultiplDistributions(options)) {

    # join across distributions and models
    for (i in seq_along(out)) {
      fit[[names(out)[i]]] <- do.call(c, out[[i]])
      attr(fit[[names(out)[i]]], "label") <- attr(out[[i]], "label")
    }

  } else if (.sapMultiplDistributions(options)) {

    # join across distributions
    for (i in seq_along(out)) {
      fit[[names(out)[i]]] <- lapply(out[[i]], function(x) x[[1]])
      attr(fit[[names(out)[i]]], "label") <- attr(out[[i]], "label")
    }

  } else if (.sapMultipleModels(options)) {

    # join across models
    for (i in seq_along(out)) {
      fit[[names(out)[i]]] <- out[[i]][[1]]
      attr(fit[[names(out)[i]]], "label") <- attr(out[[i]], "label")
    }

  } else {

    # join subgroups if they have a single model
    fit <- list(lapply(out, function(x) x[[1]][[1]]))
  }

  # return all models in the restructured format
  if (type == "all")
    return(fit)

  ### return only the selected models
  selectDistributions <- length(.sapGetDistributions(options)) > 1 && options[["distribution"]]   %in% c("bestAic", "bestBic")
  selectModels        <- .sapMultipleModels(options)               && options[["interpretModel"]] !=   "all"

  # all models are selected
  if (!selectModels && !selectDistributions)
    return(fit)

  # no selection is needed when only a single model & distribution is specified (those are already joined across subgroups)
  if (!.sapMultipleModels(options) && !.sapMultiplDistributions(options))
    return(fit)

  # we don't need to worry whether we select across models / distributions since the output is already correctly structured
  # select the best distribution:
  if (selectDistributions) {
    for (i in seq_along(fit)) {
      # reuse the summary data function to obtain fit statistics
      tempSummary      <- .saSafeRbind(lapply(fit[[i]], .sapRowSummaryTable))
      bestDistribution <- tempSummary[["distribution"]][which.min(tempSummary[[switch(
        options[["distribution"]],
        "bestAic" = "aic",
        "bestBic" = "bic"
      )]])]
      fit[[i]][which(tempSummary[["distribution"]] != bestDistribution, arr.ind = TRUE)] <- NULL
    }
  }

  # select the best models:
  if (selectModels) {
    for (i in seq_along(fit)) {
      # reuse the summary data function to obtain fit statistics
      tempSummary      <- .saSafeRbind(lapply(fit[[i]], .sapRowSummaryTable))
      if (options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
        bestModel <- tempSummary[["model"]][which.min(tempSummary[[switch(
          options[["interpretModel"]],
          "bestAic" = "aic",
          "bestBic" = "bic"
        )]])]
        fit[[i]][which(tempSummary[["model"]] != bestModel, arr.ind = TRUE)] <- NULL
      } else {
        modelNames <- sapply(fit[[i]], attr, "modelId")
        fit[[i]][which(modelNames != options[["interpretModel"]], arr.ind = TRUE)] <- NULL
      }
    }
  }

  return(fit)
}


# summary tables
.sapSummaryTable         <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "all")

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "alwaysDisplayModelInformation",
                          "modelSummary",
                          "modelSummaryRankModels", "modelSummaryRankModelsBy",
                          "modelSummaryAicWeighs", "modelSummaryBicWeighs")

  if (length(fit) > 1) {

    summaryTable <- createJaspContainer(title = gettext("Model Summary"))
    summaryTable$dependOn(outputDependencies)
    summaryTable$position <- 1
    jaspResults[["summaryTable"]] <- summaryTable

    for (i in seq_along(fit)) {

      # create a table for each model set
      summaryTable[[paste0("table", i)]] <- .sapSummaryTableFun(fit[[i]], options)
      summaryTable[[paste0("table", i)]]$position <- i
      summaryTable[[paste0("table", i)]]$title    <- attr(fit[[i]], "label")

    }

  } else {

    # only one table needed
    summaryTable          <- .sapSummaryTableFun(fit[[1]], options)
    summaryTable$title    <- gettext("Model Summary")
    summaryTable$dependOn(outputDependencies)
    summaryTable$position <- 1
    jaspResults[["summaryTable"]] <- summaryTable

  }

  return()
}
.sapSummaryTableFun      <- function(fit, options) {

  # create the table
  summaryTable <- createJaspTable()
  .sapAddColumnSubgroup(     summaryTable, options, output = "modelSummary")
  .sapAddColumnDistribution( summaryTable, options, output = "modelSummary")
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
  errors <- .sapCollectFitErrors(fit, options)
  for (i in seq_along(errors))
    summaryTable$addFootnote(errors[[i]], symbol = gettext("Error: "))

  summaryTable$setData(data)
  summaryTable$showSpecifiedColumnsOnly <- TRUE

  return(summaryTable)
}
.sapCoefficientsTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["coefficientsTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "coefficients")

  if (length(fit) > 1) {

    coefficientsTable <- createJaspContainer(title = gettext("Coefficients Summary"))
    coefficientsTable$dependOn(outputDependencies)
    coefficientsTable$position <- 1
    jaspResults[["coefficientsTable"]] <- coefficientsTable

    for (i in seq_along(fit)) {

      # create a table for each model set
      coefficientsTable[[paste0("table", i)]] <- .sapCoefficientsTableFun(fit[[i]], options)
      coefficientsTable[[paste0("table", i)]]$position <- i
      coefficientsTable[[paste0("table", i)]]$title    <- attr(fit[[i]], "label")

    }

  } else {

    # only one table needed
    coefficientsTable          <- .sapCoefficientsTableFun(fit[[1]], options)
    coefficientsTable$title    <- gettext("Coefficients Summary")
    coefficientsTable$dependOn(outputDependencies)
    coefficientsTable$position <- 1
    jaspResults[["coefficientsTable"]] <- coefficientsTable

  }

  return()
}
.sapCoefficientsTableFun <- function(fit, options) {

  # create the table
  estimatesTable <- createJaspTable()
  .sapAddColumnSubgroup(     estimatesTable, options, output = "coefficients")
  .sapAddColumnDistribution( estimatesTable, options, output = "coefficients")
  .sapAddColumnModel(        estimatesTable, options, output = "coefficients")
  estimatesTable$addColumnInfo(name = "coefficient",    title = "",                         type = "string")
  estimatesTable$addColumnInfo(name = "est",            title = gettext("Estimate"),        type = "number")
  estimatesTable$addColumnInfo(name = "se",             title = gettext("Standard Error"),  type = "number")
  overtitleCi <- gettextf("%s%% CI", 100 * 0.95) # TODO: add options[["confidenceIntervalsLevel"]]
  estimatesTable$addColumnInfo(name = "L95.", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
  estimatesTable$addColumnInfo(name = "U95.", title = gettext("Upper"), type = "number", overtitle = overtitleCi)


  if (!.saSurvivalReady(options))
    return(estimatesTable)

  # extract the data
  data <- .saSafeRbind(lapply(fit, .sapRowCoefficientsTable))
  data <- .saSafeSimplify(data)

  # add footnotes
  messages <- .sapSelectedModelMessage(fit, options)
  for (i in seq_along(messages))
    estimatesTable$addFootnote(messages[[i]])

  estimatesTable$setData(data)
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  return(estimatesTable)
}
.sapCoefficientsCovarianceMatrixTable    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["coefficientsTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  # flatten the list - each model has to get its own matrix because they might differ in parameters etc...
  fit <- list(lapply(out, function(x) x[[1]][[1]]))

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "coefficientsCovarianceMatrix")

  if (length(fit) > 1) {

    covarianceMatrixTableTable <- createJaspContainer(title = gettext("Coefficients Covariance Matrix"))
    covarianceMatrixTableTable$dependOn(outputDependencies)
    covarianceMatrixTableTable$position <- 1
    jaspResults[["covarianceMatrixTableTable"]] <- covarianceMatrixTableTable

    for (i in seq_along(fit)) {

      # create a table for each model set
      covarianceMatrixTableTable[[paste0("table", i)]] <- .sapCoefficientsCovarianceMatrixTableFun(fit[[i]], options)
      covarianceMatrixTableTable[[paste0("table", i)]]$position <- i
      covarianceMatrixTableTable[[paste0("table", i)]]$title    <- attr(fit[[i]], "label")

    }

  } else {

    # only one table needed
    covarianceMatrixTableTable          <- .sapCoefficientsCovarianceMatrixTableFun(fit[[1]], options)
    covarianceMatrixTableTable$title    <- gettext("Coefficients Covariance Matrix")
    covarianceMatrixTableTable$dependOn(outputDependencies)
    covarianceMatrixTableTable$position <- 1
    jaspResults[["covarianceMatrixTableTable"]] <- covarianceMatrixTableTable

  }

  return()
}
.sapCoefficientsCovarianceMatrixTableFun <- function(fit, options) {

  # create the table
  covarianceMatrixTableTable <- createJaspTable()
  .sapAddColumnSubgroup(     covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  covarianceMatrixTableTable$addColumnInfo(name = "coefficient",    title = "",                         type = "string")

  if (!.saSurvivalReady(options))
    return(covarianceMatrixTableTable)

  # extract the data
  data <- .sapRowcovarianceMatrixTableTable(fit[[1]])

  if (jaspBase::isTryError(fit[[1]]))
    return(covarianceMatrixTableTable)

  # add columns for each parameter
  for (i in 1:nrow(data)) {
    covarianceMatrixTableTable$addColumnInfo(name = rownames(data)[i], title = rownames(data)[i], type = "number")
  }

  # add footnotes
  messages <- .sapSelectedModelMessage(fit, options)
  for (i in seq_along(messages))
    estimatesTable$addFootnote(messages[[i]])

  covarianceMatrixTableTable$setData(data)
  covarianceMatrixTableTable$showSpecifiedColumnsOnly <- TRUE

  return(covarianceMatrixTableTable)
}

.sapRowModelInformation  <- function(fit) {
  return(data.frame(
    subgroup     = attr(fit, "subgroup"),
    model        = attr(fit, "modelTitle"),
    distribution = attr(fit, "distribution")
  ))
}
.sapRowSummaryTable      <- function(fit) {

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
.sapRowCoefficientsTable <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  return(data.frame(
    .sapRowModelInformation(fit),
    coefficient  = rownames(fit[["res"]]),
    fit[["res"]]
  ))
}
.sapRowcovarianceMatrixTableTable <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  return(data.frame(
    .sapRowModelInformation(fit),
    coefficient  = rownames(fit[["cov"]]),
    fit[["cov"]]
  ))
}

.sapAddColumnSubgroup     <- function(tempTable, options, output) {

  if (output %in% c("modelSummary", "coefficients")) {
    if (options[["subgroup"]] != "" && !.sapMultiplDistributions(options) && !.sapMultipleModels(options))
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
.sapAddColumnDistribution <- function(tempTable, options, output) {

  if(options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }

  if (output == "modelSummary" && .sapMultiplDistributions(options)) {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }

  if (output == "coefficients" && options[["distribution"]] == "all") {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }

  if (output == "coefficientsCovarianceMatrix" && options[["alwaysDisplayModelInformation"]]) {
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    return()
  }
}

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

.sapCollectFitErrors      <- function(fit, options) {

  errors <- NULL

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]))
      errors <- c(errors, gettextf(
        "%1$s model %2$s%3$s failed with the following message: %4$s.",
        distribution = attr(fit[[i]], "distribution"),
        model        = attr(fit[[i]], "model"),
        subgroup     = if (options[["subgroup"]] != "") paste0(" (", attr(fit[[i]], "subgroupLabel"), ")") else "",
        error        = fit[[i]]
      ))
  }

  return(errors)
}
.sapSelectedModelMessage  <- function(fit, options) {

  message <- NULL

  # check whether selection rules were applied
  multipleModels        <- .sapMultipleModels(options)
  multipleDistributions <- .sapMultiplDistributions(options)

  selectModels        <- multipleModels        && options[["interpretModel"]] != "all"
  selectDistributions <- multipleDistributions && options[["distribution"]]   %in% c("bestAic", "bestBic")

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
    } else if (selectModels && !selectDistributions && options[["distribution"]] != "all") {
      # best fitting model for the selected distribution is shown
      message <- gettext("Results are based on best fitting models.")
    }  else if (selectModels && !selectDistributions && options[["distribution"]] == "all") {
      # best fitting model for all distributions is shown
      message <- gettext("Results are based on best fitting models within each distribution.")
    } else if (selectModels && selectDistributions && !options[["compareModelsAcrossDistributions"]]) {
      # best fitting model for the given distribution is shown
      message <- gettext("Results are based on best fitting models within each distribution.")
    } else if (selectModels && selectDistributions && options[["compareModelsAcrossDistributions"]]) {
      # best fitting model for the best distribution is shown
      message <- gettextf("Results are based on %1$s with %2$s distribution which was the best fitting model across all models and distributions.", attr(fit[[1]], "modelTitle"), attr(fit[[1]], "distribution"))
    }

  }

  return(message)
}

# add the model names
.sapMultipleModels          <- function(options) {
  return(length(options[["modelTerms"]]) > 1)
}
.sapMultiplDistributions    <- function(options) {
  return(options[["distribution"]] %in% c("all", "bestAic", "bestBic") && length(.sapGetDistributions(options)) > 1)
}
.sapMultipleOutputs         <- function(options) {

  # create a container with multiple outputs if
  # - subgroup analysis is specified
  # - multiple models are compared across multiple distributions & the output is not to be joined
  return((options[["subgroup"]] != "" || (options[["compareModelsAcrossDistributions"]] && .sapMultipleModels(options) && .sapMultiplDistributions(options))))
}
.sapOption2Distribution     <- function(optionName) {

  return(switch(
    optionName,
    "exponential"              = "exp",
    "gamma"                    = "gamma",
    "generalizedF"             = "genf",
    "generalizedGamma"         = "gengamma",
    "gompertz"                 = "gompertz",
    "logLogistic"              = "llogis",
    "logNormal"                = "lnorm",
    "weibull"                  = "weibull",
    "generalizedGammaOriginal" = "gengamma.orig",
    "generalizedFOriginal"     = "genf.orig"
  ))
}
.sapOption2DistributionName <- function(optionName) {

  return(switch(
    optionName,
    # either using option name
    "exponential"              = "Exponential",
    "gamma"                    = "Gamma",
    "generalizedF"             = "Generalized F",
    "generalizedGamma"         = "Generalized gamma",
    "gompertz"                 = "Gompertz",
    "logLogistic"              = "Log-logistic",
    "logNormal"                = "Log-normal",
    "weibull"                  = "Weibull",
    "generalizedGammaOriginal" = "Generalized gamma (original)",
    "generalizedFOriginal"     = "Generalized F (original)",
    # or using distribution name
    "exp"           = "Exponential",
    "gamma"         = "Gamma",
    "genf"          = "Generalized F",
    "gengamma"      = "Generalized gamma",
    "gompertz"      = "Gompertz",
    "llogis"        = "Log-logistic",
    "lnorm"         = "Log-normal",
    "weibull"       = "Weibull",
    "gengamma.orig" = "Generalized gamma (original)",
    "genf.orig"     = "Generalized F (original)"
  ))
}
.sapGetDistributions        <- function(options) {

  if (options[["distribution"]] %in% c("all", "bestAic", "bestBic")) {

    distributions <- list()
    if (options[["selectedParametricDistributionExponential"]])
      distributions[["exponential"]]      <- .sapOption2Distribution("exponential")
    if (options[["selectedParametricDistributionGamma"]])
      distributions[["gamma"]]            <- .sapOption2Distribution("gamma")
    if (options[["selectedParametricDistributionGeneralizedF"]])
      distributions[["generalizedF"]]     <- .sapOption2Distribution("generalizedF")
    if (options[["selectedParametricDistributionGeneralizedGamma"]])
      distributions[["generalizedGamma"]] <- .sapOption2Distribution("generalizedGamma")
    if (options[["selectedParametricDistributionGompertz"]])
      distributions[["gompertz"]]         <- .sapOption2Distribution("gompertz")
    if (options[["selectedParametricDistributionLogLogistic"]])
      distributions[["logLogistic"]]      <- .sapOption2Distribution("logLogistic")
    if (options[["selectedParametricDistributionLogNormal"]])
      distributions[["logNormal"]]       <- .sapOption2Distribution("logNormal")
    if (options[["selectedParametricDistributionWeibull"]])
      distributions[["weibull"]]         <- .sapOption2Distribution("weibull")
    if (options[["selectedParametricDistributionGeneralizedGammaOriginal"]])
      distributions[["generalizedGammaOriginal"]] <- .sapOption2Distribution("generalizedGammaOriginal")
    if (options[["selectedParametricDistributionGeneralizedFOriginal"]])
      distributions[["generalizedFOriginal"]]     <- .sapOption2Distribution("generalizedFOriginal")

    distributions <- do.call(c, distributions)
    if (length(distributions) == 0)
      .quitAnalysis(paste0("No parametric Distribution selected. Please select at least one parametric Distribution."))

  } else {

    distributions <- .sapOption2Distribution(options[["distribution"]])

  }

  return(distributions)
}
