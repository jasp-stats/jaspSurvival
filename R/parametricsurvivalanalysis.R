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
  if (options[["sequentialModelComparison"]])
    .sapSequentialModelComparisonTable(jaspResults, options)
  if (options[["coefficients"]])
    .sapCoefficientsTable(jaspResults, options)
  if (options[["coefficientsCovarianceMatrix"]])
    .sapCoefficientsCovarianceMatrixTable(jaspResults, options)


  # Prediction Tables
  if (options[["survivalTimeTable"]])
    .sapSurvivalTimeTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["survivalProbabilityTable"]])
    .sapSurvivalProbabilityTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["hazardTable"]])
    .sapHazardTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["cumulativeHazardTable"]])
    .sapCumHazardTable(jaspResults, options)
  if (!options[["lifeTimeMergeTablesAcrossMeasures"]] && options[["restrictedMeanSurvivalTimeTable"]])
    .sapRmstTable(jaspResults, options)
  if (options[["lifeTimeMergeTablesAcrossMeasures"]])
    .sapLifeTimeTable(jaspResults, options)

  # Prediction Plots
  if (options[["survivalTimePlot"]])
    .sapSurvivalTimePlot(jaspResults, options)
  if (options[["survivalProbabilityPlot"]])
    .sapSurvivalProbabilityPlot(jaspResults, options)
  if (options[["hazardPlot"]])
    .sapHazardPlot(jaspResults, options)
  if (options[["cumulativeHazardPlot"]])
    .sapCumHazardPlot(jaspResults, options)
  if (options[["restrictedMeanSurvivalTimePlot"]])
    .sapRmstPlot(jaspResults, options)

  # Diagnostics
  if (options[["residualPlotResidualVsTime"]])
    .sapResidualsVsTimePlot(jaspResults, options)
  if (options[["residualPlotResidualVsPredictors"]])
    .sapResidualsVsPredictorsPlot(jaspResults, options)
  if (options[["residualPlotResidualVsPredicted"]])
    .sapResidualsVsPredictedPlot(jaspResults, options)
  if (options[["residualPlotResidualHistogram"]])
    .sapResidualHistogramPlot(jaspResults, options)

  return()
}

.sapDependencies <- c(
  "intervalStart", "intervalEnd", "timeToEvent", "eventStatus", "eventIndicator", "censoringType", "subgroup",
  "factors", "covariates", "weights", "subgroup", "distribution", "includeFullDatasetInSubgroupAnalysis",
  "selectedParametricDistributionExponential" ,"selectedParametricDistributionGamma" ,"selectedParametricDistributionGeneralizedF" ,
  "selectedParametricDistributionGeneralizedGamma" ,"selectedParametricDistributionGompertz" ,"selectedParametricDistributionLogLogistic" ,
  "selectedParametricDistributionLogNormal" ,"selectedParametricDistributionWeibull" ,"selectedParametricDistributionGeneralizedGammaOriginal" ,
  "selectedParametricDistributionGeneralizedFOriginal",
  "modelTerms", "includeIntercept",
  "includeFullDatasetInSubgroupAnalysis",
  # the CIs are not a simple multiplier of the standard error
  # as such, they need to be changed during the fitting process
  "coefficientsConfidenceIntervalLevel"
)

# model fitting and extraction functions
# these are pretty much the workhorse of the analysis:
# they make sure that you obtain exactly the models you want
# (all the following output just uses the models extracted in the correct format)
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
      "selectedParametricDistributionGeneralizedFOriginal",
      # the CIs are not a simple multiplier of the standard error
      # as such, they need to be changed during the fitting process
      "coefficientsConfidenceIntervalLevel"
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
    weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]],
    cl      = options[["coefficientsConfidenceIntervalLevel"]]
  ))

  # store attributes
  attr(fit, "subgroup")       <- attr(dataset, "subgroup")
  attr(fit, "subgroupLabel")  <- attr(dataset, "subgroupLabel")
  attr(fit, "modelTitle")     <- modelTerms[["title"]]
  attr(fit, "modelId")        <- modelTerms[["name"]]
  attr(fit, "modelTerms")     <- modelTerms
  attr(fit, "distribution")   <- .sapOption2DistributionName(distribution)
  attr(fit, "dataset")        <- dataset

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

  # extract models by subgroups
  if (type == "byModel") {
    fit <- .sapExtractFitModels(out, options)
  } else {
    fit <- .sapExtractFitGroups(out, options)
  }

  # return all models in the restructured format
  if (type %in% c("all", "byModel"))
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
  if (type != "byDistribution" && selectModels) {
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
.sapExtractFitGroups    <- function(out, options) {

  fit <- list()

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

  return(fit)
}
.sapExtractFitModels    <- function(out, options) {

  fit <- list()

  # automatically extract models by model across distributions
  # subgroups are never collapsed
  for (i in seq_along(out)) {
    for (j in seq_along(options[["modelTerms"]])) {
      fit[[length(fit) + 1]] <- lapply(out[[i]], function(x) x[[j]])
      if (options[["subgroup"]] != "" && length(options[["modelTerms"]]) > 1)
        attr(fit[[length(fit)]], "label") <- paste0(attr(out[[i]], "label"), " | ", options[["modelTerms"]][[j]][["title"]])
      else if (options[["subgroup"]] == "" && length(options[["modelTerms"]]) > 1)
        attr(fit[[length(fit)]], "label") <- options[["modelTerms"]][[j]][["title"]]
      else if (options[["subgroup"]] != "" && length(options[["modelTerms"]]) == 1)
        attr(fit[[length(fit)]], "label") <- attr(out[[i]], "label")
      else
        attr(fit[[length(fit)]], "label") <- ""
    }
  }

  return(fit)
}
.sapFlattenFit          <- function(fit, options) {

  out <- list()

  # check the output type
  multipleModels        <- .sapMultipleModels(options)
  multipleDistributions <- .sapMultiplDistributions(options)

  for(i in seq_along(fit)) {
    for(j in seq_along(fit[[i]])) {
      out[[length(out) + 1]] <- fit[[i]][[j]]

      if (options[["subgroup"]] != "") {
        prefix <- paste0(attr(fit[[i]][[j]], "subgroupLabel"), " | ")
      } else {
        prefix <- ""
      }

      if (multipleModels && multipleDistributions) {
        attr(out[[length(out)]], "label") <- paste0(prefix, attr(fit[[i]][[j]], "distribution"), " distribution | ", attr(fit[[i]][[j]], "modelTitle"))
      } else if (multipleModels) {
        attr(out[[length(out)]], "label") <- paste0(prefix, attr(fit[[i]][[j]], "modelTitle"))
      } else if (multipleDistributions) {
        attr(out[[length(out)]], "label") <- paste0(prefix, attr(fit[[i]][[j]], "distribution"), " distribution")
      } else {
        attr(out[[length(out)]], "label") <- prefix
      }

    }
  }

  if (length(out) == 0)
    return(NULL)

  return(out)
}
.sapNestFit             <- function(fit) {

  out <- list()
  for (i in seq_along(fit)) {
    out[[i]] <- fit[i]
    attr(out[[i]], "label") <- attr(fit[[i]], "label")
  }

  if (length(out) == 0)
    return(NULL)

  return(out)
}

# all tables are created in the same way
.sapSectionWrapper <- function(jaspResults, options, fit, tableFunction, name, title, dependencies, position) {

  if (length(fit) > 1) {

    tempContainer <- createJaspContainer(title = title)
    tempContainer$dependOn(dependencies)
    tempContainer$position <- position
    jaspResults[[name]]    <- tempContainer

    for (i in seq_along(fit)) {

      # create a table for each model set
      tempContainer[[paste0("table", i)]] <- do.call(tableFunction, list(fit = fit[[i]], options = options))
      tempContainer[[paste0("table", i)]]$position <- i
      tempContainer[[paste0("table", i)]]$title    <- attr(fit[[i]], "label")

    }

  } else {

    # only one table needed
    tempTable           <- do.call(tableFunction, list(fit = fit[[1]], options = options))
    tempTable$title     <- title
    tempTable$dependOn(dependencies)
    tempTable$position  <- position
    jaspResults[[name]] <- tempTable

  }

  return()
}

# summary tables
.sapSummaryTable                      <- function(jaspResults, options) {

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
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "alwaysDisplayModelInformation",
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
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "coefficients")

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

  if (!is.null(jaspResults[["covarianceMatrixTableTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  # flatten the list - each model has to get its own matrix because they might differ in parameters etc...
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
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

  summaryTable$setData(data)
  summaryTable$showSpecifiedColumnsOnly <- TRUE

  return(summaryTable)
}
.sapSequentialModelComparisonTableFun    <- function(fit, options) {

  # create the table
  sequentialModelComparisonTable <- createJaspTable()
  .sapAddColumnSubgroup(     sequentialModelComparisonTable, options, output = "coefficients")
  .sapAddColumnDistribution( sequentialModelComparisonTable, options, output = "coefficients")
  sequentialModelComparisonTable$addColumnInfo(name = "model0", title = "H\U2080",      type = "string")
  sequentialModelComparisonTable$addColumnInfo(name = "model1", title = "H\U2081",      type = "string")
  sequentialModelComparisonTable$addColumnInfo(name = "chi2",   title = "\U03C7\U00B2", type = "number")
  sequentialModelComparisonTable$addColumnInfo(name = "df",     title = gettext("df"),           type = "integer")
  sequentialModelComparisonTable$addColumnInfo(name = "pValue", title = gettext("p"),            type = "pvalue")

  if (!.saSurvivalReady(options))
    return(sequentialModelComparisonTable)

  data <- list()
  for(i in 1:(length(fit) - 1)) {
    data[[i]] <- .sapRowSequentialModelComparisonTable(fit[[i]], fit[[i + 1]])
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
  .sapAddColumnModel(        estimatesTable, options, output = "coefficients")
  estimatesTable$addColumnInfo(name = "coefficient",    title = "",                         type = "string")
  estimatesTable$addColumnInfo(name = "est",            title = gettext("Estimate"),        type = "number")
  estimatesTable$addColumnInfo(name = "se",             title = gettext("Standard Error"),  type = "number")
  overtitleCi <- gettextf("%s%% CI", 100 * options[["coefficientsConfidenceIntervalLevel"]])
  estimatesTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
  estimatesTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitleCi)

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
    thisRegression <- grepl("JaspColumn", data[["coefficient"]])
    data$z[thisRegression]      <- data$est[thisRegression] / data$se[thisRegression]
    data$pValue[thisRegression] <- 2 * pnorm(-abs(data$z[thisRegression]))
    estimatesTable$addFootnote(gettext("P-values are based on a Wald test."))

    # fix coefficient names
    data[["coefficient"]][thisRegression] <- sapply(data[["coefficient"]][thisRegression], .saTermNames, variables = c(options[["covariates"]], options[["factors"]]))
  }

  # add footnotes
  messages <- .sapSelectedModelMessage(fit, options)
  for (i in seq_along(messages))
    estimatesTable$addFootnote(messages[[i]])

  estimatesTable$setData(data)
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  return(estimatesTable)
}
.sapCoefficientsCovarianceMatrixTableFun <- function(fit, options) {

  # create the table
  covarianceMatrixTableTable <- createJaspTable()
  .sapAddColumnSubgroup(     covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( covarianceMatrixTableTable, options, output = "coefficientsCovarianceMatrix")
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

# predictions tables
.sapSurvivalTimeTable        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalTimeTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  # flatten the list - each model has to get its own matrix because they might differ in parameters etc...
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalTimeTable", "predictionsSurvivalTimeStepsType", "predictionsSurvivalTimeStepsNumber", "predictionsSurvivalTimeStepsFrom",
                          "predictionsSurvivalTimeStepsSize", "predictionsSurvivalTimeStepsTo", "predictionsSurvivalTimeCustom",
                          "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel"
                          )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalTimeTableFun,
    name          = "survivalTimeTable",
    title         = gettext("Predicted Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.01
  )

  return()
}
.sapSurvivalProbabilityTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalProbabilityTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalProbabilityTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom", "survivalProbabilityAsFailureProbability"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalProbabilityTableFun,
    name          = "survivalProbabilityTable",
    title         = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Predicted Failure Probability") else gettext("Predicted Survival Probability"),
    dependencies  = outputDependencies,
    position      = 3.11
  )

  return()
}
.sapHazardTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["hazardTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "hazardTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapHazardTableFun,
    name          = "hazardTable",
    title         = gettext("Predicted Hazard"),
    dependencies  = outputDependencies,
    position      = 3.21
  )

  return()
}
.sapCumHazardTable           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["cumHazardTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "cumulativeHazardTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapCumHazardTableFun,
    name          = "cumHazardTable",
    title         = gettext("Predicted Cumulative Hazard"),
    dependencies  = outputDependencies,
    position      = 3.31
  )

  return()
}
.sapRmstTable                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["rmstTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "restrictedMeanSurvivalTimeTable", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapRmstTableFun,
    name          = "rmstTable",
    title         = gettext("Predicted Restricted Mean Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.41
  )

  return()
}
.sapLifeTimeTable            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["lifeTimeTable"]]))
    return()

  if (!options[["survivalProbabilityTable"]] && !options[["hazardTable"]] && !options[["cumulativeHazardTable"]] && !options[["restrictedMeanSurvivalTimeTable"]])
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "survivalProbabilityTable", "hazardTable", "cumulativeHazardTable", "restrictedMeanSurvivalTimeTable", "lifeTimeMergeTablesAcrossMeasures",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapLifeTimeTableFun,
    name          = "lifeTimeTable",
    title         = gettext("Life Time Table"),
    dependencies  = outputDependencies,
    position      = 3.11
  )

  return()
}

# prediction plots
.sapSurvivalTimePlot        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalTimePlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (options[["survivalTimeMergePlotsAcrossDistributions"]] && options[["distribution"]] %in% "all" && !options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalTimePlot", "predictionsSurvivalTimeStepsType", "predictionsSurvivalTimeStepsNumber", "predictionsSurvivalTimeStepsFrom",
                          "predictionsSurvivalTimeStepsSize", "predictionsSurvivalTimeStepsTo", "predictionsSurvivalTimeCustom",
                          "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel", "survivalTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalTimePlotFun,
    name          = "survivalTimePlot",
    title         = gettext("Predicted Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.02
  )

  return()
}
.sapSurvivalProbabilityPlot <- function(jaspResults, options) {

  if (!is.null(jaspResults[["survivalProbabilityPlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (options[["lifeTimeMergePlotsAcrossDistributions"]] && options[["distribution"]] %in% "all" && !options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "survivalProbabilityPlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme",
                          "survivalProbabilityPlotAddKaplanMeier", "survivalProbabilityPlotAddCensoringEvents", "survivalProbabilityPlotTransformXAxis", "survivalProbabilityPlotTransformYAxis",
                          "survivalProbabilityAsFailureProbability"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapSurvivalProbabilityPlotFun,
    name          = "survivalProbabilityPlot",
    title         = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Predicted Failure Probability") else gettext("Predicted Survival Probability"),
    dependencies  = outputDependencies,
    position      = 3.12
  )

  return()
}
.sapHazardPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["hazardPlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (options[["lifeTimeMergePlotsAcrossDistributions"]] && options[["distribution"]] %in% "all" && !options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "hazardPlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapHazardPlotFun,
    name          = "hazardPlot",
    title         = gettext("Predicted Hazard"),
    dependencies  = outputDependencies,
    position      = 3.22
  )

  return()
}
.sapCumHazardPlot           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["cumulativeHazardPlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (options[["lifeTimeMergePlotsAcrossDistributions"]] && options[["distribution"]] %in% "all" && !options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "cumulativeHazardPlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapCumHazardPlotFun,
    name          = "cumulativeHazardPlot",
    title         = gettext("Predicted Cumulative Hazard"),
    dependencies  = outputDependencies,
    position      = 3.32
  )

  return()
}
.sapRmstPlot                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["restrictedMeanSurvivalTimePlot"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  if (options[["lifeTimeMergePlotsAcrossDistributions"]] && options[["distribution"]] %in% "all" && !options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapNestFit(.sapFlattenFit(fit, options))
  }

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "restrictedMeanSurvivalTimePlot", "lifeTimeMergeTablesAcrossMeasures", "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeRoundSteps", "predictionsLifeTimeCustom",
                          "lifeTimeMergePlotsAcrossDistributions", "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapRmstPlotFun,
    name          = "restrictedMeanSurvivalTimePlot",
    title         = gettext("Predicted Restricted Mean Survival Time"),
    dependencies  = outputDependencies,
    position      = 3.42
  )

  return()
}

.sapCreatePredictionTableWrapper <- function(fit, options, type) {

  if (type == "quantile") {
    atTitle <- gettext("Quantile")
  } else {
    atTitle <- gettext("Time")
  }

  estimateTitle <- switch(
    type,
    "quantile"  = gettext("Survival Time"),
    "survival"  = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"),
    "hazard"    = gettext("Hazard"),
    "cumhaz"    = gettext("Cumulative Hazard"),
    "rmst"      = gettext("Restricted Mean Survival Time")
  )

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit)) {
    tempTable <- .sapCreatePredictionTable(options, atTitle = atTitle, estimateNames = "", estimateTitles = estimateTitle)
    return(tempTable)
  }

  # if there is any continuous predictor, the output is averaged across the predictors matrix
  if (type == "quantile") {
    optionsSequence <- .sapOptions2PredictionQuantile(options)
    data  <- summary(fit, type = type, quantiles = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]])
  } else {
    optionsSequence <- .sapOptions2PredictionTime(options, fit)
    data  <- summary(fit, type = type, t = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]])
  }
  dataLength <- length(data)

  for (i in seq_along(data)) {
    data[[i]]           <- data[[i]][,-1]
    colnames(data[[i]]) <- c("estimate", "lCi", "uCi")

    # transform survival to failure if requested
    if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
      data[[i]]$estimate <- 1 - data[[i]]$estimate
      data[[i]]$lCi      <- 1 - data[[i]]$lCi
      data[[i]]$uCi      <- 1 - data[[i]]$uCi
    }
  }

  estimateTitles <- names(data)
  names(data)    <- paste0("par", seq_along(data))
  data           <- do.call(cbind, data)

  tempTable <- .sapCreatePredictionTable(
    options        = options,
    atTitle        = atTitle,
    estimateNames  = paste0("par", 1:dataLength, "."),
    estimateTitles = if (dataLength == 1) estimateTitle else estimateTitles
  )

  # add remaining information
  data$at              <- optionsSequence
  data$subgroup        <- NA
  data$distribution    <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$model[1]        <- attr(fit, "modelTitle")

  if (!is.null(attr(fit, "label")))
    tempTable$addFootnote(attr(fit, "label"))

  tempTable$setData(data)
  tempTable$showSpecifiedColumnsOnly <- TRUE

  return(tempTable)
}
.sapLifeTimeTableWrapper         <- function(fit, options, type, timeSequence) {

  tempData           <- summary(fit, type = type, t = timeSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]])
  if (length(tempData) > 1) {
    tempTable$setError(gettext("Life time tables cannot be merged if there is more than a one prediction from a given model."))
    return(tempTable)
  }
  tempData           <- tempData[[1]][,-1]
  colnames(tempData) <- c("estimate", "lCi", "uCi")

  # transform survival to failure if requested
  if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
    tempData$estimate <- 1 - tempData$estimate
    tempData$lCi      <- 1 - tempData$lCi
    tempData$uCi      <- 1 - tempData$uCi
  }

  return(tempData)
}

.sapSurvivalTimeTableFun         <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "quantile")
  return(tempTable)
}
.sapSurvivalProbabilityTableFun  <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "survival")
  return(tempTable)
}
.sapHazardTableFun               <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "hazard")
  return(tempTable)
}
.sapCumHazardTableFun            <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "cumhaz")
  return(tempTable)
}
.sapRmstTableFun                 <- function(fit, options) {

  tempTable <- .sapCreatePredictionTableWrapper(fit, options, type = "rmst")
  return(tempTable)
}
.sapLifeTimeTableFun             <- function(fit, options) {

  tempTable <- createJaspTable()
  .sapAddColumnSubgroup(     tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        tempTable, options, output = "coefficientsCovarianceMatrix")
  tempTable$addColumnInfo(name = "at", title = gettext("Time"), type = "number")

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(tempTable)

  timeSequence <- .sapOptions2PredictionTime(options, fit)
  data <- list()

  # add dots to 'estimateName' since cbind merges names with collapse = "."

  if (options[["survivalProbabilityTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"), estimateName = "survivalProbability.")
    data[["survivalProbability"]] <- .sapLifeTimeTableWrapper(fit, options, type = "survival", timeSequence = timeSequence)
  }

  if (options[["hazardTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Hazard"), estimateName = "hazard.")
    data[["hazard"]] <- .sapLifeTimeTableWrapper(fit, options, type = "hazard", timeSequence = timeSequence)
  }

  if (options[["cumulativeHazardTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Cumulative Hazard"), estimateName = "cumulativeHazard.")
    data[["cumulativeHazard"]] <- .sapLifeTimeTableWrapper(fit, options, type = "cumhaz", timeSequence = timeSequence)
  }

  if (options[["restrictedMeanSurvivalTimeTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Restricted Mean Survival Time"), estimateName = "restrictedMeanSurvivalTime.")
    data[["restrictedMeanSurvivalTime"]] <- .sapLifeTimeTableWrapper(fit, options, type = "rmst", timeSequence = timeSequence)
  }

  data <- do.call(cbind, data)

  data$at              <- timeSequence
  data$subgroup        <- NA
  data$distribution    <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$model[1]        <- attr(fit, "modelTitle")

  if (!is.null(attr(fit, "label")))
    tempTable$addFootnote(attr(fit, "label"))

  tempTable$setData(data)
  tempTable$showSpecifiedColumnsOnly <- TRUE

  return(tempTable)
}

.sapCreatePredictionPlotWrapper <- function(fit, options, type) {

  if (type == "quantile") {
    atTitle <- gettext("Quantile")
  } else {
    atTitle <- gettext("Time")
  }

  estimateTitle <- switch(
    type,
    "quantile"  = gettext("Survival Time"),
    "survival"  = if (options[["survivalProbabilityAsFailureProbability"]]) gettext("Failure Probability") else gettext("Survival Probability"),
    "hazard"    = gettext("Hazard"),
    "cumhaz"    = gettext("Cumulative Hazard"),
    "rmst"      = gettext("Restricted Mean Survival Time")
  )

  checkFit <- sapply(fit, jaspBase::isTryError)
  if (!.saSurvivalReady(options) || all(checkFit)) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    return(tempPlot)
  }

  # extract an example fit & dataset (make sure the fit converged)
  tempFit  <- fit[[which.min(checkFit)]]
  tempData <- attr(tempFit, "dataset")

  if (type == "quantile") {
    optionsSequence <- .sapOptions2PredictionQuantile(options)
  } else {
    optionsSequence <- .sapOptions2PredictionTime(options, tempFit, type, plot = TRUE)
  }

  out <- list()
  for (i in seq_along(fit)) {

    # skip model on error
    if (jaspBase::isTryError(fit[[i]]))
      next

    if (type == "quantile") {
      data  <- summary(fit[[i]], type = type, quantiles = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]])
    } else {
      data  <- summary(fit[[i]], type = type, t = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]])
    }

    # deal with potentially multiple predictions
    for (j in seq_along(data)) {

      # rename output
      colnames(data[[j]]) <- c("at", "estimate", "lCi", "uCi")

      if (type == "survival" && options[["survivalProbabilityAsFailureProbability"]]) {
        data[[j]]$estimate <- 1 - data[[j]]$estimate
        data[[j]]$lCi      <- 1 - data[[j]]$lCi
        data[[j]]$uCi      <- 1 - data[[j]]$uCi
      }

      # add factor level
      if (length(data) > 1) {
        data[[j]]$Level <- decodeColNames(names(data)[j])
      } else {
        data[[j]]$Level <- NA
      }

      # add distribution information
      data[[j]]$Distribution <- attr(fit[[i]], "distribution")
    }

    # bind across levels
    out[[i]] <- do.call(rbind, data)
  }

  # bind across models
  out <- do.call(rbind, out)

  # set any Inf to NA
  out[["estimate"]][is.infinite(out[["estimate"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])] <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])] <- NA

  # check how to distribute legend
  hasDistribution <- length(unique(out[["Distribution"]])) > 1
  hasLevel        <- length(unique(out[["Level"]])) > 1

  # compute Kaplan-Meier if needed
  if (type == "survival" && options[["survivalProbabilityPlotAddKaplanMeier"]] && options[["censoringType"]] == "right") {

    kmFit    <- try(survfit(
      formula = .saGetFormula(options, type = "KM"),
      type    = "kaplan-meier",
      data    = tempData
    ))
    kmTable <- summary(kmFit) # , times = optionsSequence
    kmTable <- with(kmTable, data.frame(
      at       = time,
      estimate = surv,
      lCi      = lower,
      uCi      = upper
    ))

    if (options[["survivalProbabilityAsFailureProbability"]]) {
      kmTable$estimate <- 1 - kmTable$estimate
      kmTable$lCi      <- 1 - kmTable$lCi
      kmTable$uCi      <- 1 - kmTable$uCi
    }

    # transform into a step function
    kmTable    <- kmTable[rep(1:nrow(kmTable), each=2), ]
    kmTable$at[1:(nrow(kmTable)-1)] <- kmTable$at[2:nrow(kmTable)]

    # extend the last step to match the last data point
    if (max(kmTable$at) < max(tempData[[options[["timeToEvent"]]]])) {
      kmTable <- rbind(kmTable, data.frame(
        at       = max(tempData[[options[["timeToEvent"]]]]),
        estimate = kmTable[["estimate"]][nrow(kmTable)],
        lCi      = kmTable[["lCi"]][nrow(kmTable)],
        uCi      = kmTable[["uCi"]][nrow(kmTable)]
      ))
    }
  }

  # create a plot
  plot <- ggplot2::ggplot(data = out)

  # add censoring observations if requested
  if (type == "survival" && options[["survivalProbabilityPlotAddCensoringEvents"]] && options[["censoringType"]] == "right") {
    plot <- plot + ggplot2::geom_rug(
      data    = data.frame(censoring = tempData[[options[["timeToEvent"]]]][!tempData[[options[["eventStatus"]]]]]),
      mapping = ggplot2::aes(x = censoring),
      sides = "b", color = "grey60", alpha = 0.5, size = 0.5
    )
  }

  # add Kaplan-Meier if needed
  if (type == "survival" && options[["survivalProbabilityPlotAddKaplanMeier"]] && options[["censoringType"]] == "right") {

    if (options[["predictionsConfidenceInterval"]]) {
      aesCall <- list(
        x        = as.name("at"),
        ymin     = as.name("lCi"),
        ymax     = as.name("uCi")
      )
      geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = kmTable, fill = "grey60",  color = "grey60", alpha = 0.10)
      plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
    }

    aesCall <- list(
      x        = as.name("at"),
      y        = as.name("estimate")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = kmTable, color = "grey60")
    plot <- plot + do.call(jaspGraphs::geom_line, geomCall)

  }

  # add CI
  if (options[["predictionsConfidenceInterval"]]) {
    aesCall <- list(
      x        = as.name("at"),
      ymin     = as.name("lCi"),
      ymax     = as.name("uCi"),
      fill     = if (hasDistribution) as.name("Distribution") else if (hasLevel) as.name("Level"),
      linetype = if (hasDistribution && hasLevel) as.name("Level")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), alpha = 0.30)
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)

  }

  # add line
  aesCall <- list(
    x        = as.name("at"),
    y        = as.name("estimate"),
    color    = if (hasDistribution) as.name("Distribution") else if (hasLevel) as.name("Level"),
    linetype = if (hasDistribution && hasLevel) as.name("Level")
  )
  geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]))
  plot <- plot + do.call(jaspGraphs::geom_line, geomCall)

  if (hasDistribution || hasLevel) {
    plot <- plot +
      jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
      jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])
  }

  # scale axis & add labels
  if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] == "log") {
    xBreaks <- exp(seq(log(min(out[["at"]], na.rm = TRUE)), log(max(out[["at"]], na.rm = TRUE)), length.out = 5))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(out[["at"]], na.rm = TRUE))
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    out[["estimate"]],
    if (options[["predictionsConfidenceInterval"]]) out[["lCi"]],
    if (options[["predictionsConfidenceInterval"]]) out[["uCi"]]), na.rm = TRUE))

  if (type == "survival") {
    plot <- .sapPredictionPlotAddSurvivalAxis(plot, options, xBreaks, yBreaks, atTitle, estimateTitle)
  } else {
    plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep) +
      jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep) +
      ggplot2::ylab(estimateTitle) + ggplot2::xlab(atTitle)
  }

  # themes
  if (type != "survival" && options[["plotTheme"]] == "detailed") {
    options[["plotTheme"]] <- "jasp"
  }
  if (options[["plotTheme"]] == "jasp") {
    plot <- plot + jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = options[["plotLegend"]])
  } else {
    plot <- plot +
      switch(
        options[["plotTheme"]],
        "whiteBackground" = ggplot2::theme_bw()       + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "light"           = ggplot2::theme_light()    + ggplot2::theme(legend.position = options[["plotLegend"]]),,
        "detailed"        = ggplot2::theme_light()    + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "minimal"         = ggplot2::theme_minimal()  + ggplot2::theme(legend.position = options[["plotLegend"]]),
        "pubr"            = jaspGraphs::themePubrRaw(legend = options[["plotLegend"]]),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = switch(
          options[["plotTheme"]],
          "none"   = "none",
          "bottom" = "bottommiddle",
          "right"  = "bottomright",
          "top"    = "topmiddle",
          "left"   = "bottomleft"
        ))
      )
  }

  tempPlot <- createJaspPlot(width = if (hasDistribution || hasLevel) 550 else 400, height = 320)
  tempPlot$plotObject <- plot

  return(tempPlot)
}

.sapPredictionPlotAddSurvivalAxis <- function(plot, options, xBreaks, yBreaks, atTitle, estimateTitle) {

  ### x-axis
  if (options[["survivalProbabilityPlotTransformXAxis"]] == "none") {
    # no transformation
    plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep)

  } else if (options[["survivalProbabilityPlotTransformXAxis"]] == "log") {
    # log transformation
    atTitle <- gettextf("%1$s (log scale)", atTitle)
    plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), trans = "log", oob = scales::oob_keep)

  }

  ### y-axis
  if (options[["survivalProbabilityPlotTransformYAxis"]] == "none") {
    # no transformation
    if (options[["plotTheme"]] == "detailed") {

      yRange    <- c(0, 1)
      yBreaks   <- seq(0, 1, by = 0.1)

      plot <- plot + ggplot2::scale_y_continuous(
        breaks = yBreaks, limits = yRange, oob = scales::oob_keep,
        minor_breaks = seq(0, 1, by = 0.05)
      )

    } else {

      plot <- plot + jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep)

    }

  } else if (options[["survivalProbabilityPlotTransformYAxis"]] == "log") {
    # log transformation
    estimateTitle <- gettextf("%1$s (log scale)", estimateTitle)

    if (options[["plotTheme"]] == "detailed") {

      yRange    <- c(0.001, 1)
      yBreaks   <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.10, 0.50, 0.80, 0.90)

      plot <- plot + ggplot2::scale_y_continuous(
        breaks = yBreaks, limits = yRange, oob = scales::oob_keep,
        minor_breaks = c(1:20/1000, 2:10/100, 1:9/10),
        transform = "log"
      )

    } else {

      yRange    <- range(yBreaks)
      yRange[1] <- max(0.01, yRange[1])
      yBreaks   <- exp(seq(log(yRange[1]), log(yRange[2]), length.out = 7))

      plot <- plot + jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = yRange, oob = scales::oob_keep, trans = "log")

    }

  } else if (options[["survivalProbabilityPlotTransformYAxis"]] == "logmlogmp") {
    # log-log transformation
    logmlogmp    <- function(x) log(-log(1-x))
    logmlogmpInv <- function(x) exp(-exp(x)) * (exp(exp(x))-1)
    estimateTitle <- gettextf("%1$s (log(log(p)) scale)", estimateTitle)

    if (options[["plotTheme"]] == "detailed") {

      yRange    <- c(0.001, 0.999)
      yBreaks   <- (c(0.001, 0.005, 0.02, 0.05, 0.10, 0.50, 0.80, 0.90, 0.95, 0.98, 0.99, 0.999))

      plot <- plot + ggplot2::scale_y_continuous(
        breaks = (yBreaks), limits = (yRange), oob = scales::oob_keep,
        minor_breaks = c(99:90/100, 8:1/10, 9:1/100),
        transform = scales::new_transform(name = "logmlogp", transform = logmlogmp, inverse = logmlogmpInv)
      )

    } else {

      yRange    <- range(yBreaks)
      yRange[1] <- max(0.01, yRange[1])
      yRange[2] <- min(0.99, yRange[2])
      yBreaks   <- logmlogmpInv(seq(logmlogmp(yRange[2]), logmlogmp(yRange[1]), length.out = 7))

      plot <- plot + jaspGraphs::scale_y_continuous(
        breaks = (yBreaks), limits = (yRange), oob = scales::oob_keep,
        trans = scales::new_transform(name = "logmlogp", transform = logmlogmp, inverse = logmlogmpInv)
      )
    }
  }

  plot <- plot + ggplot2::ylab(estimateTitle) + ggplot2::xlab(atTitle)
  return(plot)
}


.sapSurvivalTimePlotFun         <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "quantile")
  return(tempPlot)
}
.sapSurvivalProbabilityPlotFun  <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "survival")
  return(tempPlot)
}
.sapHazardPlotFun               <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "hazard")
  return(tempPlot)
}
.sapCumHazardPlotFun            <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "cumhaz")
  return(tempPlot)
}
.sapRmstPlotFun                 <- function(fit, options) {

  tempPlot <- .sapCreatePredictionPlotWrapper(fit, options, type = "rmst")
  return(tempPlot)
}

# diagnostics plots
.sapResidualsVsTimePlot                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualsVsTimePlot"]]) || options[["censoringType"]] != "right")
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualVsTime", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualsVsTimePlotFun,
    name          = "residualsVsTimePlot",
    title         = gettext("Residuals vs. Time"),
    dependencies  = outputDependencies,
    position      = 5.1
  )

  return()
}
.sapResidualsVsPredictorsPlot          <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualsVsPredictorsPlot"]]))
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualVsPredictors", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualsVsPredictorsPlotFun,
    name          = "residualsVsPredictorsPlot",
    title         = gettext("Residuals vs. Predictors"),
    dependencies  = outputDependencies,
    position      = 5.2
  )

  return()
}
.sapResidualsVsPredictedPlot           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualVsPredictedPlot"]]))
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualVsPredicted", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualsVsPredictedPlotFun,
    name          = "residualVsPredictedPlot",
    title         = gettext("Residuals vs. Predicted"),
    dependencies  = outputDependencies,
    position      = 5.3
  )

  return()
}
.sapResidualHistogramPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualHistogram"]]))
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualHistogram", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualHistogramPlotFun,
    name          = "residualHistogram",
    title         = gettext("Residual Histogram"),
    dependencies  = outputDependencies,
    position      = 5.4
  )

  return()
}

.sapResidualsVsTimePlotFun       <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  tempPlot$plotObject <- try(.saspResidualsPlot(time, res, gettext("Time"), switch(
    options[["residualPlotResidualType"]],
    "response" = gettext("Response"),
    "coxSnell" = gettext("Cox-Snell")
  )))

  return(tempPlot)
}
.sapResidualsVsPredictorsPlotFun <- function(fit, options) {

  residualPlotResidualVsPredictors <- createJaspContainer()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(residualPlotResidualVsPredictors)

  # extract the dataset and compute residuals
  predictorsFit <- model.matrix(fit)
  res           <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  for (i in seq_len(ncol(predictorsFit))) {
    tempPredictorName <- .saTermNames(colnames(predictorsFit)[i], variables = c(options[["covariates"]], options[["factors"]]))
    residualPlotResidualVsPredictors[[paste0("residualPlotResidualVsPredictors", i)]] <- createJaspPlot(
      plot         = .saspResidualsPlot(x = predictorsFit[,i], y = res, xlab = tempPredictorName, ylab = switch(
        options[["residualPlotResidualType"]],
        "response" = gettext("Response"),
        "coxSnell" = gettext("Cox-Snell")
      )),
      title        = gettextf("Residuals vs. %1$s", tempPredictorName),
      position     = i,
      width        = 450,
      height       = 320
    )
  }

  return(residualPlotResidualVsPredictors)
}
.sapResidualsVsPredictedPlotFun  <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  pred    <- unlist(predict(fit))
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  tempPlot$plotObject <- try(.saspResidualsPlot(pred, res, gettext("Predicted Time"), switch(
    options[["residualPlotResidualType"]],
    "response" = gettext("Response"),
    "coxSnell" = gettext("Cox-Snell")
  )))

  return(tempPlot)
}
.sapResidualHistogramPlotFun     <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  tempPlot$plotObject <- try(jaspGraphs::jaspHistogram(res, xName =switch(
    options[["residualPlotResidualType"]],
    "response" = gettext("Response"),
    "coxSnell" = gettext("Cox-Snell")
  )))

  return(tempPlot)
}

# adding rows to the output
.sapRowModelInformation               <- function(fit) {
  return(data.frame(
    subgroup     = attr(fit, "subgroup"),
    model        = attr(fit, "modelTitle"),
    distribution = attr(fit, "distribution")
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
      distribution = attr(fit0, "distribution")
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
    covMat
  ))
}

# adding columns to tables
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
.sapCreatePredictionTable     <- function(options, atTitle, estimateNames, estimateTitles) {

  tempTable <- createJaspTable()
  .sapAddColumnSubgroup(     tempTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( tempTable, options, output = "coefficientsCovarianceMatrix")
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
.sapOptions2PredictionQuantile  <- function(options) {

  if (options[["predictionsSurvivalTimeStepsType"]] == "quantiles") {

    setQuantiles <- seq(0, 1, length.out = options[["predictionsSurvivalTimeStepsNumber"]] + 1)
    setQuantiles <- setQuantiles[-length(setQuantiles)] # don't predict for 1 as it is infinity

  } else if (options[["predictionsSurvivalTimeStepsType"]] == "sequence") {

    setQuantiles <- seq(options[["predictionsSurvivalTimeStepsFrom"]], options[["predictionsSurvivalTimeStepsTo"]], options[["predictionsSurvivalTimeStepsSize"]])
    setQuantiles <- setQuantiles[-length(setQuantiles)]

  } else if (options[["predictionsSurvivalTimeStepsType"]] == "custom") {

    setQuantiles <- options[["predictionsSurvivalTimeCustom"]]
    setQuantiles <- .sapCleanCustomOptions(setQuantiles, gettext("Custom steps for predicted survival time were specified in an incorrect format. Try '0.25, 0.50, 0.75'."))
    setQuantiles <- sort(setQuantiles)
    if (any(setQuantiles < 0 | setQuantiles > 1))
      .quitAnalysis(gettext("Custom steps for predicted survival time must be between 0 and 1."))

  }

  return(setQuantiles)
}
.sapOptions2PredictionTime      <- function(options, fit, type = "uknown", plot = FALSE) {

  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  minTime <- min(time[time > 0])
  maxTime <- max(time[time < Inf])

  # plotting preset which makes the plots look smoother than the generated tables
  if (plot) {
    options[["predictionsLifeTimeStepsNumber"]] <- 101
  }

  if (options[["predictionsLifeTimeStepsType"]] == "quantiles") {

    # special treatment for setting limits when survival plot with transformation is used
    if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
      setTime <- exp(seq(log(minTime), log(maxTime), length.out = options[["predictionsLifeTimeStepsNumber"]]))
    } else {
      setTime <- seq(0, maxTime, length.out = options[["predictionsLifeTimeStepsNumber"]])
      if (options[["predictionsLifeTimeRoundSteps"]])
        setTime <- unique(round(setTime))
    }

  } else if (options[["predictionsLifeTimeStepsType"]] == "sequence") {

    stepFrom <- options[["predictionsLifeTimeStepsFrom"]]
    stepSize <- options[["predictionsLifeTimeStepsSize"]]
    stepTo   <- options[["predictionsLifeTimeStepsTo"]]

    if (stepFrom != "") {
      stepFrom <- as.numeric(trimws(stepFrom, which = "both"))
      if (is.na(stepFrom) || stepFrom <= 0)
        .quitAnalysis(gettext("Step from for predicted survival time must be a positive number."))
    } else {
      stepTo <- 0
    }
    if (stepTo != "") {
      stepTo <- as.numeric(trimws(stepTo, which = "both"))
      if (is.na(stepTo) || stepTo <= 0)
        .quitAnalysis(gettext("Step to for predicted survival time must be a positive number."))
    } else {
      stepTo <- maxTime
    }
    if (stepSize != "") {
      stepSize <- as.numeric(trimws(stepSize, which = "both"))
      if (is.na(stepSize) || stepSize <= 0)
        .quitAnalysis(gettext("Step size for predicted survival time must be a positive number."))
    } else {
      stepSize <- (stepTo - stepFrom) / 10
    }

    # special treatment for setting limits when survival plot with transformation is used
    if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
      if (stepFrom == 0) {
        stepFrom <- minTime
      }
      if (plot) {
        setTime <- seq(stepFrom, stepTo, length.out = options[["predictionsLifeTimeStepsNumber"]])
      } else {
        setTime <- seq(stepFrom, stepTo, stepSize)
      }
    } else {
      if (plot) {
        if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
          setTime <- exp(seq(log(stepFrom), log(stepTo), length.out = options[["predictionsLifeTimeStepsNumber"]]))
        } else {
          setTime <- seq(stepFrom, stepTo, length.out = options[["predictionsLifeTimeStepsNumber"]])
        }
      } else {
        setTime <- seq(stepFrom, stepTo, stepSize)
        if (options[["predictionsLifeTimeRoundSteps"]])
          setTime <- unique(round(setTime))
      }
    }

  } else if (options[["predictionsLifeTimeStepsType"]] == "custom") {

    setTime <- options[["predictionsLifeTimeCustom"]]
    setTime <- .sapCleanCustomOptions(setTime, gettext("Custom steps for predicted survival time were specified in an incorrect format. Try '0.25, 0.50, 0.75'."))
    setTime <- sort(setTime)
    if (any(setTime < 0))
      .quitAnalysis(gettext("Custom steps for predicted survival time must be greater than or equal to 0."))

    # special treatment for setting limits when survival plot with transformation is used
    if (type == "survival" && options[["survivalProbabilityPlotTransformXAxis"]] %in% c("log")) {
      setTime[setTime <= 0] <- minTime
    }

    if (plot) {
      setTime <- seq(min(setTime), max(setTime), length.out = options[["predictionsLifeTimeStepsNumber"]])
    }
  }

  return(setTime)
}
.sapCleanCustomOptions          <- function(x, message) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  if (anyNA(as.numeric(x)))
    .quitAnalysis(message)

  return(as.numeric(x))
}

# table messages
.sapCollectFitErrors      <- function(fit, options) {

  errors <- NULL

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]))
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

  if (options[["distribution"]] %in% c("bestAic", "bestBic") && !options[["interpretModel"]] %in% c("bestAic", "bestBic") && options[["compareModelsAcrossDistributions"]]) {

    selected <- which.min(data[[switch(
      options[["distribution"]],
      "bestAic" = "aic",
      "bestBic" = "bic"
    )]])
    message <- gettextf("All following output is based on the best fitting %1$s distribution.", data[["distribution"]][selected])

  } else {

    message <- NULL

  }

  return(message)

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
