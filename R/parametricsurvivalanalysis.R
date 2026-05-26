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

  # Censoring summary table
  if (options[["censoringSummary"]])
    .saCensoringSummaryTable(jaspResults, dataset, options)

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
  if (isTRUE(options[["probabilityPlot"]]))
    .sapProbabilityPlot(jaspResults, options)

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
    data  <- try(summary(fit, type = type, quantiles = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
  } else {
    optionsSequence <- .sapOptions2PredictionTime(options, fit)
    data  <- try(summary(fit, type = type, t = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
  }

  # error handling for divergent integrals
  if (jaspBase::isTryError(data)) {
    tempTable <- .sapCreatePredictionTable(options, atTitle = atTitle, estimateNames = "", estimateTitles = estimateTitle)
    tempTable$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempTable)
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
    data[["survivalProbability"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "survival", timeSequence = timeSequence))

    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["survivalProbability"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(gettext("The model failed to produce survival predictions. Consider simplifying the model."))
      return(tempTable)
    }
  }

  if (options[["hazardTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Hazard"), estimateName = "hazard.")
    data[["hazard"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "hazard", timeSequence = timeSequence))


    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["hazard"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(gettext("The model failed to produce hazard predictions. Consider simplifying the model."))
      return(tempTable)
    }
  }

  if (options[["cumulativeHazardTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Cumulative Hazard"), estimateName = "cumulativeHazard.")
    data[["cumulativeHazard"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "cumhaz", timeSequence = timeSequence))


    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["cumulativeHazard"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(gettext("The model failed to produce cumulative predictions. Consider simplifying the model."))
      return(tempTable)
    }
  }

  if (options[["restrictedMeanSurvivalTimeTable"]]) {
    .sapAddColumnsPredictionTable(tempTable, options, estimateTitle = gettext("Restricted Mean Survival Time"), estimateName = "restrictedMeanSurvivalTime.")
    data[["restrictedMeanSurvivalTime"]] <- try(.sapLifeTimeTableWrapper(fit, options, type = "rmst", timeSequence = timeSequence))


    # error handling for divergent integrals
    if (jaspBase::isTryError(data[["restrictedMeanSurvivalTime"]])) {
      tempTable <- createJaspTable()
      tempTable$setError(gettext("The model failed to produce restricted mean survival time predictions. Consider simplifying the model."))
      return(tempTable)
    }
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
      data  <- try(summary(fit[[i]], type = type, quantiles = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
    } else {
      data  <- try(summary(fit[[i]], type = type, t = optionsSequence, ci = TRUE, cl = options[["predictionsConfidenceIntervalLevel"]]))
    }

    # error handling for divergent integrals
    if (jaspBase::isTryError(data)) {
      tempPlot <- createJaspPlot(title = estimateTitle)
      tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
      return(tempPlot)
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
    estimateTitle <- gettextf("%1$s (log(-log(1-p)) scale)", estimateTitle)

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
.sapProbabilityPlot                   <- function(jaspResults, options) {

  if (!is.null(jaspResults[["probabilityPlot"]]) || options[["censoringType"]] != "right")
    return()

  # Probability-paper diagnostics are distribution-level checks by default.
  # When requested, group the selected distributions by model/subgroup and
  # overlay them in one canvas, following the prediction-plot merge pattern.
  if (isTRUE(options[["probabilityPlotMergePlotsAcrossDistributions"]]) && options[["distribution"]] %in% "all" && !options[["interpretModel"]] %in% c("bestAic", "bestBic")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel")
    fit <- .sapProbabilityPlotFilterSelectedModel(fit, options)
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapFlattenFit(fit, options)
  }

  outputDependencies <- c(
    .sapDependencies, "interpretModel", "compareModelsAcrossDistributions", "alwaysDisplayModelInformation",
    "probabilityPlot", "probabilityPlotCanvas", "probabilityPlotEmpiricalPoints",
    "probabilityPlotPointCoordinates", "probabilityPlotFittedCurve",
    "probabilityPlotMergePlotsAcrossDistributions",
    "probabilityPlotConfidenceInterval", "probabilityPlotConfidenceIntervalLevel",
    "probabilityPlotGrid", "probabilityPlotPlottingPosition", "probabilityPlotRankAdjustment",
    "probabilityPlotTiesHandler", "probabilityPlotLegend", "probabilityPlotColorPalette",
    "probabilityPlotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapProbabilityPlotFun,
    name          = "probabilityPlot",
    title         = gettext("Probability Plot"),
    dependencies  = outputDependencies,
    position      = 5.5
  )

  return()
}

.sapProbabilityPlotFilterSelectedModel <- function(fit, options) {

  if (!.sapMultipleModels(options) || options[["interpretModel"]] %in% c("all", "bestAic", "bestBic"))
    return(fit)

  keep <- vapply(fit, function(fitGroup) {
    modelIds <- vapply(fitGroup, function(x) {
      modelId <- attr(x, "modelId")
      if (is.null(modelId) || length(modelId) == 0 || is.na(modelId[1]))
        return(NA_character_)
      return(as.character(modelId[1]))
    }, character(1))

    return(any(modelIds == options[["interpretModel"]], na.rm = TRUE))
  }, logical(1))

  return(fit[keep])
}

.sapProbabilityPlotFun <- function(fit, options) {

  fitList  <- .sapProbabilityPlotAsFitList(fit)
  fitValid <- .sapProbabilityPlotValidFits(fitList)

  tempPlot <- createJaspPlot(width = if (length(fitValid) > 1) 620 else 520, height = 420)

  if (length(fitValid) == 0)
    return(tempPlot)

  tempPlot$plotObject <- try(.sapCreateProbabilityPlot(fitList, options))

  if (jaspBase::isTryError(tempPlot$plotObject))
    tempPlot$setError(gettext("The model failed to produce a probability plot. Consider simplifying the model."))

  return(tempPlot)
}

.sapProbabilityPlotAsFitList <- function(fit) {

  if (is.null(fit))
    return(list())

  if (jaspBase::isTryError(fit) || inherits(fit, "flexsurvreg"))
    return(list(fit))

  return(fit)
}

.sapProbabilityPlotValidFits <- function(fit) {

  fitList <- .sapProbabilityPlotAsFitList(fit)

  keep <- vapply(fitList, function(x) !is.null(x) && !jaspBase::isTryError(x) && length(x) > 0, logical(1))
  return(fitList[keep])
}

.sapCreateProbabilityPlot <- function(fit, options) {

  fitList <- .sapProbabilityPlotValidFits(fit)

  if (length(fitList) == 0)
    stop(gettext("The probability plot requires at least one fitted model."))

  dataset <- attr(fitList[[1]], "dataset")
  observedTimeRange <- .sapProbabilityPlotTimeRange(.saExtractSurvTimes(dataset, options))
  timeSequence <- .sapProbabilityPlotTimeSequence(observedTimeRange, options)

  empiricalData <- data.frame(time = numeric(0), probability = numeric(0), label = character(0))
  if (options[["probabilityPlotEmpiricalPoints"]])
    empiricalData <- .sapProbabilityPlotEmpiricalData(dataset, options)

  curveData <- .sapProbabilityPlotEmptyCurveData()
  if (options[["probabilityPlotFittedCurve"]])
    curveData <- .sapProbabilityPlotCurveData(fitList, options, timeSequence)

  if (nrow(empiricalData) == 0 && nrow(curveData) == 0)
    stop(gettext("The probability plot requires at least one positive observed failure time or a fitted curve."))

  hasDistribution <- nrow(curveData) > 0 && length(unique(stats::na.omit(curveData[["Distribution"]]))) > 1
  hasLevel        <- nrow(curveData) > 0 && length(unique(stats::na.omit(curveData[["Level"]]))) > 1
  hasGroup        <- nrow(curveData) > 0 && length(unique(stats::na.omit(curveData[["Group"]]))) > 1

  plot <- ggplot2::ggplot()

  if (nrow(curveData) > 0 && options[["probabilityPlotConfidenceInterval"]]) {
    aesCall <- list(
      x     = as.name("time"),
      ymin  = as.name("lCi"),
      ymax  = as.name("uCi"),
      fill  = if (hasDistribution) as.name("Distribution") else if (hasLevel) as.name("Level"),
      group = if (hasGroup) as.name("Group")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = curveData, alpha = 0.22)
    if (!hasDistribution && !hasLevel)
      geomCall[["fill"]] <- "grey60"
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
  }

  if (nrow(curveData) > 0) {
    aesCall <- list(
      x        = as.name("time"),
      y        = as.name("probability"),
      color    = if (hasDistribution) as.name("Distribution") else if (hasLevel) as.name("Level"),
      linetype = if (hasDistribution && hasLevel) as.name("Level"),
      group    = if (hasGroup) as.name("Group")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = curveData)
    if (!hasDistribution && !hasLevel)
      geomCall[["color"]] <- "black"
    plot <- plot + do.call(jaspGraphs::geom_line, geomCall)
  }

  if (nrow(empiricalData) > 0 && options[["probabilityPlotEmpiricalPoints"]]) {
    plot <- plot + ggplot2::geom_point(
      data    = empiricalData,
      mapping = ggplot2::aes(x = time, y = probability),
      color   = "black",
      fill    = "white",
      shape   = 21,
      size    = 2.1,
      stroke  = 0.6
    )

    if (options[["probabilityPlotPointCoordinates"]]) {
      plot <- plot + ggplot2::geom_text(
        data    = empiricalData,
        mapping = ggplot2::aes(x = time, y = probability, label = label),
        hjust   = -0.05,
        vjust   = -0.45,
        size    = 2.5,
        color   = "grey20"
      )
    }
  }

  if (hasDistribution || hasLevel) {
    plot <- plot +
      jaspGraphs::scale_JASPcolor_discrete(options[["probabilityPlotColorPalette"]]) +
      jaspGraphs::scale_JASPfill_discrete(options[["probabilityPlotColorPalette"]])
  }

  plot <- .sapProbabilityPlotAddAxes(plot, empiricalData, curveData, options, observedTimeRange)
  plot <- .sapProbabilityPlotAddTheme(plot, options)

  return(plot)
}

.sapProbabilityPlotTimeSequence <- function(timeRange, options) {

  if (.sapProbabilityPlotIsDetailed(options))
    timeRange <- .sapProbabilityPlotDetailedTimeRange(timeRange)

  return(exp(seq(log(timeRange[1]), log(timeRange[2]), length.out = 101)))
}

.sapProbabilityPlotEmpiricalData <- function(dataset, options) {

  time  <- .saExtractSurvTimes(dataset, options)
  event <- dataset[[options[["eventStatus"]]]]

  if (!is.null(options[["weights"]]) && options[["weights"]] != "") {
    weights <- dataset[[options[["weights"]]]]
  } else {
    weights <- rep(1L, length(time))
  }

  keep <- is.finite(time) & time > 0 & !is.na(event) & is.finite(weights) & weights > 0
  time <- time[keep]
  event <- as.logical(event[keep])
  weights <- as.integer(weights[keep])

  if (length(time) == 0 || !any(event))
    return(data.frame(time = numeric(0), probability = numeric(0), label = character(0)))

  if (any(weights > 1L)) {
    observationIndex <- rep.int(seq_along(time), weights)
    time <- time[observationIndex]
    event <- event[observationIndex]
  }

  out <- .sapProbabilityPlotRankData(time, event, options)

  if (nrow(out) == 0)
    return(out)

  out[["probability"]] <- .sapProbabilityPlotClampProbability(out[["probability"]])
  out[["label"]]       <- sprintf("%s, %s", .sapProbabilityPlotTimeLabel(out[["time"]]), .sapProbabilityPlotProbabilityLabel(out[["probability"]]))
  rownames(out)        <- NULL

  return(out)
}

.sapProbabilityPlotRankData <- function(time, event, options) {

  orderedData <- data.frame(time = time, event = event)
  orderedData <- orderedData[order(orderedData[["time"]], !orderedData[["event"]]), , drop = FALSE]

  n <- nrow(orderedData)
  if (options[["probabilityPlotRankAdjustment"]] == "kaplanMeier") {
    rankData <- .sapProbabilityPlotKaplanMeierAdjustedRanks(orderedData)
  } else {
    rankData <- .sapProbabilityPlotJohnsonAdjustedRanks(orderedData)
  }

  if (nrow(rankData) == 0)
    return(data.frame(time = numeric(0), probability = numeric(0), label = character(0)))

  rankData <- .sapProbabilityPlotHandleTies(rankData, options[["probabilityPlotTiesHandler"]])

  probability <- switch(
    options[["probabilityPlotPlottingPosition"]],
    "median"      = stats::qbeta(0.5, rankData[["adjustedRank"]], n - rankData[["adjustedRank"]] + 1),
    "benard"      = (rankData[["adjustedRank"]] - 0.3) / (n + 0.4),
    "hazen"       = (rankData[["adjustedRank"]] - 0.5) / n,
    "mean"        = rankData[["adjustedRank"]] / (n + 1),
    "kaplanMeier" = .sapProbabilityPlotKaplanMeierPlottingPosition(rankData[["adjustedRank"]], n),
    "blom"        = (rankData[["adjustedRank"]] - 0.375) / (n + 0.25),
    stats::qbeta(0.5, rankData[["adjustedRank"]], n - rankData[["adjustedRank"]] + 1)
  )

  out <- data.frame(time = rankData[["time"]], probability = probability)
  rownames(out) <- NULL

  return(out)
}

.sapProbabilityPlotJohnsonAdjustedRanks <- function(orderedData) {

  n <- nrow(orderedData)
  adjustedRank <- numeric(0)
  failureTime  <- numeric(0)
  previousRank <- 0

  for (i in seq_len(n)) {
    if (!orderedData[["event"]][i])
      next

    reverseRank <- n - i + 1
    previousRank <- previousRank + (n + 1 - previousRank) / (reverseRank + 1)

    adjustedRank <- c(adjustedRank, previousRank)
    failureTime  <- c(failureTime, orderedData[["time"]][i])
  }

  return(data.frame(time = failureTime, adjustedRank = adjustedRank))
}

.sapProbabilityPlotKaplanMeierAdjustedRanks <- function(orderedData) {

  n <- nrow(orderedData)
  adjustedRank <- numeric(0)
  previousRank <- 0

  for (i in seq_len(n)) {
    if (orderedData[["event"]][i]) {
      currentRank <- 1 - ((1 - previousRank) * (n - i) / (n - i + 1))
    } else {
      currentRank <- previousRank
    }

    adjustedRank <- c(adjustedRank, currentRank)
    previousRank <- currentRank
  }

  out <- data.frame(
    time         = orderedData[["time"]][orderedData[["event"]]],
    adjustedRank = adjustedRank[orderedData[["event"]]]
  )

  # WeibullR follows the Minitab convention of moving a final complete failure
  # just below 100%, so the point remains finite on probability paper.
  if (nrow(out) > 1 && isTRUE(all.equal(out[["adjustedRank"]][nrow(out)], 1)))
    out[["adjustedRank"]][nrow(out)] <- 1 - ((1 - out[["adjustedRank"]][nrow(out) - 1]) / 10)

  out[["adjustedRank"]] <- out[["adjustedRank"]] * n

  return(out)
}

.sapProbabilityPlotHandleTies <- function(data, tiesHandler) {

  if (!tiesHandler %in% c("highest", "lowest", "mean", "sequential"))
    return(data)

  tiedRanks <- stats::aggregate(
    data[["adjustedRank"]],
    by = list(time = data[["time"]]),
    FUN = function(x) c(lowest = min(x), highest = max(x))
  )
  lowest  <- tiedRanks[["x"]][, "lowest"]
  highest <- tiedRanks[["x"]][, "highest"]

  out <- data.frame(
    time         = tiedRanks[["time"]],
    adjustedRank = switch(
      tiesHandler,
      "highest"    = highest,
      "lowest"     = lowest,
      "mean"       = (highest + lowest) / 2,
      "sequential" = highest - cumsum(highest - lowest)
    )
  )

  return(out[order(out[["time"]]), , drop = FALSE])
}

.sapProbabilityPlotKaplanMeierPlottingPosition <- function(adjustedRank, n) {

  probability <- adjustedRank / n

  # Same finite-endpoint convention as WeibullR/SuperSMITH for KM plotting positions.
  if (length(adjustedRank) > 0 && isTRUE(all.equal(adjustedRank[length(adjustedRank)], n)))
    probability[length(probability)] <- length(adjustedRank) / (n + 0.001)

  return(probability)
}

.sapProbabilityPlotCurveData <- function(fit, options, timeSequence) {

  fitList <- .sapProbabilityPlotValidFits(fit)
  if (length(fitList) == 0)
    return(.sapProbabilityPlotEmptyCurveData())

  ciLevel <- .sapProbabilityPlotConfidenceIntervalLevel(options)

  out <- list()
  for (i in seq_along(fitList)) {

    data <- summary(fitList[[i]], type = "survival", t = timeSequence, ci = TRUE, cl = ciLevel)

    for (j in seq_along(data)) {
      colnames(data[[j]]) <- c("time", "survival", "survivalLCI", "survivalUCI")

      data[[j]][["probability"]]  <- 1 - data[[j]][["survival"]]
      data[[j]][["lCi"]]          <- 1 - data[[j]][["survivalUCI"]]
      data[[j]][["uCi"]]          <- 1 - data[[j]][["survivalLCI"]]
      data[[j]][["Level"]]        <- if (length(data) > 1) decodeColNames(names(data)[j]) else NA_character_
      data[[j]][["Distribution"]] <- .sapProbabilityPlotDistributionLabel(fitList[[i]])
      data[[j]][["Group"]]        <- paste(data[[j]][["Distribution"]], data[[j]][["Level"]], sep = " | ")

      out[[length(out) + 1]] <- data[[j]][, c("time", "probability", "lCi", "uCi", "Level", "Distribution", "Group"), drop = FALSE]
    }
  }

  if (length(out) == 0)
    return(.sapProbabilityPlotEmptyCurveData())

  out <- do.call(rbind, out)
  out[["probability"]] <- .sapProbabilityPlotClampProbability(out[["probability"]])
  out[["lCi"]]         <- .sapProbabilityPlotClampProbability(out[["lCi"]])
  out[["uCi"]]         <- .sapProbabilityPlotClampProbability(out[["uCi"]])
  out[["time"]][is.infinite(out[["time"]])]               <- NA
  out[["probability"]][is.infinite(out[["probability"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])]                 <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])]                 <- NA
  out <- out[stats::complete.cases(out[, c("time", "probability")]) & out[["time"]] > 0, , drop = FALSE]
  rownames(out) <- NULL

  return(out)
}

.sapProbabilityPlotDistributionLabel <- function(fit) {

  distribution <- attr(fit, "distribution")
  if (is.null(distribution) || length(distribution) == 0 || is.na(distribution[1]))
    distribution <- gettext("Fitted")

  return(as.character(distribution[1]))
}

.sapProbabilityPlotEmptyCurveData <- function() {
  return(data.frame(
    time         = numeric(0),
    probability  = numeric(0),
    lCi          = numeric(0),
    uCi          = numeric(0),
    Level        = character(0),
    Distribution = character(0),
    Group        = character(0)
  ))
}

.sapProbabilityPlotAddAxes <- function(plot, empiricalData, curveData, options, observedTimeRange = NULL) {

  canvas   <- .sapProbabilityPlotCanvasTransform(options[["probabilityPlotCanvas"]])
  detailed <- .sapProbabilityPlotIsDetailed(options)

  if (detailed) {
    axisSetup <- .sapProbabilityPlotDetailedAxisSetup(empiricalData, curveData, observedTimeRange)
    plot <- plot + ggplot2::geom_hline(
      yintercept = canvas[["inverse"]](0),
      linetype   = 3,
      color      = "grey70",
      linewidth  = 0.35
    )
  } else {
    axisSetup <- .sapProbabilityPlotDefaultAxisSetup(empiricalData, curveData)
  }

  xScaleCall <- list(
    trans        = "log",
    breaks       = axisSetup[["xBreaks"]],
    minor_breaks = axisSetup[["xMinor"]],
    limits       = axisSetup[["timeRange"]],
    labels       = .sapProbabilityPlotTimeLabel,
    oob          = scales::oob_keep
  )
  yScaleCall <- list(
    trans        = scales::new_transform(name = canvas[["name"]], transform = canvas[["transform"]], inverse = canvas[["inverse"]]),
    breaks       = axisSetup[["yBreaks"]],
    minor_breaks = axisSetup[["yMinor"]],
    labels       = .sapProbabilityPlotProbabilityLabel,
    limits       = axisSetup[["probabilityRange"]],
    oob          = scales::oob_keep
  )

  if (detailed) {
    xScaleCall[["sec.axis"]] <- ggplot2::dup_axis(name = NULL, labels = .sapProbabilityPlotTimeLabel)
    yScaleCall[["sec.axis"]] <- ggplot2::dup_axis(name = NULL, labels = .sapProbabilityPlotProbabilityLabel)
  }

  plot <- plot +
    do.call(ggplot2::scale_x_continuous, xScaleCall) +
    do.call(ggplot2::scale_y_continuous, yScaleCall) +
    ggplot2::xlab(gettext("Time (log scale)")) +
    ggplot2::ylab(gettextf("Failure Probability (%s scale)", canvas[["label"]]))

  return(plot)
}

.sapProbabilityPlotDefaultAxisSetup <- function(empiricalData, curveData) {

  timeValues <- c(empiricalData[["time"]], curveData[["time"]])
  timeRange  <- .sapProbabilityPlotTimeRange(timeValues)

  return(list(
    timeRange        = timeRange,
    probabilityRange = .sapProbabilityPlotProbabilityRange(),
    xBreaks          = .sapProbabilityPlotTimeBreaks(timeRange),
    xMinor           = .sapProbabilityPlotTimeMinorBreaks(timeRange),
    yBreaks          = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.98, 0.99, 0.995, 0.999),
    yMinor           = sort(unique(c(seq(0.001, 0.009, by = 0.001), seq(0.01, 0.09, by = 0.01), seq(0.10, 0.90, by = 0.10), seq(0.91, 0.99, by = 0.01), 0.995, 0.999)))
  ))
}

.sapProbabilityPlotDetailedAxisSetup <- function(empiricalData, curveData, observedTimeRange = NULL) {

  if (is.null(observedTimeRange)) {
    timeValues <- c(empiricalData[["time"]], curveData[["time"]])
    observedTimeRange <- .sapProbabilityPlotTimeRange(timeValues)
  }
  timeRange <- .sapProbabilityPlotDetailedTimeRange(observedTimeRange)

  probabilityValues <- empiricalData[["probability"]]
  if (length(probabilityValues) == 0)
    probabilityValues <- curveData[["probability"]]

  probabilityRange <- .sapProbabilityPlotDetailedProbabilityRange(probabilityValues)
  probabilityGridRange <- c(probabilityRange[1] / 10, 1 - (1 - probabilityRange[2]) / 10)

  yBreaks <- sort(unique(c(
    .sapProbabilityPlotSeqProbability(probabilityGridRange[1], probabilityGridRange[2], c(1, 2, 5)),
    0.9
  )))
  yBreaks <- yBreaks[yBreaks >= probabilityRange[1] & yBreaks <= probabilityRange[2]]

  yMinor <- .sapProbabilityPlotSeqProbability(probabilityGridRange[1], probabilityGridRange[2], 1:9)
  yMinor <- yMinor[yMinor >= probabilityRange[1] & yMinor <= probabilityRange[2]]

  return(list(
    timeRange        = timeRange,
    probabilityRange = probabilityRange,
    xBreaks          = .sapProbabilityPlotSeqLog(timeRange[1] / 10, timeRange[2] * 10, c(1, 5)),
    xMinor           = .sapProbabilityPlotSeqLog(timeRange[1] / 10, timeRange[2] * 10, 1:10),
    yBreaks          = yBreaks,
    yMinor           = yMinor
  ))
}

.sapProbabilityPlotCanvasTransform <- function(canvas) {

  switch(
    canvas,
    "lognormal" = list(
      name      = "lognormalProbability",
      label     = gettext("log-normal"),
      transform = stats::qnorm,
      inverse   = stats::pnorm
    ),
    "loglogistic" = list(
      name      = "loglogisticProbability",
      label     = gettext("log-logistic"),
      transform = stats::qlogis,
      inverse   = stats::plogis
    ),
    list(
      name      = "weibullProbability",
      label     = gettext("Weibull"),
      transform = function(p) log(-log1p(-p)),
      inverse   = function(x) 1 - exp(-exp(x))
    )
  )
}

.sapProbabilityPlotAddTheme <- function(plot, options) {

  if (options[["probabilityPlotTheme"]] == "jasp") {
    plot <- plot + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  } else {
    plot <- plot +
      switch(
        options[["probabilityPlotTheme"]],
        "detailed"        = ggplot2::theme_light(),
        "whiteBackground" = ggplot2::theme_bw(),
        "light"           = ggplot2::theme_light(),
        "minimal"         = ggplot2::theme_minimal(),
        "pubr"            = jaspGraphs::themePubrRaw(legend = "none"),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = "none"),
        ggplot2::theme_light()
      )
  }

  legendTheme <- .sapProbabilityPlotLegendTheme(options[["probabilityPlotLegend"]])
  plot <- plot + legendTheme

  if (options[["probabilityPlotGrid"]]) {
    if (.sapProbabilityPlotIsDetailed(options)) {
      plot <- plot + ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey82", linewidth = 0.25),
        panel.grid.minor = ggplot2::element_line(color = "grey82", linewidth = 0.25)
      )
    } else {
      plot <- plot + ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.3),
        panel.grid.minor = ggplot2::element_line(color = "grey92", linewidth = 0.2)
      )
    }
  } else {
    plot <- plot + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  }

  return(plot)
}

.sapProbabilityPlotLegendTheme <- function(legendPosition) {

  legendPosition <- .sapProbabilityPlotLegendPosition(legendPosition)

  if (legendPosition == "none")
    return(ggplot2::theme(legend.position = "none"))

  return(ggplot2::theme(
    legend.position       = legendPosition,
    legend.background     = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_blank(),
    legend.key            = ggplot2::element_blank()
  ))
}

.sapProbabilityPlotLegendPosition <- function(legendPosition) {

  if (is.null(legendPosition))
    return("right")

  return(switch(
    legendPosition,
    "bottom"      = "bottom",
    "right"       = "right",
    "left"        = "left",
    "top"         = "top",
    "none"        = "none",
    "right"
  ))
}

.sapProbabilityPlotConfidenceIntervalLevel <- function(options) {

  level <- options[["probabilityPlotConfidenceIntervalLevel"]]
  if (is.null(level))
    level <- 0.90
  if (level > 1)
    level <- level / 100

  return(level)
}

.sapProbabilityPlotIsDetailed <- function(options) {
  return(identical(options[["probabilityPlotTheme"]], "detailed"))
}

.sapProbabilityPlotProbabilityRange <- function() {
  return(c(0.001, 0.999))
}

.sapProbabilityPlotDetailedProbabilityRange <- function(probability) {

  probability <- probability[is.finite(probability)]

  if (length(probability) == 0) {
    probabilityRange <- c(0.01, 0.99)
  } else if (min(probability) < 0.01) {
    probabilityRange <- c(signif(min(probability), 1), 0.99)
  } else {
    probabilityRange <- c(0.01, 0.99)
  }

  probabilityRange <- .sapProbabilityPlotClampProbability(probabilityRange)

  return(probabilityRange)
}

.sapProbabilityPlotClampProbability <- function(probability) {

  probabilityRange <- .sapProbabilityPlotProbabilityRange()
  return(pmin(pmax(probability, probabilityRange[1]), probabilityRange[2]))
}

.sapProbabilityPlotTimeRange <- function(time) {

  time <- time[is.finite(time) & time > 0]
  if (length(time) == 0)
    stop(gettext("The probability plot requires positive time values."))

  timeRange <- range(time)
  if (timeRange[1] == timeRange[2])
    timeRange <- c(timeRange[1] * 0.8, timeRange[2] * 1.2)

  return(timeRange)
}

.sapProbabilityPlotDetailedTimeRange <- function(timeRange) {

  return(c(
    10^(log10(timeRange[1]) - 0.5),
    10^(log10(timeRange[2]) + 1)
  ))
}

.sapProbabilityPlotTimeBreaks <- function(timeRange) {

  exponentRange <- seq(floor(log10(timeRange[1])), ceiling(log10(timeRange[2])))
  breaks <- as.vector(outer(c(1, 2, 5), 10^exponentRange, "*"))
  breaks <- sort(unique(breaks[breaks >= timeRange[1] & breaks <= timeRange[2]]))

  if (length(breaks) < 2) {
    breaks <- jaspGraphs::getPrettyAxisBreaks(timeRange)
    breaks <- breaks[breaks > 0]
  }

  return(breaks)
}

.sapProbabilityPlotSeqLog <- function(from, to, base = c(1, 2, 5)) {

  if (!is.finite(from) || !is.finite(to) || from <= 0 || to <= 0 || from >= to)
    return(numeric(0))

  exponentRange <- seq(floor(log10(from)), floor(log10(to)))
  breaks <- as.vector(outer(base, 10^exponentRange, "*"))
  breaks <- sort(unique(breaks[breaks >= from & breaks <= to & breaks > 0]))

  return(breaks)
}

.sapProbabilityPlotSeqProbability <- function(from, to, base = 1:9) {

  lower <- .sapProbabilityPlotSeqLog(from, 0.9, base)
  upper <- rev(1 - .sapProbabilityPlotSeqLog(1 - to, 0.1, base))
  if (length(upper) > 0)
    upper <- upper[-1]

  breaks <- c(lower, upper)
  breaks <- sort(unique(breaks[breaks >= from & breaks <= to]))

  return(breaks)
}

.sapProbabilityPlotTimeMinorBreaks <- function(timeRange) {

  exponentRange <- seq(floor(log10(timeRange[1])), ceiling(log10(timeRange[2])))
  breaks <- as.vector(outer(1:9, 10^exponentRange, "*"))
  breaks <- sort(unique(breaks[breaks >= timeRange[1] & breaks <= timeRange[2]]))

  return(breaks)
}

.sapProbabilityPlotProbabilityLabel <- function(probability) {

  return(paste0(formatC(100 * probability, format = "fg", digits = 4), "%"))
}

.sapProbabilityPlotTimeLabel <- function(time) {

  return(formatC(time, format = "fg", digits = 4))
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
  pred    <- try(unlist(predict(fit)))
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  if (jaspBase::isTryError(pred)) {
    tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempPlot)
  }

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
