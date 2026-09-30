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
      attr(subgroupDataset, "missingObservations") <- .saMissingObservations(dataset)

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
