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

  .sapFit(jaspResults, dataset, options)
  .sapSummaryTable(jaspResults, options)

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

    attr(dataset, "subgroup") <- gettext("Full dataset")

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
      attr(subgroupDataset, "subgroup") <- gettextf("Subgroup: %1$s", subgroupLevels[i])

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

    # store attributes
    attr(out[[i]], "subgroup")     <- attr(dataset, "subgroup")
    attr(out[[i]], "model")        <- options[["modelTerms"]][[i]][["title"]]
    attr(out[[i]], "modelTerms")   <- options[["modelTerms"]][[i]]
    attr(out[[i]], "distribution") <- .sapOption2DistributionName(distribution)
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

  return(fit)
}
.sapExtractFit          <- function(jaspResults, options) {

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

    # single model
    fit <- list(lapply(out, function(x) x[[1]][[1]]))
  }


  return(fit)
}


# summary tables
.sapSummaryTable        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution
  # (or joins them within subgroups if distributions / models are to be collapsed)
  fit <- .sapExtractFit(jaspResults, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "compareModelsAcrossDistributions", "modelSummaryRankModels", "modelSummaryRankModelsBy")

  if (length(fit) > 1) {

    summaryTable <- createJaspContainer(title = gettext("Model Summary Table"))
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
    summaryTable$title    <- gettext("Model Summary Table")
    summaryTable$dependOn(outputDependencies)
    summaryTable$position <- 1
    jaspResults[["summaryTable"]] <- summaryTable

  }

  return()
}
.sapSummaryTableFun     <- function(fit, options) {

  # create the table
  summaryTable <- createJaspTable()
  .sapAddColumnSubgroup(summaryTable, options)
  .sapAddColumnModel(summaryTable, options)
  .sapAddColumnDistribution(summaryTable, options)
  summaryTable$addColumnInfo(name = "logLik",        title = gettext("Log Lik."),     type = "number")
  summaryTable$addColumnInfo(name = "df",            title = gettext("df"),           type = "integer")
  summaryTable$addColumnInfo(name = "aic",           title = gettext("AIC"),          type = "number", format="dp:3")
  summaryTable$addColumnInfo(name = "bic",           title = gettext("BIC"),          type = "number", format="dp:3")
  if (options[["modelSummaryRankModels"]] && length(fit) > 1)
    summaryTable$addColumnInfo(name = "rank",        title = gettext("Rank"),         type = "integer")

  if (!.saSurvivalReady(options))
    return(summaryTable)

  # extract the data
  data <- .saSafeRbind(lapply(fit, .sapRowSummaryTable))

  # add postprocessing
  if (options[["modelSummaryRankModels"]] && !is.null(data[[options[["modelSummaryRankModelsBy"]]]])) {
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


.sapRowSummaryTable <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(data.frame(
      subgroup     = attr(fit, "subgroup"),
      model        = attr(fit, "model"),
      distribution = attr(fit, "distribution")
    ))

  return(data.frame(
    subgroup     = attr(fit, "subgroup"),
    model        = attr(fit, "model"),
    distribution = attr(fit, "distribution"),
    logLik = as.numeric(logLik(fit)),
    df     = attr(logLik(fit), "df"),
    aic    = AIC(fit),
    bic    = BIC(fit)
  ))
}

.sapAddColumnSubgroup     <- function(tempTable, options) {
  if (options[["subgroup"]] != "" && !.sapMultiplDistributions(options) && !.sapMultipleModels(options))
    tempTable$addColumnInfo(name = "subgroup", title = gettext("Subgroup"), type = "string")
}
.sapAddColumnModel        <- function(tempTable, options) {
  if (.sapMultipleModels(options))
    tempTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
}
.sapAddColumnDistribution <- function(tempTable, options) {
  if ((options[["compareModelsAcrossDistributions"]] && .sapMultipleModels(options) && .sapMultiplDistributions(options)) ||
      (!.sapMultipleModels(options) && .sapMultiplDistributions(options)))
    tempTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
}

.sapCollectFitErrors      <- function(fit, options) {

  errors <- NULL

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]))
      errors <- c(errors, gettextf(
        "%1$s model %2$s%3$s failed with the following message: %4$s.",
        distribution = attr(fit[[i]], "distribution"),
        model        = attr(fit[[i]], "model"),
        subgroup     = if (options[["subgroup"]] != "") paste0(" (", attr(fit[[i]], "subgroup"), ")") else "",
        error        = fit[[i]]
      ))
  }

  return(errors)
}

# add the model names
.sapMultipleModels          <- function(options) {
  return(length(options[["modelTerms"]]) > 1)
}
.sapMultiplDistributions    <- function(options) {
  return(options[["distribution"]] == "all" && length(.sapGetDistributions(options)) > 1)
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

  if (options[["distribution"]] %in% c("all", "bestAIC", "bestBIC")) {

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



