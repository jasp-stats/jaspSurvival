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
    # the models, the full dataset fit, and the numbers of components are not included as the fits are updated
    # only if the corresponding model / number of components changes (the comparison option only affects the output)
    fitContainer$dependOn(setdiff(.sapGetDependencies(options), .sapFitExcludedDependencies))
    jaspResults[["fit"]] <- fitContainer
    out                  <- NULL
  } else {
    fitContainer <- jaspResults[["fit"]]
    out          <- fitContainer$object
  }

  # check whether anything in the model had changed
  if (!is.null(out) &&
      isTRUE(all.equal(attr(out, "modelTerms"), options[["modelTerms"]])) &&
      isTRUE(all.equal(attr(out, "components"), .sapComponents(options))) &&
      attr(out, "includeFullDatasetInSubgroupAnalysis") == options[["includeFullDatasetInSubgroupAnalysis"]])
    return()

  # structure the container following:
  # - subgroup
  #   - family
  #     - components
  #       - model

  distributions <- .sapGetDistributions(options)
  components    <- .sapComponents(options)
  subgroups     <- if (.sapHasSubgroups(options)) .sapSubgroupFactor(dataset, options)

  # fit the full dataset
  if (!.sapHasSubgroups(options) || options[["includeFullDatasetInSubgroupAnalysis"]]) {

    attr(dataset, "subgroup")      <- gettext("Full dataset")
    attr(dataset, "subgroupLabel") <- gettext("Full dataset")

    out[["fullDataset"]] <- .sapFitDistributions(out[["fullDataset"]], dataset, options, distributions, components)

    attr(out[["fullDataset"]], "label")         <- gettext("Full dataset")
    attr(out[["fullDataset"]], "dataset")       <- dataset
    attr(out[["fullDataset"]], "isSubgroup")    <- FALSE
    attr(out[["fullDataset"]], "distributions") <- distributions
  }

  # fit the subgroups
  if (.sapHasSubgroups(options)) {

    subgroupLevels <- unique(subgroups)

    for(i in seq_along(subgroupLevels)) {

      subgroupDataset <- dataset[subgroups == subgroupLevels[i],,drop=FALSE]
      subgroupDataset <- droplevels(subgroupDataset)
      attr(subgroupDataset, "missingObservations") <- .saMissingObservations(dataset)

      attr(subgroupDataset, "subgroup")      <- as.character(subgroupLevels[i])
      attr(subgroupDataset, "subgroupLabel") <- gettextf("Subgroup: %1$s", subgroupLevels[i])

      out[[paste0("subgroup", subgroupLevels[i])]] <- .sapFitDistributions(out[[paste0("subgroup", subgroupLevels[i])]], subgroupDataset, options, distributions, components)

      attr(out[[paste0("subgroup", subgroupLevels[i])]], "label")         <- gettextf("Subgroup: %1$s", subgroupLevels[i])
      attr(out[[paste0("subgroup", subgroupLevels[i])]], "dataset")       <- subgroupDataset
      attr(out[[paste0("subgroup", subgroupLevels[i])]], "isSubgroup")    <- TRUE
      attr(out[[paste0("subgroup", subgroupLevels[i])]], "distributions") <- distributions
    }
  }

  attr(out, "modelTerms")                           <- options[["modelTerms"]]
  attr(out, "components")                           <- components
  attr(out, "includeFullDatasetInSubgroupAnalysis") <- options[["includeFullDatasetInSubgroupAnalysis"]]

  fitContainer$object <- out

  return()
}
.sapFitDistributions    <- function(out, dataset, options, distributions, components) {

  for (i in seq_along(distributions)) {

    # previously fitted numbers of components are kept (and only the missing ones are fitted)
    for (k in components) {
      # the fits with one component fewer of the same distribution supply the split starts of the mixture models
      out[[distributions[i]]][[as.character(k)]] <- .sapFitDistribution(
        out[[distributions[i]]][[as.character(k)]], dataset, options, distributions[i], k,
        previous = out[[distributions[i]]][[as.character(k - 1)]]
      )
    }

    attr(out[[distributions[i]]], "distribution") <- distributions[i]
    attr(out[[distributions[i]]], "label")        <- .sapOption2DistributionName(distributions[i])
  }

  return(out)
}
.sapFitDistribution     <- function(out, dataset, options, distribution, components, previous = NULL) {

  for (i in seq_along(options[["modelTerms"]])) {

    # check whether the model has already been fitted
    if (length(out) >= i) {

      currentTerms <- options[["modelTerms"]][[i]]

      if (.sapSameModelTerms(attr(out[[i]], "modelTerms"), currentTerms)) {
        # everything but the title is the same -> relabel
        attr(out[[i]], "label")      <- currentTerms[["title"]]
        attr(out[[i]], "modelTitle") <- currentTerms[["title"]]
        attr(out[[i]], "modelTerms") <- options[["modelTerms"]][[i]]
        next
      }
    }

    # fit the model (the same model of the previous number of components, when it is available)
    out[[i]] <- .sapFitModel(dataset, options, distribution, options[["modelTerms"]][[i]], components,
                             previous = .sapPreviousComponentsFit(previous, options[["modelTerms"]][[i]], i))

  }

  # remove models that are not selected
  if (length(out) > length(options[["modelTerms"]])) {
    out <- out[seq_along(options[["modelTerms"]])]
  }

  # store attributes
  attr(out, "distribution") <- distribution
  attr(out, "components")   <- components
  attr(out, "label")        <- .sapComponentsLabel(components)

  return(out)
}
.sapSameModelTerms <- function(previous, current) {

  # Titles may change without refitting; model IDs and predictor terms must match.
  previous[["title"]] <- ""
  current[["title"]]  <- ""

  return(isTRUE(all.equal(previous, current)))
}
.sapPreviousComponentsFit <- function(previous, modelTerms, index) {

  # the fit with one component fewer supplies the split starts only when it is the same model:
  # the store keeps the fits of previously specified model terms, and those are not a starting point
  # of the current model (the chain is then fitted by the mixture estimator itself)
  if (length(previous) < index)
    return(NULL)

  candidate <- previous[[index]]
  if (jaspBase::isTryError(candidate))
    return(NULL)

  previousTerms <- attr(candidate, "modelTerms")
  if (is.null(previousTerms))
    return(NULL)

  if (!.sapSameModelTerms(previousTerms, modelTerms))
    return(NULL)

  return(candidate)
}
.sapFitModel            <- function(dataset, options, distribution, modelTerms, components, previous = NULL, silent = FALSE) {

  if (components > 1) {
    fit <- try(.sapmFitMixture(dataset, options, distribution, modelTerms, components, previous), silent = silent)
  } else if (options[["analysisType"]] == "mixture" && options[["mixtureConstrainMinimumSpread"]]) {
    fit <- try(.sapmFitSingle(dataset, options, distribution, modelTerms), silent = silent)
  } else {
    fit <- try(flexsurv::flexsurvreg(
      formula = .sapGetFormula(options, modelTerms),
      data    = dataset,
      dist    = distribution,
      weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]],
      cl      = options[["coefficientsConfidenceIntervalLevel"]]
    ), silent = silent)
  }

  # flexsurvreg stores a scalar NA covariance if the Hessian is not finite, the output expects a matrix
  if (!jaspBase::isTryError(fit) && !is.matrix(fit[["cov"]])) {
    parameters   <- rownames(fit[["res.t"]])
    fit[["cov"]] <- matrix(NA_real_, length(parameters), length(parameters), dimnames = list(parameters, parameters))
  }

  # store attributes
  attr(fit, "subgroup")       <- attr(dataset, "subgroup")
  attr(fit, "subgroupLabel")  <- attr(dataset, "subgroupLabel")
  attr(fit, "modelTitle")     <- modelTerms[["title"]]
  attr(fit, "modelId")        <- modelTerms[["name"]]
  attr(fit, "modelTerms")     <- modelTerms
  attr(fit, "distribution")   <- .sapOption2DistributionName(distribution)
  attr(fit, "family")         <- distribution
  attr(fit, "components")     <- components
  attr(fit, "dataset")        <- dataset

  return(fit)
}
.sapFitIndex            <- function(out, options) {

  # flat index of the nested fit store (subgroup - family - components - model)
  # the fits are selected and grouped via the index and extracted afterwards
  components <- as.character(.sapComponents(options))
  index      <- list()

  for (subgroup in names(out)) {
    for (family in names(out[[subgroup]])) {
      for (k in components) {
        for (model in seq_along(out[[subgroup]][[family]][[k]])) {

          fit     <- out[[subgroup]][[family]][[k]][[model]]
          isError <- jaspBase::isTryError(fit)

          index[[length(index) + 1]] <- data.frame(
            subgroup      = subgroup,
            subgroupLabel = attr(out[[subgroup]], "label"),
            family        = family,
            distribution  = attr(fit, "distribution"),
            components    = as.integer(k),
            model         = model,
            modelId       = attr(fit, "modelId"),
            aic           = if (isError) NA_real_ else AIC(fit),
            bic           = if (isError) NA_real_ else BIC(fit)
          )
        }
      }
    }
  }

  index <- do.call(rbind, index)
  index[["row"]] <- seq_len(nrow(index))

  # remove the full dataset fits if not requested anymore
  if (.sapHasSubgroups(options) && !options[["includeFullDatasetInSubgroupAnalysis"]])
    index <- index[index[["subgroup"]] != "fullDataset", , drop = FALSE]

  return(index)
}
.sapIndexFits           <- function(out, index) {

  # extract the fits corresponding to the (grouped) index
  fit <- lapply(seq_len(nrow(index)), function(i) {
    out[[index[["subgroup"]][i]]][[index[["family"]][i]]][[as.character(index[["components"]][i])]][[index[["model"]][i]]]
  })

  if (nrow(index) > 0 && !anyNA(index[["fitName"]]))
    names(fit) <- index[["fitName"]]

  if (nrow(index) > 0 && !is.na(index[["groupLabel"]][1]))
    attr(fit, "label") <- index[["groupLabel"]][1]

  return(fit)
}
.sapExtractFit          <- function(jaspResults, options, type = "all", output = NULL) {

  if (!.saSurvivalReady(options))
    return()

  out   <- jaspResults[["fit"]][["object"]]
  index <- .sapFitIndex(out, options)

  # select the models within each subgroup
  if (type %in% c("selected", "byDistribution", "byModel"))
    index <- .sapSelectIndex(index, options, type)

  # group the models into output sections by subgroups (and models)
  if (type == "byModel") {
    groups <- .sapGroupIndexModels(index, options, output)
  } else {
    groups <- .sapGroupIndex(index, options)
  }

  return(lapply(groups, .sapIndexFits, out = out))
}
.sapSelectIndex         <- function(index, options, type) {

  # the models are selected within each subgroup by traversing the axes hierarchically (distribution - components - model):
  # - an axis with all levels splits the remaining selection by its levels
  # - an axis with the best level keeps the level of the best fitting model across all models of the current selection
  #   (e.g., the best distribution is the distribution of the best fitting model across all numbers of components and models)
  # - an axis with a specified level keeps only that level
  # the output grouping (i.e., comparing models across distributions / components) does not affect the selection
  axes <- list(
    list(column = "family",     multiple = .sapMultipleFamilies(options),                           selection = .sapFamilySelection(options)),
    list(column = "components", multiple = .sapMultipleComponents(options),                         selection = .sapComponentSelection(options)),
    list(column = "modelId",    multiple = .sapMultipleModels(options) && type != "byDistribution", selection = options[["interpretModel"]])
  )

  groups <- split(index, factor(index[["subgroup"]], levels = unique(index[["subgroup"]])))

  for (axis in axes) {

    if (!axis[["multiple"]])
      next

    if (axis[["selection"]] == "all") {
      groups <- do.call(c, lapply(unname(groups), function(group) {
        unname(split(group, factor(group[[axis[["column"]]]], levels = unique(group[[axis[["column"]]]]))))
      }))
    } else if (axis[["selection"]] %in% c("bestAic", "bestBic")) {
      groups <- lapply(groups, function(group) {
        best <- which.min(group[[.sapSelectionCriterion(axis[["selection"]])]])
        if (length(best) == 0)
          return(group)
        return(group[group[[axis[["column"]]]] == group[[axis[["column"]]]][best], , drop = FALSE])
      })
    } else {
      groups <- lapply(groups, function(group) group[group[[axis[["column"]]]] == axis[["selection"]], , drop = FALSE])
    }
  }

  selectedRows <- unlist(lapply(groups, function(group) group[["row"]]), use.names = FALSE)

  return(index[index[["row"]] %in% selectedRows, , drop = FALSE])
}
.sapGroupIndex          <- function(index, options) {

  multipleModels     <- .sapMultipleModels(options)
  multipleFamilies   <- .sapMultipleFamilies(options)
  multipleComponents <- .sapMultipleComponents(options)

  # subgroups are collapsed only if the user specifies a single distribution, number of components, and model
  if (!multipleModels && !multipleFamilies && !multipleComponents) {
    index[["groupLabel"]] <- NA_character_
    index[["fitName"]]    <- index[["subgroup"]]
    return(list(index))
  }

  # distributions / numbers of components are collapsed if the user specifies one model (or asks for comparing models across them)
  splitFamilies   <- multipleModels && multipleFamilies   && !options[["compareModelsAcrossDistributions"]]
  splitComponents <- multipleModels && multipleComponents && !options[["compareModelsAcrossComponents"]]
  spanFamilies    <- multipleFamilies   && !splitFamilies
  spanComponents  <- multipleComponents && !splitComponents

  labelParts <- list()
  if (.sapHasSubgroups(options) || (!splitFamilies && !splitComponents))
    labelParts[["subgroup"]]   <- index[["subgroupLabel"]]
  if (splitFamilies)
    labelParts[["family"]]     <- index[["distribution"]]
  if (splitComponents)
    labelParts[["components"]] <- .sapComponentsLabel(index[["components"]])

  index[["groupLabel"]] <- do.call(paste, c(unname(labelParts), sep = " | "))
  index[["fitName"]]    <- if (spanFamilies || spanComponents) paste0(
    if (spanFamilies)   index[["family"]],
    if (spanComponents) paste0("components", index[["components"]]),
    if (multipleModels) index[["model"]]
  ) else NA_character_

  groupKey <- paste(index[["subgroup"]], if (splitFamilies) index[["family"]], if (splitComponents) index[["components"]], sep = "|")

  return(unname(split(index, factor(groupKey, levels = unique(groupKey)))))
}
.sapGroupIndexModels    <- function(index, options, output) {

  # automatically extract models by model across distributions (and numbers of components)
  multipleModels     <- .sapMultipleModels(options)
  splitSubgroups     <- !.sapMergePlotsAcrossSubgroups(options, output)
  mergeSelectedModels <- !splitSubgroups && .sapBestModelSelection(options)
  splitFamilies      <- .sapMultipleFamilies(options) && .sapFamilySelection(options) == "all" && !.sapMergePlotsAcrossFamilies(options, output)
  # a selected (best) number of components leaves a single number of components per distribution
  splitComponents    <- .sapMultipleComponents(options) && .sapComponentSelection(options) == "all" && !.sapMergePlotsAcrossComponents(options, output)
  spanComponents     <- .sapMultipleComponents(options) && !splitComponents

  labelParts <- list()
  if (.sapHasSubgroups(options) && splitSubgroups)
    labelParts[["subgroup"]]   <- index[["subgroupLabel"]]
  if (splitFamilies)
    labelParts[["family"]]     <- index[["distribution"]]
  if (splitComponents)
    labelParts[["components"]] <- .sapComponentsLabel(index[["components"]])
  if (multipleModels && !mergeSelectedModels)
    labelParts[["model"]]      <- vapply(options[["modelTerms"]][index[["model"]]], function(x) x[["title"]], character(1))

  index[["groupLabel"]] <- if (length(labelParts) > 0) do.call(paste, c(unname(labelParts), sep = " | ")) else ""
  index[["fitName"]]    <- paste0(if (!splitSubgroups) paste0(index[["subgroup"]], "|"), index[["family"]], if (spanComponents) paste0("components", index[["components"]]))

  groupKey <- paste(if (splitSubgroups) index[["subgroup"]], if (mergeSelectedModels) rep("", nrow(index)) else index[["model"]], if (splitFamilies) index[["family"]], if (splitComponents) index[["components"]], sep = "|")

  return(unname(split(index, factor(groupKey, levels = unique(groupKey)))))
}
.sapFlattenFit          <- function(fit, options) {

  out <- list()

  # check the output type
  multipleModels        <- .sapMultipleModels(options)
  multipleDistributions <- .sapMultipleFamilies(options)
  multipleComponents    <- .sapMultipleComponents(options)

  for(i in seq_along(fit)) {
    for(j in seq_along(fit[[i]])) {
      out[[length(out) + 1]] <- fit[[i]][[j]]

      labelParts <- c(
        if (.sapHasSubgroups(options)) attr(fit[[i]][[j]], "subgroupLabel"),
        if (multipleDistributions) paste0(attr(fit[[i]][[j]], "distribution"), " distribution"),
        if (multipleComponents)    .sapComponentsLabel(attr(fit[[i]][[j]], "components")),
        if (multipleModels)        attr(fit[[i]][[j]], "modelTitle")
      )
      attr(out[[length(out)]], "label") <- paste(labelParts, collapse = " | ")

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
.sapSectionWrapper <- function(jaspResults, options, fit, outputFunction, name, title, dependencies, position) {

  if (length(fit) > 1) {

    tempContainer <- createJaspContainer(title = title)
    tempContainer$dependOn(dependencies)
    tempContainer$position <- position
    jaspResults[[name]]    <- tempContainer

    for (i in seq_along(fit)) {

      # create a table for each model set
      tempContainer[[paste0("table", i)]] <- outputFunction(fit = fit[[i]], options = options)
      tempContainer[[paste0("table", i)]]$position <- i
      tempContainer[[paste0("table", i)]]$title    <- attr(fit[[i]], "label")
      .sapSectionFitError(tempContainer[[paste0("table", i)]], fit[[i]], options)

    }

  } else {

    # only one table needed
    tempTable           <- outputFunction(fit = fit[[1]], options = options)
    tempTable$title     <- title
    tempTable$dependOn(dependencies)
    tempTable$position  <- position
    jaspResults[[name]] <- tempTable
    .sapSectionFitError(tempTable, fit[[1]], options)

  }

  return()
}
.sapSectionFitError <- function(output, fit, options) {

  # Fit failures must remain visible even when the model summary is disabled.
  if (inherits(fit, "try-error"))
    fit <- list(fit)
  else if (inherits(fit, "flexsurvreg") || length(fit) == 0)
    return()

  if (all(vapply(fit, inherits, logical(1), what = "try-error")))
    output$setError(paste(.sapCollectFitErrors(fit, options), collapse = "\n"))

  return()
}

# analysis axes: subgroup - family (distribution) - number of components - model
.sapMultipleModels          <- function(options) {
  return(length(options[["modelTerms"]]) > 1)
}
.sapBestModelSelection      <- function(options) {
  return(.sapMultipleModels(options) && options[["interpretModel"]] %in% c("bestAic", "bestBic"))
}
.sapMultipleFamilies        <- function(options) {
  return(.sapFamilySelection(options) %in% c("all", "bestAic", "bestBic") && length(.sapGetDistributions(options)) > 1)
}
.sapMultipleComponents      <- function(options) {
  return(options[["analysisType"]] == "mixture" && .sapComponentSelection(options) %in% c("all", "bestAic", "bestBic") && options[["mixtureMaximumComponents"]] > 1)
}
.sapFamilySelection         <- function(options) {
  return(options[["distribution"]])
}
.sapComponentSelection      <- function(options) {
  if (options[["analysisType"]] != "mixture")
    return("1")
  return(options[["mixtureComponents"]])
}
.sapComponents              <- function(options) {

  if (options[["analysisType"]] != "mixture")
    return(1L)

  if (.sapComponentSelection(options) %in% c("all", "bestAic", "bestBic"))
    return(seq_len(options[["mixtureMaximumComponents"]]))

  return(as.integer(.sapComponentSelection(options)))
}
.sapComponentsLabel         <- function(components) {
  return(vapply(components, function(k) sprintf(ngettext(k, "%1$i component", "%1$i components"), k), character(1)))
}
.sapSeriesLabel             <- function(fit, options) {

  # label of the fitted model in plots with multiple distributions (and numbers of components)
  if (options[["analysisType"]] != "mixture")
    return(attr(fit, "distribution"))

  return(gettextf("%1$s, %2$i comp.", attr(fit, "distribution"), attr(fit, "components")))
}
.sapSelectionCriterion      <- function(selection) {
  return(switch(
    selection,
    "bestAic" = "aic",
    "bestBic" = "bic"
  ))
}
.sapMergePlotsAcrossFamilies   <- function(options, output) {
  # merging across distributions is possible only if all distributions are displayed without model selection
  return(options[[paste0(output, "MergePlotsAcrossDistributions")]] && .sapFamilySelection(options) == "all" && !.sapBestModelSelection(options))
}
.sapMergePlotsAcrossComponents <- function(options, output) {
  # merging across numbers of components is possible only if all numbers of components are displayed without model selection
  return(options[["analysisType"]] == "mixture" && options[[paste0(output, "MergePlotsAcrossComponents")]] && .sapComponentSelection(options) == "all" && !.sapBestModelSelection(options))
}
.sapMergePlots                 <- function(options, output) {
  return(.sapMergePlotsAcrossFamilies(options, output) || .sapMergePlotsAcrossComponents(options, output) || .sapMergePlotsAcrossSubgroups(options, output))
}
.sapMergePlotsAcrossSubgroups <- function(options, output) {
  return(.sapHasSubgroups(options) && options[[paste0(output, "MergePlotsAcrossSubgroups")]])
}
.sapOption2Distribution <- function(optionName) {
  return(as.list(.sapDistributions)[[optionName]])
}
.sapOption2DistributionName <- function(optionName) {

  if (optionName %in% names(.sapDistributions))
    optionName <- .sapOption2Distribution(optionName)

  return(switch(optionName,
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
.sapGetDistributions <- function(options) {

  if (!options[["distribution"]] %in% c("all", "bestAic", "bestBic"))
    return(.sapOption2Distribution(options[["distribution"]]))

  selected <- vapply(.sapDistributionOptions, function(name) options[[name]], logical(1))
  distributions <- .sapDistributions[selected]
  if (length(distributions) == 0)
    .quitAnalysis(gettext("No parametric distribution selected. Please select at least one parametric distribution."))

  return(distributions)
}
