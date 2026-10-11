# Original predictor values define groups; every model uses the same memberships
# for its cohort before mapping observation times to its own reference design.
.sapProbabilityPlotSelectedVariables <- function(options) {
  return(.sapObservedStrataSelectedVariables(options, "probabilityPlot"))
}
.sapObservedStrataSelectedVariables <- function(options, prefix) {
  if (!options[[paste0(prefix, "StratifyBy")]])
    return(character(0))
  specification <- options[[paste0(prefix, "StratifyByVariables")]]
  return(unique(vapply(Filter(function(row) row[["stratify"]], specification), `[[`, character(1), "variable")))
}

.sapProbabilityPlotStrata <- function(dataset, variables, options) {
  return(.sapObservedStrata(dataset, variables, options, "probabilityPlot"))
}
.sapObservedStrata <- function(dataset, variables, options, prefix) {
  weights <- .sapCaseWeights(dataset, options)
  groups <- lapply(variables, function(variable) {
    value <- dataset[[variable]]
    if (is.factor(value))
      return(paste0(variable, "=", as.character(value)))
    uniqueValues <- sort(unique(value))
    if (options[[paste0(prefix, "StratifyByGroupUniqueLevels")]] && length(uniqueValues) <= as.numeric(options[[paste0(prefix, "StratifyByGroupUniqueLevelsLimit")]]))
      return(.sapPredictionCovariateLabels(uniqueValues, variable)[match(value, uniqueValues)])
    bins <- .sapPredictionGroups(value, as.numeric(options[[paste0(prefix, "StratifyByQuantiles")]]), variable, weights)
    binLabels <- function(digits) vapply(bins[["rows"]], function(rows) {
      limits <- range(value[rows])
      gettextf("%1$s: %2$s to %3$s", variable,
        format(limits[1L], digits = digits, trim = TRUE), format(limits[2L], digits = digits, trim = TRUE))
    }, character(1))
    names <- binLabels(8L)
    if (anyDuplicated(names)) names <- binLabels(17L)
    labels <- character(length(value))
    for (i in seq_along(bins[["rows"]])) labels[bins[["rows"]][[i]]] <- names[i]
    return(labels)
  })
  labels <- rep("", nrow(dataset))
  if (length(groups)) {
    keys <- .sapPredictorGroups(as.data.frame(groups, stringsAsFactors = FALSE))
    joined <- do.call(paste, c(groups, sep = ", "))
    uniqueKeys <- unique(keys)
    names <- joined[match(uniqueKeys, keys)]
    if (anyDuplicated(names))
      names <- vapply(seq_along(names), function(i) gettextf("%1$s (stratum %2$d)", names[i], i), character(1))
    labels <- names[match(keys, uniqueKeys)]
  }
  return(stats::setNames(labels, rownames(dataset)))
}

.sapProbabilityPlotPrepareFits <- function(fit, options) {
  fit <- .sapProbabilityPlotValidFits(fit)
  return(.sapObservedStrataFits(fit, options, "probabilityPlot", meanDesign = TRUE))
}
.sapObservedStrataDependencies <- function(prefix) {
  return(paste0(prefix, c("StratifyBy", "StratifyByVariables", "StratifyByQuantiles", "StratifyByGroupUniqueLevels", "StratifyByGroupUniqueLevelsLimit")))
}
.sapObservedStrataFits <- function(fit, options, prefix, meanDesign) {
  variables <- intersect(.sapObservedStrataSelectedVariables(options, prefix),
    unique(unlist(lapply(fit, function(model) .sapModelVariables(attr(model, "modelTerms"))), use.names = FALSE)))
  subgroups <- if (.sapHasSubgroups(options)) vapply(fit, function(model) attr(model, "subgroup"), character(1)) else rep("", length(fit))
  for (indices in split(seq_along(fit), subgroups)) {
    dataset <- .saSafeRbind(lapply(fit[indices], .sapPlotObservedDataset))
    # The same units may occur in multiple displayed distributions/models.
    identities <- unlist(lapply(fit[indices], function(model) rownames(.sapPlotObservedDataset(model))), use.names = FALSE)
    dataset <- dataset[!duplicated(identities), , drop = FALSE]
    rownames(dataset) <- unique(identities)
    strata <- .sapObservedStrata(dataset, variables, options, prefix)
    for (index in indices) {
      model <- fit[[index]]
      observed <- .sapPlotObservedDataset(model)
      membership <- unname(strata[rownames(observed)])
      stopifnot(!anyNA(membership))
      rows <- split(seq_len(nrow(observed)), factor(membership, levels = unique(membership)))
      if (meanDesign) {
        design <- stats::model.matrix(model)
        designRows <- match(rownames(observed), rownames(design))
        stopifnot(!anyNA(designRows), ncol(design) == model[["ncoveffs"]])
        weights <- .sapCaseWeights(observed, options)
        reference <- do.call(rbind, lapply(rows, function(selected)
          .sapWeightedColumnMeans(design[designRows[selected], , drop = FALSE], weights[selected])))
        attr(model, "predictionX") <- reference
        attr(model, "predictionLabels") <- names(rows)
      }
      attr(model, paste0(prefix, "Strata")) <- rows
      attr(model, paste0(prefix, "Membership")) <- membership
      attr(model, paste0(prefix, "Variables")) <- variables
      fit[[index]] <- model
    }
  }
  return(fit)
}

.sapProbabilityPlotLabels <- function(fit, options) {
  labels <- names(attr(fit, "probabilityPlotStrata"))
  show <- options[["probabilityPlotMergePlotsAcrossStrata"]] && length(attr(fit, "probabilityPlotVariables")) > 0L
  if (!show || (length(labels) == 1L && !nzchar(labels)))
    labels <- rep(NA_character_, length(labels))
  else
    labels <- decodeColNames(labels)
  return(.sapPlotLevelLabel(labels, fit, options, "probabilityPlot"))
}

.sapProbabilityPlotAcrossStrata <- function(fit, options) {
  fit <- .sapProbabilityPlotPrepareFits(fit, options)
  labels <- unique(unlist(lapply(fit, function(model) names(attr(model, "probabilityPlotStrata"))), use.names = FALSE))
  if (options[["probabilityPlotMergePlotsAcrossStrata"]] || length(labels) < 2L)
    return(.sapProbabilityPlotForData(fit, options))
  axisOptions <- options
  axisOptions[["probabilityPlotConfidenceIntervals"]] <- FALSE
  axisSetup <- try(.sapProbabilityPlotAxisSetup(.sapProbabilityPlotData(fit, axisOptions), options))
  if (jaspBase::isTryError(axisSetup))
    return(.sapProbabilityPlotForData(fit, options))
  container <- createJaspContainer()
  for (i in seq_along(labels)) {
    selected <- list()
    for (model in fit) {
      groups <- attr(model, "probabilityPlotStrata")
      index <- match(labels[i], names(groups))
      if (is.na(index)) next
      rows <- groups[[index]]
      attr(model, "plotDataset") <- .sapPlotObservedDataset(model)[rows, , drop = FALSE]
      attr(model, "predictionX") <- attr(model, "predictionX")[index, , drop = FALSE]
      attr(model, "predictionLabels") <- labels[i]
      attr(model, "probabilityPlotStrata") <- stats::setNames(list(seq_along(rows)), labels[i])
      attr(model, "probabilityPlotMembership") <- rep(labels[i], length(rows))
      selected[[length(selected) + 1L]] <- model
    }
    plot <- .sapProbabilityPlotForData(selected, options, axisSetup)
    plot$title <- decodeColNames(labels[i])
    plot$position <- i
    container[[paste0("stratum", i)]] <- plot
  }
  return(container)
}
