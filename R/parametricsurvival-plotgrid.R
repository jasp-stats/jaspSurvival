.sapPlotAcrossFactors <- function(fit, options, mergeOption, plotFunction) {

  if (is.null(fit))
    fit <- list()
  else if (inherits(fit, "try-error") || inherits(fit, "flexsurvreg"))
    fit <- list(fit)
  valid <- vapply(fit, function(x) !is.null(x) && !jaspBase::isTryError(x) && length(x) > 0, logical(1))
  if (!.saSurvivalReady(options) || !any(valid))
    return(plotFunction(fit, options))

  if (options[[mergeOption]]) {
    for (j in which(valid))
      fit[[j]] <- .sapPlotFactorPredictions(fit[[j]], options)
    return(plotFunction(fit, options))
  }

  if (.sapMergePlotsAcrossSubgroups(options, sub("MergePlotsAcrossFactors$", "", mergeOption)))
    return(.sapPlotAcrossSubgroupFactors(fit, options, plotFunction))

  firstFit <- fit[[which(valid)[1]]]
  factors  <- intersect(names(stats::model.frame(firstFit)), unlist(options[["factors"]], use.names = FALSE))
  if (firstFit[["ncovs"]] == 0 || length(factors) == 0)
    return(plotFunction(fit, options))

  dataset <- attr(firstFit, "dataset", exact = TRUE)
  groups  <- split(seq_len(nrow(dataset)), .sapPredictorGroups(dataset[factors]))
  if (length(groups) < 2)
    return(plotFunction(fit, options))

  container <- createJaspContainer()
  for (i in seq_along(groups)) {
    rows   <- groups[[i]]
    labels <- vapply(dataset[rows[1], factors, drop = FALSE], as.character, character(1))
    title  <- paste(paste0(decodeColNames(factors), "=", labels), collapse = ", ")
    plot <- try({
      groupRows <- rownames(dataset)[rows]
      plotFit   <- fit
      for (j in which(valid)) {
        model        <- fit[[j]]
        modelDataset <- attr(model, "dataset", exact = TRUE)
        design       <- stats::model.matrix(model)
        datasetRows  <- match(groupRows, rownames(modelDataset))
        designRows   <- match(groupRows, rownames(design))
        stopifnot(length(groupRows) > 0, !anyDuplicated(rownames(modelDataset)), !anyDuplicated(rownames(design)),
                  !anyNA(datasetRows), !anyNA(designRows), ncol(design) == model[["ncoveffs"]])
        # Use the original expanded columns, retaining native positional order.
        attr(model, "predictionX") <- matrix(unname(colMeans(design[designRows, , drop = FALSE])), nrow = 1L)
        attr(model, "plotDataset") <- modelDataset[datasetRows, , drop = FALSE]
        plotFit[[j]] <- model
      }
      plotFunction(plotFit, options)
    })
    if (inherits(attr(plot, "condition", exact = TRUE), "validationError"))
      stop(attr(plot, "condition", exact = TRUE))
    failed <- jaspBase::isTryError(plot)
    if (failed)
      plot <- createJaspPlot()
    plot$title    <- title
    plot$position <- i
    container[[paste0("plot", i)]] <- plot
    if (failed)
      plot$setError(if (mergeOption == "probabilityPlotMergePlotsAcrossFactors")
        gettext("The model failed to produce a probability plot. Consider simplifying the model.") else
        gettext("The model failed to produce predictions. Consider simplifying the model."))
  }
  return(container)
}
.sapPlotAcrossSubgroupFactors <- function(fit, options, plotFunction) {

  valid <- vapply(fit, function(x) !is.null(x) && !jaspBase::isTryError(x) && length(x) > 0, logical(1))
  factors <- unique(unlist(lapply(fit[valid], function(model)
    intersect(names(stats::model.frame(model)), unlist(options[["factors"]], use.names = FALSE))), use.names = FALSE))
  if (length(factors) == 0)
    return(plotFunction(fit, options))

  dataset <- .saSafeRbind(lapply(fit[valid], .sapPlotObservedDataset))
  groups <- split(seq_len(nrow(dataset)), .sapPredictorGroups(dataset[factors]))
  container <- createJaspContainer()
  for (i in seq_along(groups)) {
    values <- dataset[groups[[i]][1], factors, drop = FALSE]
    plotFit <- list()
    for (j in seq_along(fit)) {
      model <- fit[[j]]
      if (valid[j]) {
        modelDataset <- .sapPlotObservedDataset(model)
        rows <- Reduce(`&`, lapply(factors, function(factor) as.character(modelDataset[[factor]]) == as.character(values[[factor]][1])))
        if (!any(rows))
          next
        # Subgroups have disjoint row identities; match their own design rows.
        design <- stats::model.matrix(model)
        designRows <- match(rownames(modelDataset)[rows], rownames(design))
        stopifnot(!anyNA(designRows), ncol(design) == model[["ncoveffs"]])
        attr(model, "predictionX") <- matrix(unname(colMeans(design[designRows, , drop = FALSE])), nrow = 1L)
        attr(model, "predictionLabels") <- NULL
        attr(model, "plotDataset") <- modelDataset[rows, , drop = FALSE]
      }
      plotFit[[length(plotFit) + 1]] <- model
    }
    plot <- plotFunction(plotFit, options)
    plot$title <- paste(paste0(decodeColNames(factors), "=", vapply(values, as.character, character(1))), collapse = ", ")
    plot$position <- i
    container[[paste0("plot", i)]] <- plot
  }
  return(container)
}
.sapPlotSubgroupFits <- function(fit) {
  keys <- vapply(fit, function(model) attr(model, "subgroup"), character(1))
  return(fit[!duplicated(keys)])
}
.sapBindPlotData <- function(data) {
  out <- .saSafeRbind(data)
  return(if (is.null(out)) data[[1]] else out)
}
.sapPlotLevelLabel <- function(level, fit, options, output) {
  if (!.sapMergePlotsAcrossSubgroups(options, output))
    return(level)
  subgroup <- attr(fit, "subgroupLabel")
  return(ifelse(is.na(level) | !nzchar(level), subgroup, paste(subgroup, level, sep = " | ")))
}
.sapPlotPredictionLevels <- function(predictions, fit, options, output, emptyLabel = NA_character_) {
  labels <- if (length(predictions) > 1) decodeColNames(names(predictions)) else emptyLabel
  if (.sapMergePlotsAcrossSubgroups(options, output) && options[[paste0(output, "MergePlotsAcrossFactors")]]) {
    factors <- intersect(names(stats::model.frame(fit)), unlist(options[["factors"]], use.names = FALSE))
    if (length(factors) > 0)
      labels <- decodeColNames(names(predictions))
  }
  return(.sapPlotLevelLabel(labels, fit, options, output))
}
.sapPlotTimeFit <- function(fit) {
  model <- fit[[1]]
  attr(model, "dataset") <- .saSafeRbind(lapply(fit, function(x) attr(x, "dataset")))
  return(model)
}
.sapPlotFactorPredictions <- function(fit, options) {

  modelFrame <- stats::model.frame(fit)
  predictors <- unique(attr(modelFrame, "covnames.orig"))
  factors    <- intersect(predictors, unlist(options[["factors"]], use.names = FALSE))
  covariates <- intersect(predictors, unlist(options[["covariates"]], use.names = FALSE))
  if (length(factors) == 0 || length(covariates) == 0)
    return(fit)

  # Match separate panels: average the expanded design within each factor cell.
  # This retains interactions and flexsurv's native positional column order.
  design <- stats::model.matrix(fit)
  groups <- split(seq_len(nrow(modelFrame)), .sapPredictorGroups(modelFrame[factors]))
  stopifnot(identical(rownames(modelFrame), rownames(design)), ncol(design) == fit[["ncoveffs"]])
  attr(fit, "predictionX") <- do.call(rbind, lapply(groups, function(rows)
    unname(colMeans(design[rows, , drop = FALSE]))))
  attr(fit, "predictionLabels") <- vapply(groups, function(rows) {
    values <- vapply(modelFrame[rows[1], factors, drop = FALSE], as.character, character(1))
    paste0(factors, "=", values, collapse = ",")
  }, character(1))

  return(fit)
}
.sapPlotObservedDataset <- function(fit) {
  dataset <- attr(fit, "plotDataset", exact = TRUE)
  return(if (is.null(dataset)) attr(fit, "dataset", exact = TRUE) else dataset)
}
.sapPredictorGroups <- function(predictors) {
  if (ncol(predictors) == 0)
    return(rep(1L, nrow(predictors)))
  return(interaction(lapply(predictors, function(x) match(x, unique(x))), drop = TRUE, lex.order = TRUE))
}

# Scout point estimates once, then prune in displayed coordinates; confidence
# intervals are still evaluated natively at the retained times. Numeric failures
# and excess mandatory nodes keep the original grid, which may exceed the cap.
.sapAdaptivePlotTimes <- function(times, evaluate, xTransform = identity, xInverse = identity,
                                  yTransform = identity, limits = NULL, minimum = 17L,
                                  maximum = 201L, anchors = numeric(0)) {
  xRange <- try(xTransform(range(times)), silent = TRUE)
  if (inherits(xRange, "try-error") || any(!is.finite(xRange)) || diff(xRange) <= 0)
    return(times)
  uniform <- try(xInverse(seq(xRange[1], xRange[2], length.out = 129L)), silent = TRUE)
  if (inherits(uniform, "try-error") || any(!is.finite(uniform)))
    return(times)
  uniform[c(1L, 129L)] <- range(times)
  anchors <- anchors[anchors >= min(times) & anchors <= max(times)]
  scouts <- sort(unique(c(times, anchors, uniform)))
  x <- try(xTransform(scouts), silent = TRUE)
  if (inherits(x, "try-error") || any(!is.finite(x)))
    return(times)
  scouts <- scouts[!duplicated(x)]
  x <- x[!duplicated(x)]
  raw <- try(evaluate(scouts), silent = TRUE)
  if (inherits(raw, "try-error"))
    return(times)
  stopifnot(is.matrix(raw), is.numeric(raw), nrow(raw) == length(scouts), ncol(raw) > 0L)
  if (any(!is.finite(raw)))
    return(times)
  selected <- try({
    y <- yTransform(raw)
    if (is.function(limits)) limits <- limits(raw)
    if (anyNA(y) || (any(is.infinite(y)) && is.null(limits)))
      stop("Invalid plot transformation")
    if (!is.null(limits)) {
      limits <- range(limits) + c(-0.05, 0.05) * diff(range(limits))
      y[is.infinite(y) & y < 0] <- limits[1]
      y[is.infinite(y) & y > 0] <- limits[2]
    }
    clip <- function(z) if (is.null(limits)) z else pmin(pmax(z, limits[1]), limits[2])
    actual <- clip(y)
    span <- diff(range(actual))
    retained <- sort(unique(match(xTransform(uniform[seq(1L, 129L, length.out = minimum)]), x)))
    # Keep visible entry/exit neighbours before clipping can hide their bends.
    crossings <- if (is.null(limits)) integer(0) else unique(unlist(lapply(limits, function(edge)
      which(rowSums((y[-nrow(y), , drop = FALSE] < edge) != (y[-1L, , drop = FALSE] < edge)) > 0L))))
    retained <- sort(unique(c(retained, crossings, crossings + 1L)))
    if (length(retained) > maximum) return(times)
    while (span > 0 && length(retained) < maximum) {
      chord <- vapply(seq_len(ncol(y)), function(j)
        stats::approx(x[retained], y[retained, j], xout = x)$y, numeric(length(x)))
      error <- apply(abs(clip(chord) - actual), 1L, max)
      error[retained] <- 0
      worst <- which.max(error)
      if (error[worst] <= 0.001 * span) break
      retained <- sort(c(retained, worst))
    }
    scouts[retained]
  }, silent = TRUE)
  return(if (inherits(selected, "try-error")) times else selected)
}
.sapPlotPredictionMatrix <- function(predictions) {
  return(do.call(cbind, lapply(predictions, function(x) x[["est"]])))
}
.sapPlotFeatureTimes <- function(fit, times, sparse = FALSE) {
  tail          <- 10^seq(-6, -1, length.out = 41L)
  probabilities <- sort(unique(c(seq(0.001, 0.999, length.out = 101L), tail, 1 - tail)))
  quantiles     <- sort(unique(c(seq(0.001, 0.999, length.out = 21L), tail, 1 - tail)))
  # Integrated estimates are costly; a sparse feature grid still seeds their bends.
  if (sparse) {
    probabilities <- c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
    quantiles     <- probabilities
  }
  anchors <- lapply(fit, function(model) {
    mixture <- attr(model, "mixture")
    if (!is.null(mixture))
      return(.sapmComponentPlotTimes(model, .sapmFamily(mixture[["family"]]), mixture[["components"]], times, probabilities))
    return(unlist(lapply(.sapSummaryPredictions(model, type = "quantile", quantiles = quantiles, ci = FALSE), function(x) x[["est"]]), use.names = FALSE))
  })
  anchors <- unlist(anchors, use.names = FALSE)
  return(anchors[is.finite(anchors) & anchors >= min(times) & anchors <= max(times)])
}
