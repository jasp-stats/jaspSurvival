# Case weights are frequencies: data-derived references must match repeated rows.
.sapUnrollFrequencyDataset <- function(dataset, options) {
  if (options[["weights"]] == "")
    return(dataset)

  # Expand before constructing bases and diagnostics so frequencies represent
  # repeated units throughout; unit weights prevent applying them twice.
  rows         <- rep(seq_len(nrow(dataset)), round(dataset[[options[["weights"]]]]))
  originalRows <- rownames(dataset)[rows]
  dataset      <- dataset[rows, , drop = FALSE]
  attr(dataset, "frequencyRowNames") <- stats::setNames(originalRows, rownames(dataset))

  return(dataset)
}

.sapCaseWeights <- function(dataset, options) {
  if (!is.null(attr(dataset, "frequencyRowNames", exact = TRUE)))
    return(rep(1, nrow(dataset)))
  if (options[["weights"]] != "")
    return(dataset[[options[["weights"]]]])
  return(rep(1, nrow(dataset)))
}

.sapWeightedMean <- function(value, weights) {
  if (all(weights == 1))
    return(mean(value))
  return(stats::weighted.mean(value, weights))
}

.sapWeightedSd <- function(value, weights) {
  if (all(weights == 1))
    return(stats::sd(value))
  center <- .sapWeightedMean(value, weights)
  return(sqrt(sum(weights * (value - center)^2) / (sum(weights) - 1)))
}

.sapWeightedColumnMeans <- function(design, weights) {
  stopifnot(nrow(design) == length(weights))
  if (all(weights == 1))
    return(unname(colMeans(design)))
  return(unname(colSums(design * (weights / sum(weights)))))
}

.sapFrequencyQuantile <- function(value, weights, probabilities) {
  if (all(weights == 1))
    return(stats::quantile(value, probabilities, names = FALSE, type = 7L))
  order <- order(value)
  value <- value[order]
  cumulative <- cumsum(weights[order])
  size <- sum(weights)
  position <- 1 + (size - 1) * probabilities
  lower <- floor(position)
  fraction <- position - lower
  # Locate order statistics in the expanded sample without allocating it.
  lowerIndex <- findInterval(lower - 1, cumulative) + 1L
  upperIndex <- findInterval(pmin(lower + 1, size) - 1, cumulative) + 1L
  return((1 - fraction) * value[lowerIndex] + fraction * value[upperIndex])
}

# Compress only fitting rows: bases and starts still use the repeated sample.
.sapmCompressedFit <- function(fitCall, fitFunction = flexsurv::flexsurvreg) {

  dataset <- fitCall[["data"]]
  formulas <- c(list(fitCall[["formula"]]), fitCall[["anc"]])
  variables <- unique(unlist(lapply(formulas, all.vars), use.names = FALSE))
  keys <- lapply(dataset[variables], function(value) match(value, unique(value)))
  keys <- do.call(paste, c(keys, sep = "|"))
  rows <- which(!duplicated(keys))
  # Avoid reconstructing full model frames for negligible reductions in work.
  if (length(rows) > 0.9 * nrow(dataset))
    return(do.call(fitFunction, fitCall))

  index <- match(keys, keys[rows])
  weights <- fitCall[["weights"]]
  if (is.null(weights))
    weights <- rep(1, nrow(dataset))
  groupWeights <- as.vector(rowsum(weights, index, reorder = FALSE))

  # Preserve flexsurv's replica-derived initializer as well as the basis.
  if (identical(fitFunction, flexsurv::flexsurvreg) && is.null(fitCall[["inits"]])) {
    pilot <- fitCall
    pilot[["method"]] <- "BFGS"
    pilot[["lower"]] <- pilot[["upper"]] <- NULL
    pilot[["control"]] <- list(maxit = 0)
    pilot[["hessian"]] <- FALSE
    initial <- do.call(flexsurv::flexsurvreg, pilot)
    fitCall[["inits"]] <- unname(initial[["res"]][, "est"])
  }

  # model.frame evaluates transformations BEFORE subset, retaining full-sample
  # polynomial coefficients, spline knots, contrasts and prediction metadata.
  fitCall[["subset"]] <- rows
  fittingWeights <- weights
  fittingWeights[rows] <- groupWeights
  fitCall[["weights"]] <- fittingWeights
  fit <- do.call(fitFunction, fitCall)
  if (!inherits(fit, "flexsurvreg"))
    return(fit)

  # Downstream diagnostics and exports expect one entry per fitting unit.
  expandMatrix <- function(value) {
    if (is.null(value)) return(NULL)
    out <- value[index, , drop = FALSE]
    for (attribute in setdiff(names(attributes(value)), c("dim", "dimnames")))
      attr(out, attribute) <- attr(value, attribute)
    rownames(out) <- rownames(dataset)
    return(out)
  }
  modelFrame <- fit[["data"]][["m"]]
  fit[["data"]][["m"]] <- modelFrame[index, , drop = FALSE]
  matrixColumns <- names(modelFrame)[vapply(modelFrame, is.matrix, logical(1))]
  if (length(matrixColumns) > 0L) {
    fullFrame <- do.call(stats::model.frame, list(formula = fit[["covdata"]][["terms"]],
      data = dataset, weights = weights, na.action = stats::na.pass))
    fit[["data"]][["m"]][matrixColumns] <- fullFrame[matrixColumns]
  }
  rownames(fit[["data"]][["m"]]) <- rownames(dataset)
  fit[["data"]][["m"]][["(weights)"]] <- weights
  fit[["data"]][["Y"]] <- expandMatrix(fit[["data"]][["Y"]])
  fit[["data"]][["mml"]] <- lapply(fit[["data"]][["mml"]], expandMatrix)
  fit[["logliki"]] <- fit[["logliki"]][index]
  means <- unlist(lapply(fit[["data"]][["mml"]], function(value) {
    if (is.null(value)) return(numeric(0))
    return(colMeans(value[, -1L, drop = FALSE]))
  }), use.names = FALSE)
  fit[["datameans"]] <- stats::setNames(means, names(fit[["datameans"]]))
  fit[["N"]] <- nrow(dataset)
  fit[["events"]] <- sum(fit[["data"]][["Y"]][, "status"] == 1)
  fit[["trisk"]] <- sum(fit[["data"]][["Y"]][, "time"])
  return(fit)
}
