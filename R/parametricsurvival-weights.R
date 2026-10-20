# Case weights are frequencies: data-derived references must match repeated rows.
.sapCaseWeights <- function(dataset, options) {

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

  order      <- order(value)
  value      <- value[order]
  cumulative <- cumsum(weights[order])
  size       <- sum(weights)
  position   <- 1 + (size - 1) * probabilities
  lower      <- floor(position)
  fraction   <- position - lower
  # Locate order statistics in the expanded sample without allocating it.
  lowerIndex <- findInterval(lower - 1, cumulative) + 1L
  upperIndex <- findInterval(pmin(lower + 1, size) - 1, cumulative) + 1L
  return((1 - fraction) * value[lowerIndex] + fraction * value[upperIndex])
}

# Orthogonal polynomial coefficients for the frequency-expanded sample, without
# allocating its design. The weighted QR is the same least-squares geometry.
.sapFrequencyPolynomial <- function(value, weights, degree) {

  center <- .sapWeightedMean(value, weights)
  x      <- value - center
  design <- outer(x, 0L:degree, `^`) * sqrt(weights)
  QR     <- qr(design)
  basis  <- qr.qy(QR, QR$qr * (row(QR$qr) == col(QR$qr)))
  norm2  <- colSums(basis^2)
  alpha  <- (colSums(x * basis^2) / norm2 + center)[seq_len(degree)]
  return(list(alpha = alpha, norm2 = c(1, norm2)))
}

# flexsurv's moment initializers multiply each time by its weight. Frequency
# data instead require moments of repeated times; survreg-based starts already
# honor frequencies. Match native repeated-row moments without copying a design.
.sapFrequencyInitials <- function(fitCall, distribution) {

  weights <- fitCall[["weights"]]
  if (is.null(weights) || all(weights == 1))
    return(NULL)
  response <- stats::model.response(stats::model.frame(fitCall[["formula"]], fitCall[["data"]]))
  counting <- attr(response, "type") == "counting"
  if (!counting && !distribution %in% c("gamma", "gengamma", "genf", "genf.orig", "gengamma.orig", "gompertz"))
    return(NULL)
  Y <- get("check.flexsurv.response", envir = asNamespace("flexsurv"))(response)
  time <- ifelse(Y[, "status"] == 3 & is.finite(Y[, "time2"]),
    (Y[, "time1"] + Y[, "time2"]) / 2, Y[, "time1"])
  meanTime <- .sapWeightedMean(time, weights)
  positive <- time > 0
  logTime  <- log(time[positive])
  meanLog  <- .sapWeightedMean(logTime, weights[positive])
  sdLog    <- .sapWeightedSd(logTime, weights[positive])
  return(switch(distribution,
    gamma         = { variance <- .sapWeightedSd(time, weights)^2; c(meanTime^2 / variance, meanTime / variance) },
    gengamma      = c(meanLog, sdLog, 0),
    genf          = c(meanLog, sdLog, 0, 1),
    genf.orig     = c(meanLog, sdLog, 1, 1),
    gengamma.orig = c(1, meanTime, 1),
    gompertz      = c(.001, 1 / meanTime),
    exp           = 1 / meanTime,
    weibull       = c(1.64 / sdLog^2, exp(meanLog + .572)),
    lnorm         = c(meanLog, sdLog),
    llogis        = {
      scale <- .sapFrequencyQuantile(time, weights, .5)
      shape <- 1 / log(.sapFrequencyQuantile(time, weights, .25) / scale, base = 3)
      c(if (shape < 0) 1 else shape, scale)
    }))
}

# Compress duplicate fitting rows, retaining row-level diagnostics and exports.
.sapmCompressedFit <- function(fitCall, fitFunction = flexsurv::flexsurvreg) {

  dataset   <- fitCall[["data"]]
  formulas  <- c(list(fitCall[["formula"]]), fitCall[["anc"]])
  variables <- unique(unlist(lapply(formulas, all.vars), use.names = FALSE))
  keys      <- lapply(dataset[variables], function(value) match(value, unique(value)))
  keys      <- do.call(paste, c(keys, sep = "|"))
  rows      <- which(!duplicated(keys))
  # Avoid reconstructing full model frames for negligible reductions in work.
  if (length(rows) > 0.9 * nrow(dataset))
    return(.sapFrequencyFitSummary(do.call(fitFunction, fitCall)))

  index   <- match(keys, keys[rows])
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

  # Downstream diagnostics and exports expect one entry per input row.
  expandMatrix <- function(value) {
    if (is.null(value))
      return(NULL)
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
  return(.sapFrequencyFitSummary(fit))
}

.sapFrequencyFitSummary <- function(fit) {

  if (!inherits(fit, "flexsurvreg"))
    return(fit)
  weights <- fit[["data"]][["m"]][["(weights)"]]
  if (is.null(weights))
    weights <- rep(1, nrow(fit[["data"]][["m"]]))
  means <- unlist(lapply(fit[["data"]][["mml"]], function(value) {
    if (is.null(value))
      return(numeric(0))
    return(.sapWeightedColumnMeans(value[, -1L, drop = FALSE], weights))
  }), use.names = FALSE)
  fit[["datameans"]] <- stats::setNames(means, names(fit[["datameans"]]))
  fit[["N"]] <- sum(weights)
  fit[["BIC"]] <- -2 * fit[["loglik"]] + log(fit[["N"]]) * fit[["npars"]]
  fit[["events"]] <- sum(weights * (fit[["data"]][["Y"]][, "status"] == 1))
  fit[["trisk"]] <- sum(weights * fit[["data"]][["Y"]][, "time"])
  return(fit)
}
