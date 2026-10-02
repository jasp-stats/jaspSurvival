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
