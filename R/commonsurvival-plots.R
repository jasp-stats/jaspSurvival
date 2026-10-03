# Trim CI limits only, retaining every estimate and the full plotted bands.
# Callers must use oob_keep. Adaptive grids are weighted by displayed x span.
.saPlotEstimateRange <- function(estimate, lCi = numeric(0), uCi = numeric(0), bounded = FALSE,
                                 trimLower = FALSE, at = NULL, group = rep(1, length(at))) {

  estimate <- estimate[is.finite(estimate)]
  ci       <- c(lCi, uCi)
  weights  <- if (is.null(at)) rep(1, length(ci)) else {
    nodes <- .saPlotCiWeights(at, is.finite(lCi) | is.finite(uCi), group)
    c(if (length(lCi) > 0) nodes, if (length(uCi) > 0) nodes)
  }
  keep     <- is.finite(ci) & weights > 0
  ci       <- ci[keep]
  weights  <- weights[keep]
  full     <- range(c(estimate, ci))

  if (bounded || length(ci) == 0)
    return(full)

  # Each curve has equal weight, independently of its adaptive point count.
  order         <- order(ci)
  cumulative    <- cumsum(weights[order]) / sum(weights)
  probabilities <- if (trimLower) c(0.025, 0.975) else c(0, 0.95)
  ciLimits      <- vapply(probabilities, function(p) ci[order[which(cumulative >= p)[1]]], numeric(1))
  limits        <- range(c(estimate, ciLimits))

  if (length(estimate) > 0) {
    lineRange <- range(estimate)
    lineSpan  <- diff(lineRange)
    if (lineSpan == 0)
      lineSpan <- max(abs(lineRange))
    padding <- 0.25 * lineSpan
    # Limit trimming rather than enlarge a band that originally had less room.
    if (limits[1] > full[1])
      limits[1] <- max(full[1], min(limits[1], lineRange[1] - padding))
    if (limits[2] < full[2])
      limits[2] <- min(full[2], max(limits[2], lineRange[2] + padding))
  }

  return(limits)
}

.saPlotCiWeights <- function(at, hasCi, group) {

  weights <- numeric(length(at))
  indices <- which(is.finite(at) & hasCi)
  for (index in split(indices, factor(group[indices], exclude = NULL))) {
    index <- index[order(at[index])]
    gaps  <- diff(at[index])
    if (length(gaps) == 0 || sum(gaps) == 0) {
      weights[index] <- 1 / length(index)
    } else {
      # Trapezoidal weights assign each endpoint half its neighbouring intervals.
      weights[index] <- (c(0, gaps) + c(gaps, 0)) / (2 * sum(gaps))
    }
  }
  return(weights)
}
