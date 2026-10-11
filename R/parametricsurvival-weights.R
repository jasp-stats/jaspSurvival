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
