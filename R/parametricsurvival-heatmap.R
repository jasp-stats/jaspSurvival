# Colour controls use outcome units and never alter contour estimates or levels.
.sapPredictionHeatmapOptions <- function(measure) {
  suffix <- paste0(toupper(substring(measure, 1L, 1L)), substring(measure, 2L))
  return(paste0("predictionsHeatmapScale", suffix, c("Transformation", "From", "To")))
}
.sapPredictionHeatmapScale <- function(options, measure, values) {
  keys <- .sapPredictionHeatmapOptions(measure)
  logarithmic <- options[[keys[1L]]] == "log"
  values <- values[is.finite(values)]
  if (logarithmic) values <- values[values > 0]
  if (!length(values)) stop(gettext("A logarithmic heatmap scale requires positive predicted values."))
  bounds <- range(values)
  specified <- FALSE
  for (i in seq_along(bounds)) {
    text <- trimws(options[[keys[i + 1L]]])
    if (!nzchar(text)) next
    value <- .sapNumericExpression(text)
    if (!.sapFiniteNumeric(value, expectedLength = 1L))
      stop(gettext("Heatmap limits require finite scalar values or expressions."))
    bounds[i] <- value
    specified <- TRUE
  }
  if (bounds[1L] > bounds[2L] || (specified && bounds[1L] == bounds[2L]))
    stop(gettext("From must be smaller than To for the heatmap scale."))
  probability <- measure %in% c("survivalProbability", "failureProbability")
  if (any(bounds < 0) || (probability && any(bounds > 1)))
    stop(if (probability) gettext("Probability heatmap limits must be between 0 and 1.") else gettext("Heatmap limits must be non-negative."))
  if (logarithmic && any(bounds <= 0)) stop(gettext("Logarithmic heatmap limits must be positive."))
  transformed <- if (logarithmic) log10(bounds) else bounds
  intervals  <- 5L
  tolerance  <- 64 * .Machine$double.eps * max(abs(transformed))
  for (attempt in seq_len(8L)) {
    ticks <- jaspGraphs::getPrettyAxisBreaks(transformed, n = intervals)
    if (logarithmic) ticks <- 10^ticks
    ticks <- ticks[is.finite(ticks) & if (logarithmic) ticks > 0 else ticks >= 0]
    if (probability) ticks <- ticks[ticks <= 1]
    displayed <- if (logarithmic) log10(ticks) else ticks
    if (length(ticks) && min(displayed) <= transformed[1L] + tolerance && max(displayed) >= transformed[2L] - tolerance) break
    intervals <- intervals * 2L
  }
  # The colour bar starts and ends at pretty ticks, including for entered limits.
  bounds <- range(ticks)
  labels <- function(x) {
    digits <- if (logarithmic) 2L else 4L
    # Retain enough precision to distinguish ticks on narrow ranges.
    while (anyDuplicated(signif(x, digits)) && digits < 15L) digits <- digits + 1L
    return(vapply(x, function(value) format(signif(value, digits), trim = TRUE), character(1)))
  }
  scale <- ggplot2::scale_fill_gradient(low = "white",
    high = jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])$palette(1L)[1L],
    limits = bounds, breaks = ticks, labels = labels,
    transform = if (logarithmic) "log10" else "identity", oob = scales::squish, na.value = "grey95")
  return(list(scale = scale, bounds = bounds))
}
