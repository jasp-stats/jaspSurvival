# Contour surfaces: axes, levels, observed support and colour scales.

# Plan axes from predictor ranges, before raster cells add their half-widths.
.sapPredictionContourPlotAxes <- function(fit, options, axes) {

  ranges <- lapply(axes, function(variable)
    range(unlist(lapply(fit, .sapPredictionAxisRange, options = options, variable = variable), use.names = FALSE)))
  mappings <- lapply(axes, function(variable)
    .sapPredictionAxisScale(fit, variable, .sapPredictionAxisSettings(options, variable)[["scale"]], options = options))
  plot <- ggplot2::ggplot(data.frame(x = ranges[[1L]], y = ranges[[2L]]), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_blank()
  positions <- c("x", "y")
  for (i in seq_along(mappings))
    if (!is.null(mappings[[i]]))
      plot <- plot + .sapPredictionPositionScale(mappings[[i]], positions[i])
  plot     <- .sapPredictionPrettyAxes(plot)
  scales   <- lapply(positions, plot$scales$get_scales)
  bounds   <- lapply(scales, function(scale) sort(scale$get_transformation()$inverse(scale$limits)))
  settings <- .sapPredictionContourSettings(options)
  for (i in seq_along(bounds)) {
    prefix <- paste0("predictionsCovariateXAxisSettings", toupper(positions[i]), "Axis")
    settings[[paste0(prefix, "From")]] <- sprintf("%.17g", bounds[[i]][1L])
    settings[[paste0(prefix, "To")]] <- sprintf("%.17g", bounds[[i]][2L])
  }
  return(list(mappings = mappings, scales = scales, bounds = bounds, settings = settings))
}

# Contour levels are outcome values; axes and evaluation slices keep their units.
.sapPredictionContourLevelOptions <- c("predictionsContoursLevels", "predictionsContoursLevelsNumber",
  "predictionsContoursCommonAcrossFigures", paste0("predictionsContoursLevels", c("SurvivalTime", "RelativeSurvivalTime",
    "SurvivalProbability", "FailureProbability", "Hazard", "CumulativeHazard", "RestrictedMeanSurvivalTime")))
.sapPredictionContourCustomLevels <- function(options, measure) {

  suffix <- paste0(toupper(substring(measure, 1L, 1L)), substring(measure, 2L))
  text   <- trimws(options[[paste0("predictionsContoursLevels", suffix)]])
  if (!nzchar(text))
    return(numeric(0))

  values <- .sapNumericExpression(text, multiple = TRUE)
  if (!.sapFiniteNumeric(values))
    stop(gettext("Contour levels must be finite values or numeric expressions separated by commas."))
  probability <- measure %in% c("survivalProbability", "failureProbability")
  if (any(values < 0) || (probability && any(values > 1)))
    stop(if (probability) gettext("Probability contour levels must be between 0 and 1.") else gettext("Contour levels must be non-negative."))
  return(sort(unique(values)))
}

.sapPredictionContourNumberLevels <- function(values, number) {

  values <- values[is.finite(values)]
  if (!length(values) || diff(range(values)) == 0)
    return(numeric(0))

  limits <- range(values)
  # Pretty spacing makes the requested count approximate; endpoints do not form contours.
  levels <- jaspGraphs::getPrettyAxisBreaks(limits, n = number + 1L)
  return(levels[is.finite(levels) & levels > limits[1L] & levels < limits[2L]])
}

.sapPredictionContourLevels <- function(data, options, measure, common = NULL) {

  panels <- unique(as.character(data[["Panel"]]))
  custom <- if (options[["predictionsContoursLevels"]] == "custom") .sapPredictionContourCustomLevels(options, measure)
  if (options[["predictionsContoursLevels"]] == "number" && options[["predictionsContoursCommonAcrossFigures"]] && is.null(common))
    common <- .sapPredictionContourNumberLevels(data[["estimate"]], options[["predictionsContoursLevelsNumber"]])
  return(stats::setNames(lapply(panels, function(panel) {
    if (options[["predictionsContoursLevels"]] == "custom")
      return(custom)
    if (!is.null(common))
      return(common)
    return(.sapPredictionContourNumberLevels(
      data[["estimate"]][as.character(data[["Panel"]]) == panel],
      options[["predictionsContoursLevelsNumber"]]
    ))
  }), panels))
}

.sapPredictionCommonContourLevels <- function(groups, options, type, measure) {

  groups <- lapply(groups, function(fit) fit[!vapply(fit, jaspBase::isTryError, logical(1))])
  groups <- Filter(length, groups)
  if (!length(groups) || !.saSurvivalReady(options))
    return(numeric(0))

  axes    <- .sapModelPredictionAxes(options)
  targets <- unlist(lapply(groups, function(fit) {
    settings <- .sapPredictionContourPlotAxes(fit, options, axes)[["settings"]]
    settings[["predictionsDisplayConfidenceIntervals"]] <- FALSE
    at <- .sapModelPredictionAt(.sapPlotTimeFit(fit), settings, type, "panels")
    lapply(fit, function(model) list(model = model, at = at, settings = settings,
      scenario = .sapPredictionContourScenarios(model, settings, axes, 65L, plot = TRUE)))
  }), recursive = FALSE, use.names = FALSE)
  ticks <- sum(vapply(targets, function(target) ceiling(nrow(target[["scenario"]][["data"]]) / 128L), numeric(1)))
  startProgressbar(expectedTicks = ticks, label = gettext("Computing common contour levels"))
  ranges <- lapply(targets, function(target) {
    blocks <- .sapPredictionContourBlocks(target[["model"]], target[["scenario"]], target[["settings"]], type, target[["at"]], measure,
      summarize = function(value) {
        estimates <- value[["estimate"]]
        estimates <- estimates[is.finite(estimates)]
        return(if (length(estimates)) range(estimates))
      })
    unlist(blocks, use.names = FALSE)
  })
  return(.sapPredictionContourNumberLevels(unlist(ranges, use.names = FALSE), options[["predictionsContoursLevelsNumber"]]))
}

# Covariate support includes every fitting row, regardless of censoring status.
.sapPredictionContourObserved <- function(fit, settings, axes, records, type) {

  labels   <- .sapPredictionFitLabels(fit, settings)
  observed <- .saSafeRbind(lapply(seq_along(fit), function(i) {
    training    <- attr(fit[[i]], "dataset")
    scenarios   <- .sapModelPredictionScenarios(fit[[i]], settings, timeRole = "panels")
    roles       <- data.frame(Line = scenarios[["lines"]], Panel = scenarios[["panels"]])
    evaluations <- if (type %in% c("mean", "median")) "" else
      .sapModelPredictionEvaluationLabels(unique(records[[i]][["at"]]), type)
    .saSafeRbind(lapply(which(!duplicated(roles)), function(j) {
      rows <- scenarios[["observedRows"]][[j]]
      if (!length(rows))
        return(NULL)
      .saSafeRbind(lapply(evaluations, function(evaluation) {
        data.frame(x = training[[axes[1L]]][rows], y = training[[axes[2L]]][rows],
          ObservedRow = rownames(training)[rows], Subgroup = attr(fit[[i]], "subgroup"),
          Panel       = .sapPredictionSeriesLabel(if (length(fit) > 1L) labels[i], scenarios[["panels"]][j], evaluation))
      }))
    }))
  }))
  if (is.null(observed))
    return(NULL)
  return(observed[!duplicated(observed[c("Subgroup", "ObservedRow", "Panel")]), , drop = FALSE])
}

.sapAddContourObservedLayer <- function(plot, observed) {

  if (is.null(observed))
    return(plot)

  keep <- is.finite(observed[["x"]]) & is.finite(observed[["y"]])
  for (position in c("x", "y")) {
    scale     <- plot$scales$get_scales(position)
    values    <- suppressWarnings(scale$get_transformation()$transform(observed[[position]]))
    bounds    <- scale$get_limits()
    tolerance <- 64 * .Machine$double.eps * max(abs(bounds))
    keep      <- keep & is.finite(values) & values >= bounds[1L] - tolerance & values <= bounds[2L] + tolerance
  }
  observed <- observed[keep, , drop = FALSE]
  if (!nrow(observed))
    return(plot)
  return(plot + ggplot2::geom_point(data = observed, ggplot2::aes(x = x, y = y),
    inherit.aes = FALSE, shape = 21, fill = "grey30", colour = "white", alpha = .65,
    size        = 1.5, stroke = .25, show.legend = FALSE))
}

# Colour controls use outcome units and never alter contour estimates or levels.
.sapPredictionHeatmapOptions <- function(measure) {

  suffix <- paste0(toupper(substring(measure, 1L, 1L)), substring(measure, 2L))
  return(paste0("predictionsHeatmapScale", suffix, c("Transformation", "From", "To")))
}

.sapPredictionHeatmapScale <- function(options, measure, values) {

  keys        <- .sapPredictionHeatmapOptions(measure)
  logarithmic <- options[[keys[1L]]] == "log"
  values      <- values[is.finite(values)]
  if (logarithmic)
    values <- values[values > 0]
  if (!length(values))
    stop(gettext("A logarithmic heatmap scale requires positive predicted values."))
  bounds    <- range(values)
  specified <- FALSE
  for (i in seq_along(bounds)) {
    text <- trimws(options[[keys[i + 1L]]])
    if (!nzchar(text))
      next
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
  if (logarithmic && any(bounds <= 0))
    stop(gettext("Logarithmic heatmap limits must be positive."))
  transformed <- if (logarithmic) log10(bounds) else bounds
  intervals   <- 5L
  tolerance   <- 64 * .Machine$double.eps * max(abs(transformed))
  for (attempt in seq_len(8L)) {
    ticks <- jaspGraphs::getPrettyAxisBreaks(transformed, n = intervals)
    if (logarithmic)
      ticks <- 10^ticks
    ticks <- ticks[is.finite(ticks) & if (logarithmic) ticks > 0 else ticks >= 0]
    if (probability)
      ticks <- ticks[ticks <= 1]
    displayed <- if (logarithmic) log10(ticks) else ticks
    if (length(ticks) && min(displayed) <= transformed[1L] + tolerance && max(displayed) >= transformed[2L] - tolerance)
      break
    intervals <- intervals * 2L
  }
  # The colour bar starts and ends at pretty ticks, including for entered limits.
  bounds <- range(ticks)
  labels <- function(x) {
    digits <- if (logarithmic) 2L else 4L
    # Retain enough precision to distinguish ticks on narrow ranges.
    while (anyDuplicated(signif(x, digits)) && digits < 15L)
      digits <- digits + 1L
    return(vapply(x, function(value) format(signif(value, digits), trim = TRUE), character(1)))
  }
  scale <- ggplot2::scale_fill_gradient(low = "white",
    high      = jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])$palette(1L)[1L],
    limits    = bounds, breaks = ticks, labels = labels,
    transform = if (logarithmic) "log10" else "identity", oob = scales::squish, na.value = "grey95")
  return(list(scale = scale, bounds = bounds))
}
