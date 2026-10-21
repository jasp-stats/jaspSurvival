.sarePredictionPlots <- function(jaspResults, options, measure, predictions) {

  key <- paste0(measure, "Plot")
  if (!is.null(jaspResults[[key]]))
    return()
  fits <- .sareFits(jaspResults, valid = TRUE)
  if (length(fits) == 0)
    return()
  container <- createJaspContainer(title = .sareMeasureLabel(measure))
  container$dependOn(c(.sareFitDependencies, .sarePredictionDependencies, .sarePlotDependencies, "predictionsOutputPlot"))
  container$position <- 5 + match(measure, .sareMeasureNames) / 10
  jaspResults[[key]] <- container
  groups <- if (options[["predictionsMergeSeriesAcrossSubgroups"]]) list(seq_along(fits)) else as.list(seq_along(fits))
  for (i in seq_along(groups)) {
    indices <- groups[[i]]
    output <- createJaspPlot(title = if (length(groups) == 1L) "" else fits[[indices]][["label"]], width = 440, height = 320)
    output$position <- i
    container[[paste0("plot", i)]] <- output
    plot <- try(.sarePredictionPlot(fits[indices], predictions[indices], measure, options), silent = TRUE)
    if (jaspBase::isTryError(plot))
      output$setError(jaspBase::.extractErrorMessage(plot))
    else {
      longLabel <- measure %in% c("cumulativeMeanTimeBetweenEvents", "instantaneousMeanTimeBetweenEvents")
      size <- .sapPredictionPlotSize(plot, options, if (longLabel) 650 else 550, if (longLabel) 400 else 320)
      output$width  <- size[["width"]]
      output$height <- size[["height"]]
      output$plotObject <- plot
    }
  }
  return()
}

.sarePredictionPlot <- function(fits, predictions, measure, options) {

  errors <- vapply(predictions, function(x) jaspBase::isTryError(x[["plot"]]), logical(1))
  if (any(errors))
    stop(paste(vapply(predictions[errors], function(x) gettextf("%1$s: %2$s", x[["label"]],
      jaspBase::.extractErrorMessage(x[["plot"]])), character(1)), collapse = "\n"))
  data <- do.call(rbind, lapply(predictions, `[[`, "plot"))
  data[["subgroup"]] <- factor(data[["subgroup"]], levels = vapply(fits, `[[`, character(1), "label"))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = estimate, colour = subgroup, group = subgroup))
  if (options[["predictionsDisplayConfidenceIntervals"]]) {
    if (options[["confidenceIntervalBounds"]] == "twoSided")
      plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = subgroup),
        colour = NA, alpha = .15, na.rm = TRUE, show.legend = FALSE)
    else
      plot <- .sapAddModelBoundLayer(plot, data, options[["predictionsDisplayConfidenceIntervalsLevel"]], options[["confidenceIntervalBounds"]],
        x = "time", lower = "lower", upper = "upper", groups = "subgroup", color = "subgroup", fill = "subgroup", alpha = .15, na.rm = TRUE)
  }
  plot <- plot + jaspGraphs::geom_line()
  if (options[["predictionsDisplayObservedData"]] && measure %in% c("cumulativeEvents", "cumulativeMeanTimeBetweenEvents")) {
    observed <- do.call(rbind, lapply(fits, function(fit) data.frame(time = fit[["time"]],
      estimate = if (measure == "cumulativeEvents") fit[["cumulativeCounts"]] else fit[["time"]] / fit[["cumulativeCounts"]],
      subgroup = fit[["label"]])))
    plot <- plot + ggplot2::geom_point(data = observed, size = 1.6)
  }
  breaks <- unlist(lapply(fits, .sareBreakpoints))
  if (length(breaks) > 0)
    plot <- plot + ggplot2::geom_vline(xintercept = breaks, linetype = "dotted", colour = "grey50")
  forecast <- do.call(rbind, lapply(fits, function(fit) data.frame(time = max(fit[["time"]]), subgroup = fit[["label"]])))
  if (any(data[["forecast"]]))
    plot <- plot + ggplot2::geom_vline(data = forecast, ggplot2::aes(xintercept = time, colour = subgroup), linetype = "dashed", show.legend = FALSE)
  axisLabel <- paste(strwrap(.sareMeasureLabel(measure), width = 24L), collapse = "\n")
  plot <- plot + ggplot2::labs(x = gettext("Time"), y = axisLabel, colour = gettext("Subgroup"),
    fill = if (options[["confidenceIntervalBounds"]] == "twoSided") gettext("Subgroup") else NULL, caption = NULL)
  if (options[["predictionsXAxisTransformation"]] == "log")
    plot <- plot + ggplot2::scale_x_log10()
  if (options[["predictionsYAxisScale"]] == "log")
    plot <- plot + ggplot2::scale_y_log10()
  else if (measure %in% c("survivalProbability", "failureProbability"))
    plot <- plot + ggplot2::scale_y_continuous(limits = c(0, 1), oob = scales::oob_keep)
  plot <- .sarePlotStyle(plot, options, length(fits), probability = measure %in% c("survivalProbability", "failureProbability"))
  return(plot)
}

.sarePlotStyle <- function(plot, options, groups, probability = FALSE) {

  labels <- unique(as.character(plot$data[["subgroup"]]))
  colourScale <- jaspGraphs::scale_JASPcolor_discrete(palette = options[["colorPalette"]], limits = labels)
  fillScale   <- jaspGraphs::scale_JASPfill_discrete(palette = options[["colorPalette"]], limits = labels)
  colourScale$palette <- .sapPlotPalette(colourScale$palette, groups)
  fillScale$palette   <- .sapPlotPalette(fillScale$palette, groups)
  plot <- plot + colourScale + fillScale
  plot <- .sapPredictionPlotAddTheme(plot, options)
  if (groups == 1L)
    plot <- plot + ggplot2::theme(legend.position = "none")
  # Match AFT: axis ticks span curves, intervals and observed/reference overlays.
  return(.sapPredictionPrettyAxes(plot, probabilityAxis = probability))
}
