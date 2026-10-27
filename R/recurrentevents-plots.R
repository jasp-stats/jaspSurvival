.sarePredictionPlots <- function(jaspResults, options, measure, predictions) {

  key <- paste0(measure, "Plot")
  if (!is.null(jaspResults[[key]]))
    return()

  fits <- .sareFits(jaspResults, valid = TRUE)
  if (length(fits) == 0)
    return()

  container <- createJaspContainer(title = .sareMeasureLabel(measure))
  container$dependOn(c(.sareFitDependencies, .sarePredictionDependencies, "predictionsOutputPlot", "predictionsDisplayObservedData",
    "predictionsMergeSeriesAcrossSubgroups", "predictionsXAxisTransformation", "predictionsYAxisScale", "plotTheme", "plotLegend", "colorPalette"))
  container$position <- 200 + match(measure, names(.sareMeasureOptions))
  jaspResults[[key]] <- container

  groups    <- if (options[["predictionsMergeSeriesAcrossSubgroups"]]) list(seq_along(fits)) else as.list(seq_along(fits))
  longLabel <- measure %in% c("cumulativeMeanTimeBetweenEvents", "instantaneousMeanTimeBetweenEvents")
  for (i in seq_along(groups)) {
    indices <- groups[[i]]
    output  <- createJaspPlot(
      title  = if (.sapHasSubgroups(options) && length(indices) == 1L) fits[[indices]][["label"]] else "",
      width  = 440,
      height = 320
    )
    output$position <- i
    container[[paste0("plot", i)]] <- output

    plot <- try(.sarePredictionPlot(fits[indices], predictions[indices], measure, options), silent = TRUE)
    if (jaspBase::isTryError(plot)) {
      output$setError(jaspBase::.extractErrorMessage(plot))
    } else {
      size              <- .sapPredictionPlotSize(plot, options, if (longLabel) 650 else 550, if (longLabel) 400 else 320)
      output$width      <- size[["width"]]
      output$height     <- size[["height"]]
      output$plotObject <- plot
    }
  }

  return()
}

.sarePredictionPlot <- function(fits, predictions, measure, options) {

  errors <- vapply(predictions, function(x) jaspBase::isTryError(x[["plot"]]), logical(1))
  if (any(errors)) {
    messages <- vapply(predictions[errors], function(x) gettextf("%1$s: %2$s", x[["label"]], jaspBase::.extractErrorMessage(x[["plot"]])), character(1))
    stop(paste(messages, collapse = "\n"))
  }

  data <- do.call(rbind, lapply(predictions, `[[`, "plot"))
  # Explicit subgroup-layer data require a local check before log-axis transformation.
  if (options[["predictionsYAxisScale"]] == "log" && !any(data[["estimate"]] > 0))
    stop(gettext("No finite predictions remain after the selected axis transformations. Choose a different scale or evaluation range."))

  data[["subgroup"]] <- factor(data[["subgroup"]], levels = vapply(fits, `[[`, character(1), "label"))
  singleTime         <- vapply(split(data[["time"]], data[["subgroup"]], drop = TRUE), function(times) length(unique(times)) == 1L, logical(1))
  singletonGroups    <- names(which(singleTime))
  singletonData      <- data[data[["subgroup"]] %in% singletonGroups, , drop = FALSE]
  curveData          <- data[!data[["subgroup"]] %in% singletonGroups, , drop = FALSE]

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = estimate, colour = subgroup, group = subgroup))

  if (options[["predictionsDisplayConfidenceIntervals"]]) {
    if (nrow(curveData) > 0L) {
      if (options[["confidenceIntervalBounds"]] == "twoSided") {
        plot <- plot + ggplot2::geom_ribbon(
          data        = curveData,
          mapping     = ggplot2::aes(ymin = lower, ymax = upper, fill = subgroup),
          colour      = NA,
          alpha       = .15,
          na.rm       = TRUE,
          show.legend = FALSE
        )
      } else {
        plot <- .sapAddModelBoundLayer(
          plot      = plot,
          data      = curveData,
          level     = options[["predictionsDisplayConfidenceIntervalsLevel"]],
          direction = options[["confidenceIntervalBounds"]],
          x         = "time",
          lower     = "lower",
          upper     = "upper",
          groups    = "subgroup",
          color     = "subgroup",
          fill      = "subgroup",
          alpha     = .15,
          na.rm     = TRUE
        )
      }
    }
    if (nrow(singletonData) > 0L) {
      if (options[["confidenceIntervalBounds"]] == "twoSided") {
        plot <- plot + ggplot2::geom_linerange(
          data        = singletonData,
          mapping     = ggplot2::aes(ymin = lower, ymax = upper),
          na.rm       = TRUE,
          show.legend = FALSE
        )
      } else {
        bound <- options[["confidenceIntervalBounds"]]
        plot  <- plot +
          ggplot2::geom_segment(
            data        = singletonData,
            mapping     = ggplot2::aes(xend = time, yend = .data[[bound]]),
            linetype    = "dashed",
            na.rm       = TRUE,
            show.legend = FALSE
          ) +
          ggplot2::geom_point(
            data        = singletonData,
            mapping     = ggplot2::aes(y = .data[[bound]]),
            shape       = 95,
            size        = 5,
            na.rm       = TRUE,
            show.legend = FALSE
          )
      }
    }
  }

  if (nrow(curveData) > 0L)
    plot <- plot + jaspGraphs::geom_line(data = curveData)
  if (nrow(singletonData) > 0L)
    plot <- plot + ggplot2::geom_point(data = singletonData, size = 1.8)

  if (options[["predictionsDisplayObservedData"]] && measure %in% c("cumulativeEvents", "cumulativeMeanTimeBetweenEvents")) {
    observed <- do.call(rbind, lapply(fits, function(fit) data.frame(
      time     = fit[["time"]],
      estimate = if (measure == "cumulativeEvents") fit[["cumulativeCounts"]] else fit[["time"]] / fit[["cumulativeCounts"]],
      subgroup = fit[["label"]]
    )))
    plot <- plot + ggplot2::geom_point(data = observed, size = 1.6)
  }

  breaks <- unlist(lapply(fits, .sareBreakpoints))
  if (length(breaks) > 0)
    plot <- plot + ggplot2::geom_vline(xintercept = breaks, linetype = "dotted", colour = "grey50")

  if (any(data[["forecast"]])) {
    forecast <- do.call(rbind, lapply(fits, function(fit) data.frame(time = max(fit[["time"]]), subgroup = fit[["label"]])))
    plot     <- plot + ggplot2::geom_vline(
      data        = forecast,
      mapping     = ggplot2::aes(xintercept = time, colour = subgroup),
      linetype    = "dashed",
      show.legend = FALSE
    )
  }

  plot <- plot + ggplot2::labs(
    x       = gettext("Time"),
    y       = paste(strwrap(.sareMeasureLabel(measure), width = 24L), collapse = "\n"),
    colour  = gettext("Subgroup"),
    fill    = if (options[["confidenceIntervalBounds"]] == "twoSided") gettext("Subgroup") else NULL,
    caption = NULL
  )

  probability <- measure %in% c("survivalProbability", "failureProbability")
  if (options[["predictionsXAxisTransformation"]] == "log")
    plot <- plot + ggplot2::scale_x_log10()
  if (options[["predictionsYAxisScale"]] == "log")
    plot <- plot + ggplot2::scale_y_log10()
  else if (probability)
    plot <- plot + ggplot2::scale_y_continuous(limits = c(0, 1), oob = scales::oob_keep)

  return(.sarePlotStyle(plot, options, length(fits), probability = probability))
}

.sarePlotStyle <- function(plot, options, groups, probability = FALSE) {

  labels      <- unique(as.character(plot$data[["subgroup"]]))
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
