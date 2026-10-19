.sargPredictionPlots <- function(jaspResults, options, measure, predictions) {

  key <- paste0(measure, "Plot")
  if (!is.null(jaspResults[[key]]))
    return()
  fits <- .sargFits(jaspResults, valid = TRUE)
  if (length(fits) == 0)
    return()
  container <- createJaspContainer(title = .sargMeasureLabel(measure))
  container$dependOn(c(.sargFitDependencies, .sargPredictionDependencies, .sargPlotDependencies, "predictionsOutputPlot"))
  container$position <- 5 + match(measure, c("cumulativeFailures", "failureIntensity", "cumulativeMtbf", "instantaneousMtbf")) / 10
  jaspResults[[key]] <- container
  groups <- if (options[["predictionsMergeSubgroups"]]) list(seq_along(fits)) else as.list(seq_along(fits))
  for (i in seq_along(groups)) {
    indices <- groups[[i]]
    output <- createJaspPlot(title = if (length(groups) == 1L) "" else fits[[indices]][["label"]], width = 530, height = 380)
    output$position <- i
    container[[paste0("plot", i)]] <- output
    plot <- try(.sargPredictionPlot(fits[indices], predictions[indices], measure, options), silent = TRUE)
    if (inherits(plot, "try-error"))
      output$setError(jaspBase::.extractErrorMessage(plot))
    else
      output$plotObject <- plot
  }
  return()
}

.sargPredictionPlot <- function(fits, predictions, measure, options) {

  errors <- vapply(predictions, function(x) inherits(x[["plot"]], "try-error"), logical(1))
  if (any(errors))
    stop(paste(vapply(predictions[errors], function(x) gettextf("%1$s: %2$s", x[["label"]],
      jaspBase::.extractErrorMessage(x[["plot"]])), character(1)), collapse = "\n"))
  data <- do.call(rbind, lapply(predictions, `[[`, "plot"))
  data[["subgroup"]] <- factor(data[["subgroup"]], levels = vapply(fits, `[[`, character(1), "label"))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = estimate, colour = subgroup, group = subgroup))
  if (options[["predictionsConfidenceIntervals"]])
    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = subgroup),
      colour = NA, alpha = 0.15, na.rm = TRUE, show.legend = FALSE)
  plot <- plot + ggplot2::geom_line(linewidth = 0.7)
  if (options[["predictionsObservedData"]] && measure %in% c("cumulativeFailures", "cumulativeMtbf")) {
    observed <- do.call(rbind, lapply(fits, function(fit) data.frame(time = fit[["time"]],
      estimate = if (measure == "cumulativeFailures") fit[["cumulativeCounts"]] else fit[["time"]] / fit[["cumulativeCounts"]],
      subgroup = fit[["label"]])))
    plot <- plot + ggplot2::geom_point(data = observed, size = 1.6)
  }
  breaks <- unlist(lapply(fits, .sargBreakpoints))
  if (length(breaks) > 0)
    plot <- plot + ggplot2::geom_vline(xintercept = breaks, linetype = "dotted", colour = "grey50")
  forecast <- do.call(rbind, lapply(fits, function(fit) data.frame(time = max(fit[["time"]]), subgroup = fit[["label"]])))
  if (any(data[["forecast"]]))
    plot <- plot + ggplot2::geom_vline(data = forecast, ggplot2::aes(xintercept = time, colour = subgroup), linetype = "dashed", show.legend = FALSE)
  plot <- plot + ggplot2::labs(x = gettext("Time"), y = .sargMeasureLabel(measure), colour = gettext("Subgroup"), fill = gettext("Subgroup"))
  if (options[["predictionsXAxisTransformation"]] == "log")
    plot <- plot + ggplot2::scale_x_log10()
  if (options[["predictionsYAxisTransformation"]] == "log")
    plot <- plot + ggplot2::scale_y_log10()
  plot <- .sargPlotStyle(plot, options, length(fits))
  return(plot)
}

.sargPlotStyle <- function(plot, options, groups) {

  if (groups == 1L) {
    plot <- plot + ggplot2::scale_colour_manual(values = "grey40") + ggplot2::scale_fill_manual(values = "grey40")
  } else {
    plot <- plot + jaspGraphs::scale_JASPcolor_discrete(palette = options[["colorPalette"]]) +
      jaspGraphs::scale_JASPfill_discrete(palette = options[["colorPalette"]])
  }
  plot <- .sapPredictionPlotAddTheme(plot, options)
  if (groups == 1L)
    plot <- plot + ggplot2::theme(legend.position = "none")
  return(plot)
}

.sargResidualPlots <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["residualPlots"]]) || length(fits) == 0)
    return()
  container <- createJaspContainer(title = gettext("Residuals vs. Time"))
  container$position <- 6.1
  container$dependOn(c(.sargFitDependencies, "residualPlot", .sargPlotDependencies))
  jaspResults[["residualPlots"]] <- container
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    output <- createJaspPlot(title = fit[["label"]], width = 530, height = 340)
    output$position <- i
    container[[paste0("plot", i)]] <- output
    result <- try({
      data <- data.frame(time = fit[["time"]], residual = .sargResiduals(fit), subgroup = fit[["label"]])
      plot <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = residual)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
        ggplot2::geom_point(colour = "grey40") +
        ggplot2::labs(x = gettext("Time"), y = if (fit[["method"]] == "MLE") gettext("Pearson Residual") else gettext("Log-scale Residual"))
      if (options[["predictionsXAxisTransformation"]] == "log")
        plot <- plot + ggplot2::scale_x_log10()
      .sargPlotStyle(plot, options, 1L)
    }, silent = TRUE)
    if (inherits(result, "try-error"))
      output$setError(jaspBase::.extractErrorMessage(result))
    else
      output$plotObject <- result
  }
  return()
}
