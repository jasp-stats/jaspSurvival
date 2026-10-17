# Prediction geometry and layout share fitted models and reference scenarios.
.sapModelPredictionDisplayDependencies <- c("predictionsOutputPlotType", "predictionsDisplayPredictionInterval",
  "predictionsDisplayPredictionIntervalLevel", "predictionsUnassignedReferenceValues", "predictionsUnassignedReferenceValuesCustomValues")

.sapModelPredictionAxes <- function(options) {

  axes <- if (options[["predictionsOutputPlotType"]] == "contour")
    c(unlist(options[["predictionsContourXAxis"]], use.names = FALSE), unlist(options[["predictionsContourYAxis"]], use.names = FALSE)) else
    unlist(options[["predictionsXAxisVariables"]], use.names = FALSE)
  return(axes[nzchar(axes)])
}

.sapRelativeLifeAvailable <- function(options) {

  if (options[["predictionsPredictFor"]] != "variables")
    return(FALSE)

  axes <- .sapModelPredictionAxes(options)
  return(length(axes) > 0L && !options[["predictionsLifetimeTimeId"]] %in% axes &&
    options[["predictionsOutputPlotType"]] == "standard")
}

.sapModelPredictionActive <- function(options) {

  if (options[["predictionsPredictFor"]] != "variables")
    return(FALSE)
  if (!(options[["predictionsOutputPlot"]] || options[["predictionsOutputTable"]]) || !length(.sapModelPredictionAxes(options)))
    return(FALSE)
  if (options[["predictionsOutputPlotType"]] == "contour" && !.sapPredictionContourReady(options))
    return(FALSE)

  measures <- .sapModelPredictionOptions
  if (!.sapRelativeLifeAvailable(options))
    measures <- setdiff(measures, "predictionsOutcomeRelativeSurvivalTime")
  if (!length(.sapSelectedLifetimeSummaries(options)))
    measures <- setdiff(measures, c("predictionsOutcomeSurvivalTime", "predictionsOutcomeRelativeSurvivalTime"))
  return(any(vapply(measures, function(measure) options[[measure]], logical(1))))
}

.sapPredictionAxisOptions <- function(options, axis) {

  custom <- options[["predictionsUnassignedReferenceValues"]] == "custom"
  if (custom)
    values <- .sapPredictionCustomValues(options)
  options[["predictionsXAxisVariables"]] <- axis
  if (custom) {
    unassigned <- setdiff(.sapOptionsVariables(options), .sapPredictionTableVariables(options))
    options[["predictionsUnassignedReferenceValuesCustomValues"]] <- lapply(values[unassigned], function(value) list(values = list(value)))
  }
  return(options)
}

.sapValidatePredictionReferences <- function(dataset, options) {

  if (!.sapModelPredictionActive(options))
    return()
  if (options[["predictionsUnassignedReferenceValues"]] == "population")
    return()
  variables <- .sapOptionsVariables(options)
  # The selected coding reference is validated for each fitting subgroup by
  # the equation compiler. Here only arbitrary Custom entries need validation.
  checked <- try(lapply(variables, function(variable) .sapPredictionReferenceValue(dataset[[variable]], variable, options,
    .sapCaseWeights(dataset, options), if (is.factor(dataset[[variable]])) levels(dataset[[variable]])[1L] else NULL)), silent = TRUE)
  if (jaspBase::isTryError(checked))
    .quitAnalysis(conditionMessage(attr(checked, "condition")))
  return()
}

.sapPredictionFitLabels <- function(fit, options, exclude = character(0)) {

  families   <- vapply(fit, function(model) attr(model, "distribution"), character(1))
  components <- vapply(fit, function(model) as.numeric(attr(model, "components")), numeric(1))
  subgroups  <- vapply(fit, function(model) attr(model, "subgroupLabel"), character(1))
  models     <- vapply(fit, function(model) attr(model, "modelTitle"), character(1))
  return(vapply(seq_along(fit), function(i) .sapPredictionSeriesLabel(
    if (!"distribution" %in% exclude && length(unique(families)) > 1L) families[i],
    if (length(unique(components)) > 1L) .sapComponentsLabel(components[i]),
    if (!"subgroup" %in% exclude && length(unique(subgroups)) > 1L) subgroups[i],
    if (length(unique(models)) > 1L) models[i]), character(1)))
}

.sapPredictionObservedPanels <- function(observed, data, type) {

  labels <- .sapModelPredictionEvaluationLabels(unique(data[["at"]]), type)
  return(.saSafeRbind(lapply(labels, function(label) {
    value <- observed
    value[["Panel"]] <- vapply(value[["Panel"]], function(panel) .sapPredictionSeriesLabel(panel, label), character(1))
    value
  })))
}

.sapPredictionRugData <- function(fit, options, data, type, timeRole) {

  labels  <- .sapPredictionFitLabels(fit, options)
  records <- lapply(seq_along(fit), function(i) {
    observed <- .sapModelPredictionObserved(list(fit[[i]]), options)
    if (is.null(observed))
      return(NULL)
    observed <- observed[!observed[["Event"]], , drop = FALSE]
    if (!nrow(observed))
      return(NULL)
    if (!type %in% c("mean", "median") && timeRole %in% c("lines", "panels")) {
      evaluations <- .sapModelPredictionEvaluationLabels(unique(data[["at"]]), type)
      observed    <- .saSafeRbind(lapply(evaluations, function(evaluation) {
        value <- observed
        field <- if (timeRole == "lines") "Line" else "Panel"
        value[[field]] <- vapply(value[[field]], function(group) .sapPredictionSeriesLabel(group, evaluation), character(1))
        value
      }))
    }
    observed[["Series"]] <- vapply(observed[["Line"]], function(line) .sapPredictionSeriesLabel(labels[i], line), character(1))
    observed[["Series"]][!nzchar(observed[["Series"]])] <- gettext("Prediction")
    observed[["Panel"]] <- factor(observed[["Panel"]], levels = unique(as.character(data[["Panel"]])))
    observed
  })
  return(.saSafeRbind(records))
}

.sapModelPredictionRange <- function(fit, options, type) {

  level         <- options[["predictionsDisplayPredictionIntervalLevel"]]
  probabilities <- unname(.sapBoundProbabilities(level, "twoSided"))
  pointOptions  <- options
  pointOptions[["predictionsDisplayConfidenceIntervals"]] <- FALSE
  fitLabels <- .sapPredictionFitLabels(fit, options)
  records   <- lapply(seq_along(fit), function(i) {
    scenarios <- .sapModelPredictionScenarios(fit[[i]], pointOptions, plot = TRUE)
    values    <- .sapModelPredictionValues(fit[[i]], scenarios, pointOptions, "quantile", probabilities)
    lower     <- values[values[["at"]] == probabilities[1L], , drop = FALSE]
    upper     <- values[values[["at"]] == probabilities[2L], , drop = FALSE]
    range     <- lower[c("x", "Scenario")]
    range[["lower"]] <- lower[["estimate"]]
    range[["upper"]] <- upper[["estimate"]]
    range[["Panel"]] <- scenarios[["panels"]]
    range[["Series"]] <- vapply(scenarios[["lines"]], function(line) .sapPredictionSeriesLabel(fitLabels[i], line), character(1))
    range[["Series"]][!nzchar(range[["Series"]])] <- gettext("Prediction")
    range[["Group"]] <- paste(i, scenarios[["lines"]], scenarios[["panels"]], sep = "|")
    if (type == "quantile" && scenarios[["timeRole"]] == "panels") {
      labels <- .sapModelPredictionEvaluationLabels(.sapOptions2PredictionQuantile(options), "quantile")
      range  <- .saSafeRbind(lapply(labels, function(label) {
        value <- range
        value[["Panel"]] <- vapply(value[["Panel"]], function(panel) .sapPredictionSeriesLabel(panel, label), character(1))
        value
      }))
    }
    if (type == "quantile" && scenarios[["timeRole"]] == "lines" && is.factor(scenarios[["x"]])) {
      labels <- .sapModelPredictionEvaluationLabels(.sapOptions2PredictionQuantile(options), "quantile")
      range  <- .saSafeRbind(lapply(labels, function(label) {
        value <- range
        value[["Series"]] <- vapply(scenarios[["lines"]], function(line) .sapPredictionSeriesLabel(fitLabels[i], line, label), character(1))
        value
      }))
    }
    range
  })
  return(.saSafeRbind(records))
}

.sapPredictionAxisOutput <- function(fit, options, builder) {

  axes <- .sapModelPredictionAxes(options)
  if (!length(axes))
    return(builder(fit, .sapPredictionAxisOptions(options, "")))
  if (length(axes) == 1L)
    return(builder(fit, .sapPredictionAxisOptions(options, axes[1L])))

  container <- createJaspContainer()
  for (i in seq_along(axes)) {
    child <- builder(fit, .sapPredictionAxisOptions(options, axes[i]))
    child$title <- jaspBase::decodeColNames(axes[i])
    child$position <- i
    container[[paste0("axis", i)]] <- child
  }
  return(container)
}

.sapModelPredictionPlot <- function(fit, options, type, measure = type, contourLevels = NULL) {

  fit <- fit[!vapply(fit, jaspBase::isTryError, logical(1))]
  if (!.saSurvivalReady(options) || !length(fit))
    return(createJaspPlot())
  if (options[["predictionsOutputPlotType"]] == "contour")
    return(.sapModelPredictionContour(fit, options, type, measure, contourLevels))

  axes <- .sapModelPredictionAxes(options)
  if (measure == "relativeSurvivalTime" &&
    (!length(axes) || options[["predictionsLifetimeTimeId"]] %in% axes)) {
    plot <- createJaspPlot()
    plot$setError(gettext("Assign predictors to X-axis variables and Time to Separate lines or Separate plots."))
    return(plot)
  }
  if (options[["predictionsDisplayCombinePredictorPlots"]] && length(axes) > 1L) {
    outputs <- lapply(
      axes,
      function(axis) .sapModelPredictionPlotForAxis(fit, .sapPredictionAxisOptions(options, axis), type, measure)
    )
    plots  <- lapply(outputs, function(output) output$plotObject)
    width  <- max(vapply(outputs, function(output) output$width, numeric(1))) * 2
    height <- max(vapply(outputs, function(output) output$height, numeric(1))) * ceiling(length(plots) / 2)
    result <- createJaspPlot(width = width, height = height)
    cells  <- c(seq_along(plots), rep(NA_integer_, 2L * ceiling(length(plots) / 2) - length(plots)))
    result$plotObject <- jaspGraphs:::jaspGraphsPlot$new(subplots = plots,
      layout = matrix(cells, ncol = 2, byrow = TRUE))
    return(result)
  }
  return(.sapPredictionAxisOutput(
    fit,
    options,
    function(models, settings) .sapModelPredictionPlotForAxis(models, settings, type, measure)
  ))
}

# Compare reference and requested lifetimes using matched joint coefficient draws.
.sapPredictionRatioSummary <- function(fit, scenarios, options, at, type = "quantile") {

  data      <- scenarios[["data"]]
  variable  <- scenarios[["xVariable"]]
  reference <- data
  training  <- attr(fit, "dataset")
  reference[[variable]] <- .sapPredictionReferenceValue(training[[variable]], variable, options, .sapCaseWeights(training, options),
    if (is.factor(training[[variable]])) .sapFittedFactorReference(fit, variable) else NULL)
  currentFit   <- .sapPredictionConditionedFit(fit, data, options, .sapPredictionUsedVariables(options))
  referenceFit <- .sapPredictionConditionedFit(fit, reference, options, setdiff(.sapPredictionUsedVariables(options), variable))
  evaluate     <- function(model) {
    if (!is.null(attr(model, "predictionPopulation")))
      return(.sapPopulationPointSummary(model, type, at))
    arguments <- list(object = model, X = attr(model, "predictionX"), type = type, ci = FALSE,
      fn = if (type == "mean") .sapNativePredictionFunction(model, type))
    if (type == "quantile")
      arguments[["quantiles"]] <- at
    do.call(summary, arguments)
  }
  point    <- evaluate(currentFit)
  baseline <- evaluate(referenceFit)
  if (type %in% c("mean", "median") && any(!is.finite(unlist(lapply(c(point, baseline), `[[`, "est")))))
    stop(gettext("Relative lifetime requires a finite summary for both the reference and current predictor values."))
  bounds <- NULL
  if (options[["predictionsDisplayConfidenceIntervals"]] && .sapUsableCovariance(fit)) {
    B      <- options[["confidenceIntervalSimulationDraws"]]
    seed   <- if (options[["setSeed"]]) options[["seed"]]
    draws  <- .sapJointDraws(fit, B, seed, options[["simulationContext"]])
    bounds <- .sapPredictionBounds(currentFit, point, type, NULL, 0, B, draws, identity, options[["simulationContext"]],
      options[["predictionsDisplayConfidenceIntervalsLevel"]], options[["confidenceIntervalBounds"]],
      reference = list(fit = referenceFit, point = baseline))
  }
  for (i in seq_along(point)) {
    point[[i]][["est"]] <- baseline[[i]][["est"]] / point[[i]][["est"]]
    indices <- (i - 1L) * length(at) + seq_along(at)
    point[[i]][["lcl"]] <- if (is.null(bounds)) NA_real_ else bounds["lower", indices]
    point[[i]][["ucl"]] <- if (is.null(bounds)) NA_real_ else bounds["upper", indices]
    if (options[["predictionsDisplayConfidenceIntervals"]] && fit[["npars"]] == 0L) {
      for (field in .sapBoundFields(options[["confidenceIntervalBounds"]]))
        point[[i]][[field]] <- point[[i]][["est"]]
    }
  }
  attr(point, "predictionWarnings") <- c(attr(bounds, "predictionWarnings"),
    if (options[["predictionsUnassignedReferenceValues"]] == "population")
      gettext("The reference lifetime averages over the fitting population, holding line and panel predictors fixed."),
    if (type == "quantile")
      gettext("Relative life is reference/current lifetime. Ratios may depend on the percentile.") else
      if (type == "mean") gettext("Relative mean survival time is the reference mean divided by the current mean.") else
        gettext("Relative median survival time is the reference median divided by the current median."))
  return(point)
}

.sapPredictionContourSettings <- function(options) {

  options[["predictionsXAxisVariables"]] <- unlist(options[["predictionsContourXAxis"]], use.names = FALSE)
  options[["predictionsSeparateLines"]] <- unlist(options[["predictionsContourSeparateLines"]], use.names = FALSE)
  options[["predictionsSeparatePlots"]] <- unlist(options[["predictionsContourSeparatePlots"]], use.names = FALSE)
  return(options)
}

.sapPredictionContourScenarios <- function(fit, settings, axes, number, plot = FALSE) {

  settings[["predictionsPredictorValuesInTable"]] <- number
  scenarios <- .sapModelPredictionScenarios(fit, settings, timeRole = "panels")
  training  <- attr(fit, "dataset")
  x         <- if (plot) .sapPredictionAxisPlotGrid(fit, settings, axes[1L], number) else
    seq(min(training[[axes[1L]]]), max(training[[axes[1L]]]), length.out = number)
  scenarios[["data"]][[axes[1L]]] <- x[match(scenarios[["data"]][[axes[1L]]], unique(scenarios[["data"]][[axes[1L]]]))]
  y <- if (plot) .sapPredictionAxisPlotGrid(fit, settings, axes[2L], number) else
    seq(min(training[[axes[2L]]]), max(training[[axes[2L]]]), length.out = number)
  index <- rep(seq_len(nrow(scenarios[["data"]])), each = length(y))
  scenarios[["data"]] <- scenarios[["data"]][index, , drop = FALSE]
  scenarios[["data"]][[axes[2L]]] <- rep(y, times = length(index) / length(y))
  scenarios[["x"]] <- scenarios[["data"]][[axes[1L]]]
  scenarios[["y"]] <- scenarios[["data"]][[axes[2L]]]
  scenarios[["lines"]] <- scenarios[["lines"]][index]
  scenarios[["panels"]] <- scenarios[["panels"]][index]
  return(scenarios)
}

.sapPredictionContourReady <- function(options) {

  axes <- .sapModelPredictionAxes(options)
  return(length(axes) == 2L && length(unique(axes)) == 2L && all(axes %in% options[["covariates"]]))
}

.sapPredictionContourBlocks <- function(fit, scenario, settings, type, at, measure, summarize = identity) {

  # Bound memory use while evaluating each surface, retaining scenario order.
  blocks <- lapply(seq.int(1L, nrow(scenario[["data"]]), by = 128L), function(first) {
    rows <- seq.int(first, min(nrow(scenario[["data"]]), first + 127L))
    part <- scenario
    for (field in c("data", "x", "y", "lines", "panels", "observedRows"))
      part[[field]] <- if (field == "data") scenario[[field]][rows, , drop = FALSE] else scenario[[field]][rows]
    value <- .sapModelPredictionValues(fit, part, settings, type, at, measure, fast = TRUE)
    progressbarTick()
    summarize(value)
  })
  return(blocks)
}

# Evaluate the two-predictor grid, then build surfaces, contours and overlays.
.sapModelPredictionContour <- function(fit, options, type, measure, commonLevels = NULL) {

  result <- createJaspPlot(width = 520, height = 450)
  if (jaspBase::isTryError(commonLevels)) {
    result$setError(conditionMessage(attr(commonLevels, "condition")))
    return(result)
  }
  axes <- .sapModelPredictionAxes(options)
  if (!.sapPredictionContourReady(options))
    return(result)

  fit <- fit[!vapply(fit, jaspBase::isTryError, logical(1))]
  if (!.saSurvivalReady(options) || !length(fit))
    return(result)

  plotAxes <- try(.sapPredictionContourPlotAxes(fit, options, axes), silent = TRUE)
  if (jaspBase::isTryError(plotAxes)) {
    result$setError(conditionMessage(attr(plotAxes, "condition")))
    return(result)
  }

  # Use one axis mapping for grid evaluation, observed data and contour labels.
  axisScales <- plotAxes[["mappings"]]
  settings   <- plotAxes[["settings"]]
  settings[["predictionsDisplayConfidenceIntervals"]] <- FALSE
  scenarios <- try(lapply(fit, function(model) .sapPredictionContourScenarios(model, settings, axes, 65L, plot = TRUE)), silent = TRUE)
  if (jaspBase::isTryError(scenarios)) {
    result$setError(conditionMessage(attr(scenarios, "condition")))
    return(result)
  }

  # Evaluate each model surface in bounded blocks, preserving panel and line identities.
  fitLabels <- .sapPredictionFitLabels(fit, options)
  ticks     <- sum(vapply(scenarios, function(scenario) ceiling(nrow(scenario[["data"]]) / 128L), numeric(1)))
  startProgressbar(expectedTicks = ticks, label = gettext("Computing contour predictions"))
  records <- try(lapply(seq_along(fit), function(i) {
    scenario <- scenarios[[i]]
    at       <- .sapModelPredictionAt(.sapPlotTimeFit(fit), settings, type, "panels")
    value    <- .saSafeRbind(.sapPredictionContourBlocks(fit[[i]], scenario, settings, type, at, measure))
    if (length(fit) > 1L)
      value[["Panel"]] <- vapply(value[["Panel"]], function(panel)
        .sapPredictionSeriesLabel(fitLabels[i], panel), character(1))
    value[["Surface"]] <- interaction(value[["Line"]], value[["Panel"]], drop = TRUE)
    value
  }), silent = TRUE)
  if (jaspBase::isTryError(records)) {
    result$setError(conditionMessage(attr(records, "condition")))
    return(result)
  }
  data <- .saSafeRbind(records)
  if (type %in% c("mean", "median") && any(!is.finite(data[["estimate"]]))) {
    result$setError(gettext("This lifetime summary is not finite or could not be evaluated for the selected model and predictor values."))
    return(result)
  }
  finite <- data[["estimate"]][is.finite(data[["estimate"]])]
  if (!length(finite)) {
    result$setError(gettext("No finite predictions are available for this plot."))
    return(result)
  }
  data[["estimate"]][!is.finite(data[["estimate"]])] <- NA_real_
  levels <- try(.sapPredictionContourLevels(data, options, measure, commonLevels), silent = TRUE)
  if (jaspBase::isTryError(levels)) {
    result$setError(conditionMessage(attr(levels, "condition")))
    return(result)
  }

  # Use a heatmap for a single surface or contour colours for separate lines.
  heatmap <- NULL
  if (!length(options[["predictionsContourSeparateLines"]])) {
    heatmap <- try(.sapPredictionHeatmapScale(options, measure, finite), silent = TRUE)
    if (jaspBase::isTryError(heatmap)) {
      result$setError(conditionMessage(attr(heatmap, "condition")))
      return(result)
    }
    data[["HeatmapValue"]] <- pmin(pmax(data[["estimate"]], heatmap[["bounds"]][1L]), heatmap[["bounds"]][2L])
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::facet_wrap(~Panel, ncol = 2, labeller = ggplot2::label_value) +
    ggplot2::labs(x = jaspBase::decodeColNames(axes[1L]), y = jaspBase::decodeColNames(axes[2L]))
  for (scale in plotAxes[["scales"]])
    plot <- plot + scale$clone()
  if (length(options[["predictionsContourSeparateLines"]])) {
    colorScale <- jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]])
    colorScale$limits <- as.character(sort(unique(unlist(levels, use.names = FALSE))))
    colorScale$palette <- .sapPlotPalette(colorScale$palette, length(colorScale$limits))
    plot <- plot + colorScale +
      ggplot2::labs(colour = .sapPredictionTitle(type, options, measure), linetype = gettext("Separate lines")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(colour = "grey40")))
  } else {
    plot <- plot + ggplot2::geom_raster(ggplot2::aes(fill = HeatmapValue), interpolate = TRUE) +
      heatmap[["scale"]] +
      ggplot2::labs(fill = .sapPredictionTitle(type, options, measure))
  }

  # Draw only contour levels that fall inside each panel's finite prediction range.
  lineLayers <- 0L
  # Mean and median have an unnamed panel; empty names cannot index an R list.
  for (i in seq_along(levels)) {
    panel   <- names(levels)[i]
    surface <- data[as.character(data[["Panel"]]) == panel, , drop = FALSE]
    values  <- surface[["estimate"]][is.finite(surface[["estimate"]])]
    if (!length(values) || diff(range(values)) == 0)
      next
    selected <- levels[[i]][levels[[i]] > min(values) & levels[[i]] < max(values)]
    if (!length(selected))
      next
    if (length(options[["predictionsContourSeparateLines"]]))
      plot <- plot + ggplot2::geom_contour(data = surface, ggplot2::aes(z = estimate, group = Surface,
        colour = ggplot2::after_stat(factor(level)), linetype = Line), breaks = selected, na.rm = TRUE)
    else
      plot <- plot + ggplot2::geom_contour(
        data = surface,
        ggplot2::aes(z = estimate),
        colour = "grey30",
        alpha  = .6,
        breaks = selected,
        na.rm  = TRUE
      )
    lineLayers <- lineLayers + 1L
  }
  if (!lineLayers && length(options[["predictionsContourSeparateLines"]]))
    return(result)
  # Fixed scales clip raster overhang without enlarging the prediction range again.
  plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
  if (options[["predictionsDisplayObservedData"]])
    plot <- .sapAddContourObservedLayer(plot, .sapPredictionContourObserved(fit, settings, axes, records, type))
  if (lineLayers && !length(options[["predictionsContourSeparateLines"]]))
    plot <- .sapPredictionContourLabels(plot, axisScales)
  result$plotObject <- .sapPredictionPlotAddTheme(plot, options) +
    ggplot2::theme(strip.clip = "off", aspect.ratio = 1, legend.location = "plot")
  if (options[["plotLegend"]] %in% c("top", "bottom"))
    result$plotObject <- result$plotObject + ggplot2::theme(legend.title.position = "top", legend.box = "vertical")
  if (!length(options[["predictionsContourSeparateLines"]])) {
    barWidth <- .sapPredictionContourColourbarWidth(result$plotObject, options)
    guide    <- if (is.null(barWidth)) ggplot2::guide_colourbar() else ggplot2::guide_colourbar(
      direction = "horizontal", theme = ggplot2::theme(legend.title.position = "top",
        legend.key.width = grid::unit(barWidth, "pt"), legend.key.height = grid::unit(10, "pt")))
    result$plotObject <- result$plotObject + ggplot2::guides(fill = guide)
  } else if (options[["plotLegend"]] %in% c("top", "bottom"))
    result$plotObject <- result$plotObject + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2),
      linetype = ggplot2::guide_legend(ncol = 2))
  size <- .sapPredictionContourPlotSize(result$plotObject, options, length(unique(data[["Panel"]])))
  result$width <- size[["width"]]
  result$height <- size[["height"]]
  # Preserve a continuous colorbar through the native decoder's legend handling.
  if (!length(options[["predictionsContourSeparateLines"]])) {
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    result$plotObject <- ggplot2::ggplotGrob(result$plotObject)
  }
  return(result)
}

# Place labels on computed contours using the plot's transformed axis coordinates.
.sapPredictionContourLabels <- function(plot, axisScales = list(NULL, NULL)) {

  built    <- ggplot2::ggplot_build(plot)
  layers   <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomContour"), logical(1)))
  contours <- .saSafeRbind(built$data[layers])
  if (is.null(contours))
    return(plot)
  if (!nrow(contours))
    return(plot)
  for (i in seq_along(axisScales))
    if (!is.null(axisScales[[i]])) {
      field <- if (i == 1L) "x" else "y"
      contours[[field]] <- axisScales[[i]][["transform"]]$inverse(contours[[field]])
    }
  # One label on the longest contour piece at each level in each panel.
  pieces <- split(contours, interaction(contours$PANEL, contours$level, contours$piece, drop = TRUE))
  labels <- .saSafeRbind(lapply(pieces, function(piece) {
    if (nrow(piece) < 5L)
      return(NULL)
    middle <- piece[ceiling(nrow(piece) / 2), , drop = FALSE]
    middle$vertices <- nrow(piece)
    middle
  }))
  if (is.null(labels) || !nrow(labels))
    return(plot)

  labels <- labels[order(labels$vertices, decreasing = TRUE), , drop = FALSE]
  labels <- labels[!duplicated(labels[c("PANEL", "level")]), , drop = FALSE]
  labels$Panel <- built$layout$layout$Panel[match(labels$PANEL, built$layout$layout$PANEL)]
  labels$label <- format(signif(labels$level, 4), trim = TRUE)
  return(plot + ggplot2::geom_text(data = labels, ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE, colour = "grey20", size = 3, check_overlap = TRUE, show.legend = FALSE))
}

.sapPredictionObservedLifetimes <- function(fit, options) {

  records <- lapply(.sapPlotSubgroupFits(fit), function(model) {
    training  <- attr(model, "dataset")
    scenarios <- .sapModelPredictionScenarios(model, options)
    roles     <- data.frame(Line = scenarios[["lines"]], Panel = scenarios[["panels"]])
    .saSafeRbind(lapply(which(!duplicated(roles)), function(i) {
      rows <- scenarios[["observedRows"]][[i]]
      if (!length(rows))
        return(NULL)
      observed <- training[rows, , drop = FALSE]
      response <- .saGetSurvObject(options, observed)
      values   <- unclass(response)
      type     <- attr(response, "type")
      code     <- values[, ncol(values)]
      lower    <- upper <- values[, 1L]
      status   <- ifelse(code == 1L, gettext("Event"), gettext("Right censored"))
      if (type == "counting")
        lower <- upper <- values[, 2L]
      if (type == "left")
        status[code == 0L] <- gettext("Left censored")
      if (type == "interval") {
        status[code == 2L] <- gettext("Left censored")
        status[code == 3L] <- gettext("Interval censored")
        upper[code == 3L] <- values[code == 3L, 2L]
      }
      data.frame(x = observed[[scenarios[["xVariable"]]]], lower = lower, upper = upper,
        Status = factor(
          status,
          levels = c(gettext("Event"), gettext("Right censored"), gettext("Left censored"), gettext("Interval censored"))
        ),
        ObservedRow   = rows, Panel = scenarios[["panels"]][i], Line = scenarios[["lines"]][i],
        Subgroup      = attr(model, "subgroupLabel"),
        ObservedGroup = .sapPredictionSeriesLabel(gettext("Observed"),
          if (.sapHasSubgroups(options)) attr(model, "subgroupLabel"), scenarios[["lines"]][i]))
    }))
  })
  return(.saSafeRbind(records))
}

# Match each observed cohort to prediction colours and categorical dodge slots.
.sapPredictionObservedLifetimeEncoding <- function(observed, data, categorical) {

  fields  <- c("Panel", "Subgroup", "ConditionLine", "Series", "Colour", if (categorical) "x")
  anchors <- unique(data[fields])
  anchors[["Offset"]] <- 0
  anchors[["Slots"]]  <- 1L
  if (categorical) {
    series <- if (is.factor(data[["Series"]])) levels(data[["Series"]]) else sort(unique(data[["Series"]]))
    cells  <- split(seq_len(nrow(anchors)), interaction(anchors[["Panel"]], anchors[["x"]], drop = TRUE))
    for (rows in cells) {
      groups <- match(as.character(anchors[["Series"]][rows]), series)
      slots  <- sort(unique(groups))
      anchors[["Offset"]][rows] <- .3 * ((match(groups, slots) - .5) / length(slots) - .5)
      anchors[["Slots"]][rows] <- length(slots)
    }
  }

  observed[["ObservedSeries"]] <- NA_character_
  observed[["ObservedOffset"]] <- if (categorical) (observed[["ObservedRow"]] %% 13L - 6L) / 50 else 0
  roles <- c("Panel", "Subgroup", "Line", if (categorical) "x")
  for (i in which(!duplicated(observed[roles]))) {
    cohort  <- rep(TRUE, nrow(observed))
    matched <- rep(TRUE, nrow(anchors))
    for (field in roles) {
      cohort <- cohort & as.character(observed[[field]]) == as.character(observed[[field]][i])
      target <- if (field == "Line") "ConditionLine" else field
      matched <- matched & as.character(anchors[[target]]) == as.character(observed[[field]][i])
    }
    colours <- unique(as.character(anchors[["Colour"]][matched]))
    if (length(colours) == 1L)
      observed[["ObservedSeries"]][cohort] <- colours
    if (categorical && any(matched)) {
      offsets <- unique(anchors[["Offset"]][matched])
      jitter  <- (observed[["ObservedRow"]][cohort] %% 13L - 6L) / 50
      observed[["ObservedOffset"]][cohort] <- mean(offsets) + jitter * length(offsets) / anchors[["Slots"]][which(matched)[1L]]
    }
  }
  return(observed)
}

.sapAddObservedLifetimeLayer <- function(plot, fit, options, data, type, timeRole, categorical) {

  observed <- .sapPredictionObservedLifetimes(fit, options)
  if (is.null(observed))
    return(plot)

  observed <- observed[observed[["Status"]] == gettext("Event"), , drop = FALSE]
  if (!nrow(observed))
    return(plot)
  if (!categorical) {
    bounds   <- range(unlist(lapply(fit, function(model) .sapPredictionAxisRange(model, options, options[["predictionsXAxisVariables"]]))))
    observed <- observed[observed[["x"]] >= bounds[1L] & observed[["x"]] <= bounds[2L], , drop = FALSE]
  }
  if (!nrow(observed))
    return(plot)
  if (type == "quantile" && timeRole == "panels")
    observed <- .sapPredictionObservedPanels(observed, data, type)
  observed[["Panel"]] <- factor(observed[["Panel"]], levels = levels(data[["Panel"]]))
  observed[["ObservedX"]] <- observed[["x"]]
  observed[["estimate"]] <- observed[["upper"]]
  observed <- .sapPredictionObservedLifetimeEncoding(observed, data, categorical)
  matched  <- !is.na(observed[["ObservedSeries"]])
  if (any(matched))
    plot <- plot + ggplot2::geom_point(data = observed[matched, , drop = FALSE],
      ggplot2::aes(x = ObservedX, y = estimate, colour = ObservedSeries), inherit.aes = FALSE, alpha = .45, size = 1.3,
      position = ggplot2::position_nudge(x = observed[["ObservedOffset"]][matched]), show.legend = FALSE)
  observed <- observed[!matched, , drop = FALSE]
  if (!nrow(observed))
    return(plot)
  return(plot + ggplot2::geom_point(data = observed, ggplot2::aes(x = ObservedX, y = estimate),
    inherit.aes = FALSE, colour = "grey40", alpha = .45, size = 1.3,
    position    = ggplot2::position_nudge(x = observed[["ObservedOffset"]]), show.legend = FALSE))
}
