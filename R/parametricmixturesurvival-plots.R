#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Shared density diagnostics, mixture component plots, and observed overlays.
.sapmComponentPlotFun           <- function(fit, options, prefix = "mixtureComponentPlot", type = options[["mixtureComponentPlotType"]]) {

  return(.sapDistributionPlotFun(fit, options, prefix, type))
}
.sapmComponentPlotForData       <- function(fit, options, dataset = attr(fit, "dataset"), prefix = "mixtureComponentPlot", type = options[["mixtureComponentPlotType"]], plotData = NULL) {

  estimateTitle <- switch(
    type,
    "survival"           = gettext("Survival Probability"),
    "failureProbability" = gettext("Failure Probability"),
    "density"            = gettext("Density"),
    "hazard"             = gettext("Hazard"),
    "cumulativeHazard"   = gettext("Cumulative Hazard"),
    "restrictedMeanSurvivalTime" = gettext("Restricted Mean Survival Time")
  )

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(createJaspPlot(title = estimateTitle))

  if (is.null(plotData))
    plotData <- try(.sapmComponentPlotData(fit, options, dataset, prefix, type))

  if (jaspBase::isTryError(plotData)) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempPlot)
  }

  predictionWarnings <- attr(plotData, "predictionWarnings")
  if (!any(is.finite(plotData[["at"]]) & is.finite(plotData[["estimate"]]))) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(paste(unique(c(gettext("No finite predictions are available for this plot."), predictionWarnings)), collapse = "\n"))
    return(tempPlot)
  }

  hasLevel <- length(unique(plotData[["Level"]])) > 1
  hasDistribution <- length(unique(plotData[["Distribution"]])) > 1
  options[["predictionsDisplayConfidenceIntervals"]] <- options[["predictionsDisplayConfidenceIntervals"]] &&
    any(is.finite(unlist(plotData[.sapBoundFields(options[["confidenceIntervalBounds"]], "lCi", "uCi")])))

  hasComponents <- nlevels(plotData[["Component"]]) > 1L
  plotData[[".fullCurve"]] <- plotData[["Component"]] == levels(plotData[["Component"]])[1]
  plotData[[".curveGroup"]] <- interaction(factor(plotData[["Distribution"]], exclude = NULL),
    plotData[["Component"]], factor(plotData[["Level"]], exclude = NULL), drop = TRUE)
  colorField <- if (hasDistribution && hasComponents) "Series" else if (hasDistribution) "Distribution" else if (hasComponents) "Component"
  if (identical(colorField, "Series"))
    plotData[["Series"]] <- factor(paste(plotData[["Distribution"]], plotData[["Component"]], sep = ": "),
      levels = unique(paste(plotData[["Distribution"]], plotData[["Component"]], sep = ": ")))
  colors <- if (is.null(colorField)) "grey40" else jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(nlevels(plotData[[colorField]]))
  if (identical(colorField, "Component"))
    colors <- c("black", jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(nlevels(plotData[["Component"]]) - 1L))
  if (!is.null(colorField))
    names(colors) <- levels(plotData[[colorField]])
  legendTitle <- if (identical(colorField, "Series")) gettext("Distribution / Component") else if (hasDistribution) gettext("Distribution") else gettext("Component")
  legendTypes <- if (is.null(colorField)) "solid" else vapply(names(colors), function(label)
    if (any(plotData[[".fullCurve"]] & as.character(plotData[[colorField]]) == label)) "solid" else "dashed", character(1))
  fullColors <- if (hasDistribution) stats::setNames(colors[as.character(plotData[plotData[[".fullCurve"]], colorField])],
    as.character(plotData[plotData[[".fullCurve"]], "Distribution"]))
  fullColors <- fullColors[!duplicated(names(fullColors))]

  logTime <- options[[paste0(prefix, "XAxisTransformation")]] == "log"
  logYAxis <- prefix == "mixtureComponentPlot" && options[["mixtureComponentPlotYAxisTransformation"]] == "log"
  if (logYAxis)
    plotData[[".displayEstimate"]] <- ifelse(plotData[["estimate"]] > 0, plotData[["estimate"]], NA_real_)
  plot <- ggplot2::ggplot(data = plotData)

  observedDensity <- NULL
  if (prefix == "densityPlot" && options[["densityPlotObservedData"]] && options[["censoringType"]] == "right")
    observedDensity <- .sapmObservedDensity(dataset, options, prefix)

  xValues <- c(plotData[["at"]], observedDensity[["lower"]], observedDensity[["upper"]])
  xRange  <- range(xValues[is.finite(xValues) & (!logTime | xValues > 0)])
  if (logTime) {
    canvas  <- .sapProbabilityPlotCanvasTransform("lognormal")
    xRange  <- .sapProbabilityPlotTimeRange(xValues)
    xBreaks <- .sapProbabilityPlotTimeBreaks(xRange, canvas)
    xScale  <- jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = xRange,
      minor_breaks = .sapProbabilityPlotTimeMinorBreaks(xRange, canvas), labels = .sapProbabilityPlotTimeLabel,
      trans = "log", transform = "log", oob = scales::oob_keep)
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(xRange)
    xScale  <- jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep)
  }

  if (!is.null(observedDensity)) {
    # Clip the zero edge of the histogram to the visible positive-time range.
    if (logTime)
      observedDensity[["lower"]] <- pmax(observedDensity[["lower"]], xRange[1])
    plot <- plot + ggplot2::geom_rect(
      data = observedDensity,
      mapping = ggplot2::aes(xmin = lower, xmax = upper, ymin = 0, ymax = density),
      inherit.aes = FALSE, fill = "grey80", color = "white", alpha = 0.6
    )
  }

  if (options[["predictionsDisplayConfidenceIntervals"]]) {
    bandData <- plotData[plotData[["Component"]] == levels(plotData[["Component"]])[1], , drop = FALSE]
    plot <- .sapAddModelBoundLayer(plot, bandData, options[["predictionsDisplayConfidenceIntervalsLevel"]], options[["confidenceIntervalBounds"]],
      groups = c(if (hasDistribution) "Distribution", "Level"),
      color = if (hasDistribution) colorField, fill = if (hasDistribution) "Distribution", defaultFill = "grey60", na.rm = TRUE,
      twoSidedMapping = list(x = as.name("at"), ymin = as.name("lCi"), ymax = as.name("uCi"),
        fill = if (hasDistribution) as.name("Distribution"), group = as.name(".curveGroup")))
  }

  aesCall <- list(
    x        = as.name("at"),
    y        = as.name(if (logYAxis) ".displayEstimate" else "estimate"),
    color    = if (!is.null(colorField)) as.name(colorField),
    group    = as.name(".curveGroup"),
    linetype = if (hasLevel) as.name("Level")
  )
  geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!vapply(aesCall, is.null, logical(1))]), na.rm = TRUE)
  if (is.null(colorField))
    geomCall[["color"]] <- "grey40"
  if (hasComponents) {
    # Draw the dashed components first so the full mixture stays visible.
    componentData <- plotData[plotData[["Component"]] != levels(plotData[["Component"]])[1], , drop = FALSE]
    componentCall <- geomCall
    componentCall[["data"]] <- componentData
    componentCall[["mapping"]][["group"]] <- ggplot2::aes(group = .curveGroup)[["group"]]
    componentCall[["linetype"]] <- "dashed"
    plot <- plot + do.call(jaspGraphs::geom_line, componentCall)
    geomCall[["data"]] <- plotData[plotData[["Component"]] == levels(plotData[["Component"]])[1], , drop = FALSE]
  }
  plot <- plot + do.call(jaspGraphs::geom_line, geomCall)
  if (!is.null(colorField))
    plot <- plot + ggplot2::scale_color_manual(values = colors, limits = names(colors), name = legendTitle,
      guide = ggplot2::guide_legend(override.aes = list(linetype = unname(legendTypes))))
  if (hasDistribution)
    plot <- plot + ggplot2::scale_fill_manual(values = fullColors, name = gettext("Distribution"), guide = "none")

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(.saPlotEstimateRange(
    estimate = c(plotData[["estimate"]], observedDensity[["density"]], if (!is.null(observedDensity)) 0),
    lCi      = if (options[["predictionsDisplayConfidenceIntervals"]]) plotData[["lCi"]],
    uCi      = if (options[["predictionsDisplayConfidenceIntervals"]]) plotData[["uCi"]],
    bounded  = type %in% c("survival", "failureProbability"),
    at       = if (logTime) log(plotData[["at"]]) else plotData[["at"]],
    group    = plotData[[".curveGroup"]]))

  yScale <- jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep)
  if (logYAxis) {
    positive <- plotData[["estimate"]][is.finite(plotData[["estimate"]]) & plotData[["estimate"]] > 0]
    if (length(positive) == 0L) {
      tempPlot <- createJaspPlot(title = estimateTitle)
      tempPlot$setError(gettext("No positive predictions are available for a logarithmic y-axis."))
      return(tempPlot)
    }
    breaks <- 10^jaspGraphs::getPrettyAxisBreaks(range(log10(positive)))
    breaks <- breaks[is.finite(breaks) & breaks > 0]
    yScale <- jaspGraphs::scale_y_continuous(breaks = breaks, limits = range(c(positive, breaks)),
      labels = scales::label_scientific(digits = 2),
      trans = "log10", transform = "log10", oob = scales::oob_keep)
  }

  axisTitle <- if (type == "restrictedMeanSurvivalTime") gettext("RMST") else estimateTitle
  plot <- plot + xScale + yScale +
    ggplot2::ylab(if (logYAxis) gettextf("%1$s (log scale)", axisTitle) else axisTitle) +
    ggplot2::xlab(if (logTime) gettextf("%1$s (log scale)", gettext("Time")) else gettext("Time"))

  # the detailed theme is available only for the survival probability plots
  if (options[["plotTheme"]] == "detailed")
    options[["plotTheme"]] <- "jasp"
  plot <- .sapPredictionPlotAddTheme(plot, options)
  horizontalLegend <- options[["plotLegend"]] %in% c("bottom", "top")
  if (horizontalLegend)
    plot <- plot + ggplot2::guides(color = ggplot2::guide_legend(ncol = 2, byrow = TRUE, title.position = "top",
      override.aes = list(linetype = unname(legendTypes))))

  seriesCount <- length(colors)
  height <- 320 + if (horizontalLegend) 30 * (ceiling(seriesCount / 2) - 1) else 0
  tempPlot <- createJaspPlot(width = 550, height = height)
  tempPlot$plotObject <- plot

  return(tempPlot)
}
.sapmObservedDensity            <- function(dataset, options, prefix = "mixtureComponentPlot") {

  time <- dataset[[options[["timeToEvent"]]]]
  logTime <- options[[paste0(prefix, "XAxisTransformation")]] == "log"
  if (logTime) {
    time <- log(time[time > 0])
    if (length(time) == 0)
      return(NULL)
  }
  binWidthType <- options[[paste0(prefix, "ObservedDataBinWidthType")]]
  if (binWidthType == "manual") {
    binWidthType <- options[[paste0(prefix, "ObservedDataNumberOfBins")]]
  } else if (binWidthType == "doane") {
    n <- length(time)
    centered <- time - mean(time)
    variance <- mean(centered^2)
    if (n > 2 && variance > 0) {
      skewness <- mean(centered^3) / variance^1.5
      skewnessSe <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
      binWidthType <- 1 + log2(n) + log2(1 + abs(skewness) / skewnessSe)
    } else {
      binWidthType <- "sturges"
    }
  } else if (binWidthType == "fd" && grDevices::nclass.FD(time) > 10000) {
    binWidthType <- 10000
  }
  histogram <- graphics::hist(time, breaks = binWidthType, plot = FALSE)
  breaks <- if (logTime) histogram[["breaks"]] else pmax(0, histogram[["breaks"]])
  breaks <- unique(breaks)

  # Survival drops supply probability masses. Do not normalize an unidentified tail away.
  outcome <- .saGetSurvObject(options, dataset)
  km <- survival::survfit(outcome ~ 1, weights = .sapCaseWeights(dataset, options))
  mass <- -diff(c(1, km[["surv"]]))
  kmTime <- if (logTime) log(km[["time"]]) else km[["time"]]
  bin <- as.integer(cut(kmTime, breaks = breaks, include.lowest = TRUE))
  probability <- vapply(seq_len(length(breaks) - 1), function(i) sum(mass[!is.na(bin) & bin == i]), numeric(1))

  return(data.frame(
    lower   = if (logTime) exp(head(breaks, -1)) else head(breaks, -1),
    upper   = if (logTime) exp(tail(breaks, -1)) else tail(breaks, -1),
    density = probability / diff(breaks)
  ))
}
.sapmComponentPlotData          <- function(fit, options, dataset = attr(fit, "dataset"), prefix = "mixtureComponentPlot", type = options[["mixtureComponentPlotType"]], times = NULL) {

  mixture    <- attr(fit, "mixture")
  family     <- if (!is.null(mixture)) .sapmFamily(mixture[["family"]])
  components <- attr(fit, "components")
  componentIndices <- if (prefix == "mixtureComponentPlot" && components > 1) seq_len(components) else integer(0)
  logDensity <- type == "density" && options[[paste0(prefix, "XAxisTransformation")]] == "log"
  marginal <- prefix == "densityPlot"
  predictionData <- NULL
  if (marginal) {
    predictors <- .sapModelVariables(attr(fit, "modelTerms"))
    groups <- .sapPredictorGroups(dataset[predictors])
    predictionData <- dataset[!duplicated(groups), predictors, drop = FALSE]
    # Combine identical predictor rows, retaining their observed proportions.
    rowIndex <- match(groups, groups[!duplicated(groups)])
    weights <- .sapCaseWeights(dataset, options)
    predictionWeights <- vapply(seq_len(nrow(predictionData)), function(i) sum(weights[rowIndex == i]), numeric(1))
    predictionWeights <- predictionWeights / sum(predictionWeights)
  }
  # the components might change rapidly, the time steps are not rounded for a smooth display
  options[["predictionsRoundTimeTableValues"]] <- FALSE
  if (is.null(times))
    times <- .sapOptions2PredictionTime(options, fit, type = if (prefix == "densityPlot") "density" else "mixtureComponents", plot = TRUE)
  if (components > 1)
    times <- .sapmComponentPlotTimes(fit, family, components, times, newdata = predictionData)
  ci         <- options[["predictionsDisplayConfidenceIntervals"]]
  level      <- options[["predictionsDisplayConfidenceIntervalsLevel"]]

  # the functions are evaluated with the covariate dependent parameters supplied by flexsurv
  # Average inside the prediction function so native CI draws average the
  # densities jointly, rather than averaging confidence interval endpoints.
  marginalDensity <- function(density) {
    force(density)
    fn <- function(t, start, ...) {
      values <- density(t, start, ...)
      values <- matrix(values, ncol = nrow(predictionData))
      return(rep(as.vector(values %*% predictionWeights), nrow(predictionData)))
    }
    attr(fn, "sharedScenarioValue") <- TRUE
    return(fn)
  }
  mixtureDensity    <- function(t, start, ...) fit[["dfns"]][["d"]](t, ...)
  if (marginal)
    mixtureDensity <- marginalDensity(mixtureDensity)
  componentFunction <- function(k) {
    function(t, start, ...) {
      return(.sapmComponentContribution(t, list(...), k, family, fit, type))
    }
  }
  if (marginal) {
    conditionalComponent <- componentFunction
    componentFunction <- function(k) marginalDensity(conditionalComponent(k))
  }
  predict <- function(...) {
    predictions <- .sapSummaryPredictions(fit, ..., newdata = predictionData, B = options[["confidenceIntervalSimulationDraws"]], seed = if (options[["setSeed"]]) options[["seed"]],
      direction = options[["confidenceIntervalBounds"]], context = options[["simulationContext"]])
    warnings <- attr(predictions, "predictionWarnings")
    if (marginal) {
      predictions <- predictions[1]
      attr(predictions, "predictionWarnings") <- warnings
    }
    return(predictions)
  }

  predictMixture <- function(times, ...) {
    if (type == "density")
      return(predict(fn = mixtureDensity, t = times, ...))
    return(predict(type = switch(type, failureProbability = "survival", cumulativeHazard = "cumhaz", restrictedMeanSurvivalTime = "rmst", type), t = times, ...,
      transform = if (type == "failureProbability") function(value) 1 - value else identity))
  }

  evaluate <- function(times) {
    summary <- predictMixture(times, ci = FALSE)
    values <- .sapPlotPredictionMatrix(summary)
    values <- do.call(cbind, c(list(values), lapply(componentIndices, function(k)
      .sapPlotPredictionMatrix(predict(fn = componentFunction(k), t = times, ci = FALSE)))))
    return(if (logDensity) values * times else values)
  }
  anchors <- try(.sapPlotFeatureTimes(list(fit), times), silent = TRUE)
  logTime <- options[[paste0(prefix, "XAxisTransformation")]] == "log"
  times   <- .sapAdaptivePlotTimes(times, evaluate, minimum = if (ci) 65L else 17L, maximum = 401L,
    xTransform = if (logTime) log else identity, xInverse = if (logTime) exp else identity,
    anchors = if (inherits(anchors, "try-error")) numeric(0) else anchors)

  mixtureSummary <- predictMixture(times, ci = ci, cl = level)
  componentSummaries <- lapply(componentIndices, function(k) predict(fn = componentFunction(k), t = times, ci = FALSE))
  predictionWarnings <- unique(c(attr(mixtureSummary, "predictionWarnings"), unlist(lapply(componentSummaries, attr, "predictionWarnings"))))

  componentLabels <- if (prefix == "densityPlot") gettext("Fitted density") else
    if (components == 1) gettext("Component 1") else c(gettext("Mixture"), gettextf("Component %1$i", componentIndices))
  out <- list()
  for (j in seq_along(mixtureSummary)) {

    levelLabel <- if (length(mixtureSummary) > 1) decodeColNames(names(mixtureSummary)[j]) else NA

    mixtureData <- data.frame(
      at        = mixtureSummary[[j]][[1]],
      estimate  = mixtureSummary[[j]][["est"]],
      lCi       = if (ci) mixtureSummary[[j]][["lcl"]] else NA,
      uCi       = if (ci) mixtureSummary[[j]][["ucl"]] else NA,
      Component = componentLabels[1],
      Level     = levelLabel
    )

    out[[length(out) + 1]] <- mixtureData
    for (k in componentIndices) {
      out[[length(out) + 1]] <- data.frame(
        at        = componentSummaries[[k]][[j]][["time"]],
        estimate  = componentSummaries[[k]][[j]][["est"]],
        lCi       = NA,
        uCi       = NA,
        Component = componentLabels[k + 1],
        Level     = levelLabel
      )
    }
  }

  out <- do.call(rbind, out)
  out[["Component"]] <- factor(out[["Component"]], levels = componentLabels)

  # The density of log time is t * f(t), including its confidence limits.
  if (logDensity)
    for (name in c("estimate", "lCi", "uCi"))
      out[[name]] <- out[["at"]] * out[[name]]

  # set any Inf to NA
  out[["estimate"]][is.infinite(out[["estimate"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])]           <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])]           <- NA
  attr(out, "predictionWarnings") <- predictionWarnings

  return(out)
}
.sapmComponentContribution <- function(t, arguments, k, family, fit, type) {

  n <- max(length(t), lengths(arguments))
  t <- rep_len(t, n)
  parameters <- lapply(stats::setNames(arguments[paste0(family[["pars"]], k)], family[["pars"]]), rep_len, length.out = n)
  components <- attr(fit, "components")
  probability <- .sapmStickBreaking(do.call(cbind, lapply(arguments[paste0("v", seq_len(components - 1L))], rep_len, length.out = n)), n)[, k]

  if (type == "cumulativeHazard")
    return(.sapmComponentCumulativeHazard(t, arguments, k, family, fit))
  if (type == "hazard") {
    logDensity <- do.call(family[["d"]], c(list(t), parameters, list(log = TRUE)))
    logSurvival <- do.call(fit[["dfns"]][["p"]], c(list(t), arguments, list(lower.tail = FALSE, log.p = TRUE)))
    out <- numeric(n)
    positive <- probability > 0
    out[positive] <- exp(log(probability[positive]) + logDensity[positive] - logSurvival[positive])
    return(out)
  }

  value <- switch(type,
    survival = do.call(family[["p"]], c(list(t), parameters, list(lower.tail = FALSE))),
    failureProbability = do.call(family[["p"]], c(list(t), parameters)),
    density = do.call(family[["d"]], c(list(t), parameters)),
    restrictedMeanSurvivalTime = do.call(family[["rmst"]], c(list(t), parameters)))
  out <- probability * value
  out[probability == 0] <- 0
  return(out)
}
.sapmComponentCumulativeHazard <- function(t, arguments, k, family, fit) {

  n <- length(t)
  numericArguments <- lapply(arguments[vapply(arguments, is.numeric, logical(1))], rep_len, length.out = n)
  groups <- split(seq_len(n), .sapPredictorGroups(as.data.frame(numericArguments)))
  out <- rep(NA_real_, n)
  for (rows in groups) {
    rows <- rows[is.finite(t[rows]) & t[rows] >= 0]
    if (length(rows) == 0L)
      next
    parameters <- arguments
    for (name in names(numericArguments))
      parameters[[name]] <- numericArguments[[name]][rows[1]]
    times <- sort(unique(c(0, t[rows])))
    increments <- vapply(seq_len(length(times) - 1L), function(i)
      stats::integrate(function(time) .sapmComponentContribution(time, parameters, k, family, fit, "hazard"),
        lower = times[i], upper = times[i + 1L], rel.tol = 1e-7, abs.tol = 1e-9, subdivisions = 200L)[["value"]], numeric(1))
    out[rows] <- c(0, cumsum(increments))[match(t[rows], times)]
  }
  return(out)
}
.sapmComponentPlotTimes         <- function(fit, family, components, times, probabilities = seq(0.001, 0.999, length.out = 101L), newdata = NULL) {

  # Add points within every component at each displayed covariate level: a
  # uniform time grid can miss narrow peaks almost entirely.
  componentTimes <- lapply(seq_len(components), function(k) {
    quantileFunction <- function(t, start, ...) {
      arguments  <- list(...)
      n          <- max(length(t), lengths(arguments))
      parameters <- lapply(stats::setNames(arguments[paste0(family[["pars"]], k)], family[["pars"]]), rep_len, length.out = n)
      return(do.call(family[["q"]], c(list(rep_len(t, n)), parameters)))
    }
    predictions <- .sapSummaryPredictions(fit, fn = quantileFunction, t = probabilities, ci = FALSE, newdata = newdata)
    return(unlist(lapply(predictions, function(x) x[["est"]]), use.names = FALSE))
  })
  componentTimes <- unlist(componentTimes, use.names = FALSE)
  componentTimes <- componentTimes[is.finite(componentTimes) & componentTimes >= min(times) & componentTimes <= max(times)]

  return(sort(unique(c(times, componentTimes))))
}

# mixture messages
