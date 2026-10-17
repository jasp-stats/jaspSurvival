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

# Mixture component plots and observed density overlays.
.sapmComponentPlotFun           <- function(fit, options) {

  fit <- fit[[1]]

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit) || options[["mixtureComponentPlotType"]] != "density")
    return(.sapmComponentPlotForData(fit, options))

  dataset <- attr(fit, "dataset")
  factors <- intersect(names(stats::model.frame(fit)), unlist(options[["factors"]], use.names = FALSE))
  if (options[["mixtureComponentPlotMergePlotsAcrossFactors"]] || length(factors) == 0)
    return(.sapmComponentPlotForData(fit, options, dataset))

  # Split observed combinations only; empty cells never need a plot.
  groups <- split(seq_len(nrow(dataset)), .sapPredictorGroups(dataset[factors]))
  if (length(groups) == 1)
    return(.sapmComponentPlotForData(fit, options, dataset))

  container <- createJaspContainer()
  for (i in seq_along(groups)) {
    rows <- groups[[i]]
    plot <- .sapmComponentPlotForData(fit, options, dataset[rows, , drop = FALSE])
    labels <- vapply(dataset[rows[1], factors, drop = FALSE], as.character, character(1))
    plot$title <- paste(paste0(decodeColNames(factors), "=", labels), collapse = ", ")
    plot$position <- i
    container[[paste0("plot", i)]] <- plot
  }
  return(container)
}
.sapmComponentPlotForData       <- function(fit, options, dataset = attr(fit, "dataset")) {

  estimateTitle <- switch(
    options[["mixtureComponentPlotType"]],
    "survival"           = gettext("Survival Probability"),
    "failureProbability" = gettext("Failure Probability"),
    "density"            = gettext("Density"),
    "hazard"             = gettext("Hazard")
  )

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(createJaspPlot(title = estimateTitle))

  plotData <- try(.sapmComponentPlotData(fit, options, dataset))

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
  options[["predictionsConfidenceInterval"]] <- options[["predictionsConfidenceInterval"]] && !all(is.na(plotData[["lCi"]]))

  # the mixture is displayed in black and the components follow the color palette
  colors <- c("black", if (attr(fit, "components") > 1) jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(attr(fit, "components")))
  names(colors) <- levels(plotData[["Component"]])

  logTime <- options[["mixtureComponentPlotTransformXAxis"]] == "log"
  plot <- ggplot2::ggplot(data = plotData)

  observedDensity <- NULL
  if (options[["mixtureComponentPlotType"]] == "density" && options[["mixtureComponentPlotObservedData"]] && options[["censoringType"]] == "right")
    observedDensity <- .sapmObservedDensity(dataset, options)

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

  if (options[["predictionsConfidenceInterval"]]) {
    aesCall <- list(
      x        = as.name("at"),
      ymin     = as.name("lCi"),
      ymax     = as.name("uCi"),
      group    = if (hasLevel) as.name("Level")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = plotData[plotData[["Component"]] == levels(plotData[["Component"]])[1], ], fill = "grey60", alpha = 0.30, na.rm = TRUE)
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
  }

  aesCall <- list(
    x        = as.name("at"),
    y        = as.name("estimate"),
    color    = as.name("Component"),
    linetype = if (hasLevel) as.name("Level")
  )
  geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]))
  plot <- plot + do.call(jaspGraphs::geom_line, geomCall) +
    ggplot2::scale_color_manual(values = colors, name = gettext("Component"))

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(.saPlotEstimateRange(
    estimate = c(plotData[["estimate"]], observedDensity[["density"]], if (!is.null(observedDensity)) 0),
    lCi      = if (options[["predictionsConfidenceInterval"]]) plotData[["lCi"]],
    uCi      = if (options[["predictionsConfidenceInterval"]]) plotData[["uCi"]],
    bounded  = options[["mixtureComponentPlotType"]] %in% c("survival", "failureProbability"),
    at       = if (logTime) log(plotData[["at"]]) else plotData[["at"]],
    group    = plotData[["Level"]]))

  plot <- plot + xScale +
    jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep) +
    ggplot2::ylab(estimateTitle) + ggplot2::xlab(if (logTime) gettextf("%1$s (log scale)", gettext("Time")) else gettext("Time"))

  # the detailed theme is available only for the survival probability plots
  if (options[["plotTheme"]] == "detailed")
    options[["plotTheme"]] <- "jasp"
  plot <- .sapPredictionPlotAddTheme(plot, options)
  horizontalLegend <- options[["plotLegend"]] %in% c("bottom", "top")
  if (horizontalLegend)
    plot <- plot + ggplot2::guides(color = ggplot2::guide_legend(ncol = 2, byrow = TRUE, title.position = "top"))

  height <- 320 + if (horizontalLegend) 30 * (ceiling(length(colors) / 2) - 1) else 0
  tempPlot <- createJaspPlot(width = 550, height = height)
  tempPlot$plotObject <- plot

  return(tempPlot)
}
.sapmObservedDensity            <- function(dataset, options) {

  time <- dataset[[options[["timeToEvent"]]]]
  logTime <- options[["mixtureComponentPlotTransformXAxis"]] == "log"
  if (logTime) {
    time <- log(time[time > 0])
    if (length(time) == 0)
      return(NULL)
  }
  binWidthType <- options[["mixtureComponentPlotHistogramBinWidthType"]]
  if (binWidthType == "manual") {
    binWidthType <- options[["mixtureComponentPlotHistogramManualNumberOfBins"]]
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
  km <- survival::survfit(outcome ~ 1, weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]])
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
.sapmComponentPlotData          <- function(fit, options, dataset = attr(fit, "dataset")) {

  mixture    <- attr(fit, "mixture")
  family     <- if (!is.null(mixture)) .sapmFamily(mixture[["family"]])
  components <- attr(fit, "components")
  componentIndices <- if (components > 1) seq_len(components) else integer(0)
  type       <- options[["mixtureComponentPlotType"]]
  logDensity <- type == "density" && options[["mixtureComponentPlotTransformXAxis"]] == "log"
  predictionData <- NULL
  if (type == "density") {
    predictors <- intersect(names(stats::model.frame(fit)), unlist(c(options[["factors"]], options[["covariates"]]), use.names = FALSE))
    groups <- .sapPredictorGroups(dataset[predictors])
    predictionData <- dataset[!duplicated(groups), predictors, drop = FALSE]
    # Combine identical predictor rows, retaining their observed proportions.
    rowIndex <- match(groups, groups[!duplicated(groups)])
    weights <- if (options[["weights"]] != "") dataset[[options[["weights"]]]] else rep(1, nrow(dataset))
    predictionWeights <- vapply(seq_len(nrow(predictionData)), function(i) sum(weights[rowIndex == i]), numeric(1))
    predictionWeights <- predictionWeights / sum(predictionWeights)
  }
  # the components might change rapidly, the time steps are not rounded for a smooth display
  options[["predictionsLifeTimeRoundSteps"]] <- FALSE
  times      <- .sapOptions2PredictionTime(options, fit, type = "mixtureComponents", plot = TRUE)
  if (components > 1)
    times <- .sapmComponentPlotTimes(fit, family, components, times, newdata = predictionData)
  ci         <- options[["predictionsConfidenceInterval"]]
  level      <- options[["predictionsConfidenceIntervalLevel"]]

  # the functions are evaluated with the covariate dependent parameters supplied by flexsurv
  # Average inside the prediction function so native CI draws average the
  # densities jointly, rather than averaging confidence interval endpoints.
  marginalDensity <- function(density) {
    force(density)
    function(t, start, ...) {
      values <- density(t, start, ...)
      values <- matrix(values, ncol = nrow(predictionData))
      return(rep(as.vector(values %*% predictionWeights), nrow(predictionData)))
    }
  }
  mixtureDensity    <- function(t, start, ...) fit[["dfns"]][["d"]](t, ...)
  if (type == "density")
    mixtureDensity <- marginalDensity(mixtureDensity)
  componentFunction <- function(k) {
    function(t, start, ...) {
      arguments  <- list(...)
      n          <- max(length(t), lengths(arguments))
      parameters <- lapply(stats::setNames(arguments[paste0(family[["pars"]], k)], family[["pars"]]), rep_len, length.out = n)
      t          <- rep_len(t, n)
      switch(
        type,
        "survival"           = do.call(family[["p"]], c(list(t), parameters, list(lower.tail = FALSE))),
        "failureProbability" = do.call(family[["p"]], c(list(t), parameters)),
        "density"            = .sapmStickBreaking(if (components > 1) do.call(cbind, lapply(arguments[paste0("v", seq_len(components - 1))], rep_len, length.out = n)), n)[, k] * do.call(family[["d"]], c(list(t), parameters)),
        "hazard"             = do.call(family[["h"]], c(list(t), parameters))
      )
    }
  }
  if (type == "density") {
    conditionalComponent <- componentFunction
    componentFunction <- function(k) marginalDensity(conditionalComponent(k))
  }
  predict <- function(...) {
    predictions <- .sapSummaryPredictions(fit, ..., newdata = predictionData, B = options[["confidenceIntervalSimulationDraws"]], seed = if (options[["setSeed"]]) options[["seed"]])
    warnings <- attr(predictions, "predictionWarnings")
    if (type == "density") {
      predictions <- predictions[1]
      attr(predictions, "predictionWarnings") <- warnings
    }
    return(predictions)
  }

  predictMixture <- function(times, ...) {
    if (type == "density")
      return(predict(fn = mixtureDensity, t = times, ...))
    return(predict(type = if (type == "failureProbability") "survival" else type, t = times, ...))
  }

  evaluate <- function(times) {
    summary <- predictMixture(times, ci = FALSE)
    values <- .sapPlotPredictionMatrix(summary)
    if (type == "failureProbability") values <- 1 - values
    values <- do.call(cbind, c(list(values), lapply(componentIndices, function(k)
      .sapPlotPredictionMatrix(predict(fn = componentFunction(k), t = times, ci = FALSE)))))
    return(if (logDensity) values * times else values)
  }
  anchors <- try(.sapPlotFeatureTimes(list(fit), times), silent = TRUE)
  logTime <- options[["mixtureComponentPlotTransformXAxis"]] == "log"
  times   <- .sapAdaptivePlotTimes(times, evaluate, minimum = if (ci) 65L else 17L, maximum = 401L,
    xTransform = if (logTime) log else identity, xInverse = if (logTime) exp else identity,
    anchors = if (inherits(anchors, "try-error")) numeric(0) else anchors)

  mixtureSummary <- predictMixture(times, ci = ci, cl = level)
  componentSummaries <- lapply(componentIndices, function(k) predict(fn = componentFunction(k), t = times, ci = FALSE))
  predictionWarnings <- unique(c(attr(mixtureSummary, "predictionWarnings"), unlist(lapply(componentSummaries, attr, "predictionWarnings"))))

  componentLabels <- if (components == 1) gettext("Component 1") else c(gettext("Mixture"), gettextf("Component %1$i", componentIndices))
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
    if (type == "failureProbability") {
      mixtureData[["estimate"]] <- 1 - mixtureData[["estimate"]]
      mixtureData[c("lCi", "uCi")] <- 1 - mixtureData[c("uCi", "lCi")]
    }

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
