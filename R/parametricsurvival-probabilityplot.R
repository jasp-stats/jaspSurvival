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

.sapProbabilityPlot                   <- function(jaspResults, options) {

  if (!is.null(jaspResults[["probabilityPlot"]]) || options[["censoringType"]] != "right")
    return()

  # Probability-paper diagnostics are distribution-level checks by default.
  # When requested, group the selected distributions by model/subgroup and
  # overlay them in one canvas, following the prediction-plot merge pattern.
  if (.sapMergePlots(options, "probabilityPlot")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "probabilityPlot")
  } else {
    fit <- .sapExtractFit(jaspResults, options, type = "selected")
    fit <- .sapFlattenFit(fit, options)
  }

  outputDependencies <- c(
    .sapGetDependencies(options), "interpretModel", "compareModelsAcrossDistributions", "alwaysDisplayModelInformation",
    "probabilityPlot", "probabilityPlotCanvas", "probabilityPlotEmpiricalPoints",
    "probabilityPlotPointCoordinates", "probabilityPlotFittedCurve",
    "probabilityPlotCensoringEvents", "probabilityPlotMergePlotsAcrossDistributions",
    "probabilityPlotConfidenceInterval", "probabilityPlotConfidenceIntervalLevel", "confidenceIntervalSimulationDraws", "setSeed", "seed",
    "probabilityPlotGrid", "probabilityPlotPlottingPosition", "probabilityPlotRankAdjustment",
    "probabilityPlotTiesHandler", "probabilityPlotLegend", "probabilityPlotColorPalette",
    "probabilityPlotTheme",
    if (options[["analysisType"]] == "mixture") "probabilityPlotMergePlotsAcrossComponents"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapProbabilityPlotFun,
    name          = "probabilityPlot",
    title         = gettext("Probability Plot"),
    dependencies  = outputDependencies,
    position      = 5.5
  )

  return()
}

.sapProbabilityPlotFun <- function(fit, options) {

  fitList  <- .sapProbabilityPlotAsFitList(fit)
  fitValid <- .sapProbabilityPlotValidFits(fitList)
  width    <- .sapProbabilityPlotWidth(fitValid, options)

  tempPlot <- createJaspPlot(width = width, height = 420)

  if (length(fitValid) == 0)
    return(tempPlot)

  plot <- try(.sapCreateProbabilityPlot(fitList, options))

  if (jaspBase::isTryError(plot))
    tempPlot$setError(gettext("The model failed to produce a probability plot. Consider simplifying the model."))
  else
    tempPlot$plotObject <- plot

  return(tempPlot)
}

.sapProbabilityPlotAsFitList <- function(fit) {

  if (is.null(fit))
    return(list())

  if (inherits(fit, "try-error") || inherits(fit, "flexsurvreg"))
    return(list(fit))

  return(fit)
}

.sapProbabilityPlotValidFits <- function(fit) {

  fitList <- .sapProbabilityPlotAsFitList(fit)

  keep <- vapply(fitList, function(x) !is.null(x) && !jaspBase::isTryError(x) && length(x) > 0, logical(1))
  return(fitList[keep])
}

.sapProbabilityPlotWidth <- function(fit, options) {

  fitList <- .sapProbabilityPlotValidFits(fit)
  width   <- if (length(fitList) > 1) 620 else 520

  if (.sapProbabilityPlotHasSideLegend(fitList, options))
    width <- width + 120

  return(width)
}

.sapProbabilityPlotHasSideLegend <- function(fit, options) {

  if (!options[["probabilityPlotFittedCurve"]] && !options[["probabilityPlotEmpiricalPoints"]] && !options[["probabilityPlotCensoringEvents"]])
    return(FALSE)

  legendPosition <- .sapProbabilityPlotLegendPosition(options[["probabilityPlotLegend"]])
  if (!legendPosition %in% c("left", "right"))
    return(FALSE)

  fitList <- .sapProbabilityPlotValidFits(fit)
  if (length(fitList) == 0)
    return(FALSE)

  distributionLabels <- vapply(fitList, .sapProbabilityPlotDistributionLabel, character(1), options = options)
  hasDistribution    <- length(unique(stats::na.omit(distributionLabels))) > 1
  hasLevel           <- any(vapply(fitList, .sapProbabilityPlotFitCanShowLevels, logical(1), options = options))

  return(hasDistribution || hasLevel)
}

.sapProbabilityPlotFitCanShowLevels <- function(fit, options) {

  factors <- options[["factors"]]
  if (is.null(factors) || length(factors) == 0 || all(factors == ""))
    return(FALSE)

  modelTerms <- attr(fit, "modelTerms")
  if (is.null(modelTerms) || is.null(modelTerms[["components"]]))
    return(FALSE)

  components <- unlist(modelTerms[["components"]], use.names = FALSE)
  return(any(components %in% factors))
}

.sapCreateProbabilityPlot <- function(fit, options) {

  fitList <- .sapProbabilityPlotValidFits(fit)

  if (length(fitList) == 0)
    stop(gettext("The probability plot requires at least one fitted model."))

  dataset <- attr(fitList[[1]], "dataset")
  observedTimeRange <- .sapProbabilityPlotTimeRange(.saExtractSurvTimes(dataset, options))
  timeSequence <- .sapProbabilityPlotTimeSequence(observedTimeRange, options)

  empiricalData <- data.frame(time = numeric(0), probability = numeric(0), label = character(0))
  if (options[["probabilityPlotEmpiricalPoints"]])
    empiricalData <- .sapProbabilityPlotObservedData(dataset, fitList[[1]], options, .sapProbabilityPlotEmpiricalData)

  censoringData <- data.frame(time = numeric(0))
  if (options[["probabilityPlotCensoringEvents"]])
    censoringData <- .sapProbabilityPlotObservedData(dataset, fitList[[1]], options, .sapProbabilityPlotCensoringData)

  curveData <- .sapProbabilityPlotEmptyCurveData()
  if (options[["probabilityPlotFittedCurve"]]) {
    canvas <- .sapProbabilityPlotCanvasTransform(options[["probabilityPlotCanvas"]])
    probabilityRange <- if (.sapProbabilityPlotIsDetailed(options))
      .sapProbabilityPlotDetailedProbabilityRange(empiricalData[["probability"]]) else .sapProbabilityPlotProbabilityRange()
    limits <- if (.sapProbabilityPlotIsDetailed(options) && nrow(empiricalData) == 0)
      function(values) canvas[["transform"]](.sapProbabilityPlotDetailedProbabilityRange(.sapProbabilityPlotCurveProbability(1 - values)))
      else canvas[["transform"]](probabilityRange)
    evaluate <- function(times) do.call(cbind, lapply(fitList, function(model)
      .sapPlotPredictionMatrix(.sapSummaryPredictions(model, type = "survival", t = times, ci = FALSE))))
    anchors <- try(.sapPlotFeatureTimes(fitList, timeSequence), silent = TRUE)
    timeSequence <- .sapAdaptivePlotTimes(timeSequence[timeSequence > 0], evaluate,
      xTransform = if (.sapProbabilityPlotUsesLogTime(canvas)) log else identity,
      xInverse = if (.sapProbabilityPlotUsesLogTime(canvas)) exp else identity,
      yTransform = function(values) canvas[["transform"]](.sapProbabilityPlotCurveProbability(1 - values)),
      limits = limits, minimum = if (options[["probabilityPlotConfidenceInterval"]]) 65L else 17L,
      maximum = if (options[["probabilityPlotConfidenceInterval"]]) 129L else 201L,
      anchors = if (inherits(anchors, "try-error")) numeric(0) else anchors)
    curveData <- .sapProbabilityPlotCurveData(fitList, options, timeSequence)
  }

  if (nrow(empiricalData) == 0 && nrow(curveData) == 0 && nrow(censoringData) == 0)
    stop(gettext("The probability plot requires at least one positive observed failure time, censored observation, or fitted curve."))

  hasDistribution <- nrow(curveData) > 0 && length(unique(stats::na.omit(curveData[["Distribution"]]))) > 1
  levelLabels     <- sort(unique(stats::na.omit(c(curveData[["Level"]], empiricalData[["Level"]], censoringData[["Level"]]))))
  hasLevel        <- length(levelLabels) > 1
  hasGroup        <- nrow(curveData) > 0 && length(unique(stats::na.omit(curveData[["Group"]]))) > 1
  hasSeries       <- hasDistribution || hasLevel

  if (!hasSeries) {
    options[["probabilityPlotLegend"]]       <- "none"
    options[["probabilityPlotColorPalette"]] <- "colorblind"
  }

  plot <- ggplot2::ggplot()

  if (nrow(curveData) > 0 && options[["probabilityPlotConfidenceInterval"]] && any(is.finite(curveData[["lCi"]]) & is.finite(curveData[["uCi"]]))) {
    aesCall <- list(
      x     = as.name("time"),
      ymin  = as.name("lCi"),
      ymax  = as.name("uCi"),
      fill  = if (hasLevel) as.name("Level") else if (hasDistribution) as.name("Distribution"),
      group = if (hasGroup) as.name("Group")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = curveData, alpha = 0.22)
    if (!hasDistribution && !hasLevel)
      geomCall[["fill"]] <- "grey60"
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
  }

  if (nrow(curveData) > 0) {
    aesCall <- list(
      x        = as.name("time"),
      y        = as.name("probability"),
      color    = if (hasLevel) as.name("Level") else if (hasDistribution) as.name("Distribution"),
      linetype = if (hasDistribution && hasLevel) as.name("Distribution"),
      group    = if (hasGroup) as.name("Group")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = curveData)
    if (!hasDistribution && !hasLevel)
      geomCall[["color"]] <- "black"
    plot <- plot + do.call(jaspGraphs::geom_line, geomCall)
  }

  if (nrow(empiricalData) > 0 && options[["probabilityPlotEmpiricalPoints"]]) {
    aesCall <- list(x = as.name("time"), y = as.name("probability"), fill = if (hasLevel) as.name("Level"))
    geomCall <- list(
      data    = empiricalData,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      color   = "black",
      shape   = 21,
      size    = 2.1,
      stroke  = 0.6
    )
    if (!hasLevel)
      geomCall[["fill"]] <- "white"
    plot <- plot + do.call(ggplot2::geom_point, geomCall)

    if (options[["probabilityPlotPointCoordinates"]]) {
      plot <- plot + ggplot2::geom_text(
        data    = empiricalData,
        mapping = ggplot2::aes(x = time, y = probability, label = label),
        hjust   = -0.05,
        vjust   = -0.45,
        size    = 2.5,
        color   = "grey20"
      )
    }
  }

  if (hasSeries) {
    scaleCall <- list(palette = options[["probabilityPlotColorPalette"]])
    if (hasLevel)
      scaleCall[["limits"]] <- levelLabels
    plot <- plot +
      do.call(jaspGraphs::scale_JASPcolor_discrete, scaleCall) +
      do.call(jaspGraphs::scale_JASPfill_discrete, scaleCall)
  }

  plot <- .sapProbabilityPlotAddAxes(plot, empiricalData, curveData, censoringData, options, observedTimeRange, hasLevel)
  plot <- .sapProbabilityPlotAddTheme(plot, options)

  return(plot)
}

.sapProbabilityPlotTimeSequence <- function(timeRange, options) {

  canvas <- .sapProbabilityPlotCanvasTransform(options[["probabilityPlotCanvas"]])
  if (.sapProbabilityPlotIsDetailed(options))
    timeRange <- .sapProbabilityPlotDetailedTimeRange(timeRange, canvas)

  if (.sapProbabilityPlotUsesLogTime(canvas))
    return(exp(seq(log(timeRange[1]), log(timeRange[2]), length.out = 101)))

  return(seq(timeRange[1], timeRange[2], length.out = 101))
}

.sapProbabilityPlotObservedData <- function(dataset, fit, options, dataFunction) {

  modelFrame <- stats::model.frame(fit)
  factors    <- unique(attr(modelFrame, "covnames.orig"))

  # flexsurv produces one prediction at the mean design vector when a continuous
  # predictor is present. Split observations only when predictions show levels.
  if (length(factors) == 0 || !all(vapply(modelFrame[factors], function(x) is.factor(x) || is.character(x), logical(1))))
    return(dataFunction(dataset, options))

  labels <- vapply(seq_len(nrow(dataset)), function(i) {
    values <- vapply(factors, function(factor) as.character(dataset[[factor]][i]), character(1))
    return(paste0(factors, "=", values, collapse = ","))
  }, character(1))

  out <- lapply(unique(labels), function(label) {
    data <- dataFunction(dataset[labels == label, , drop = FALSE], options)
    data[["Level"]] <- rep(decodeColNames(label), nrow(data))
    return(data)
  })

  return(do.call(rbind, out))
}

.sapProbabilityPlotEmpiricalData <- function(dataset, options) {

  time  <- .saExtractSurvTimes(dataset, options)
  event <- dataset[[options[["eventStatus"]]]]

  if (!is.null(options[["weights"]]) && options[["weights"]] != "") {
    weights <- dataset[[options[["weights"]]]]
  } else {
    weights <- rep(1L, length(time))
  }

  keep <- is.finite(time) & time > 0 & !is.na(event) & is.finite(weights) & weights > 0
  time <- time[keep]
  event <- as.logical(event[keep])
  weights <- as.integer(weights[keep])

  if (length(time) == 0 || !any(event))
    return(data.frame(time = numeric(0), probability = numeric(0), label = character(0)))

  if (any(weights > 1L)) {
    observationIndex <- rep.int(seq_along(time), weights)
    time <- time[observationIndex]
    event <- event[observationIndex]
  }

  out <- .sapProbabilityPlotRankData(time, event, options)

  if (nrow(out) == 0)
    return(out)

  out[["probability"]] <- .sapProbabilityPlotClampProbability(out[["probability"]])
  out[["label"]]       <- sprintf("%s, %s", .sapProbabilityPlotTimeLabel(out[["time"]]), .sapProbabilityPlotProbabilityLabel(out[["probability"]]))
  rownames(out)        <- NULL

  return(out)
}

.sapProbabilityPlotCensoringData <- function(dataset, options) {

  time  <- .saExtractSurvTimes(dataset, options)
  event <- dataset[[options[["eventStatus"]]]]

  if (!is.null(options[["weights"]]) && options[["weights"]] != "") {
    weights <- dataset[[options[["weights"]]]]
  } else {
    weights <- rep(1L, length(time))
  }

  keep <- is.finite(time) & time > 0 & !is.na(event) & is.finite(weights) & weights > 0
  time <- time[keep]
  event <- as.logical(event[keep])
  weights <- as.integer(weights[keep])

  keepCensored <- !event
  time <- time[keepCensored]
  weights <- weights[keepCensored]

  if (length(time) == 0)
    return(data.frame(time = numeric(0)))

  if (any(weights > 1L))
    time <- rep.int(time, weights)

  out <- data.frame(time = time)
  out <- out[order(out[["time"]]), , drop = FALSE]
  rownames(out) <- NULL

  return(out)
}

.sapProbabilityPlotRankData <- function(time, event, options) {

  orderedData <- data.frame(time = time, event = event)
  orderedData <- orderedData[order(orderedData[["time"]], !orderedData[["event"]]), , drop = FALSE]

  n <- nrow(orderedData)
  if (options[["probabilityPlotRankAdjustment"]] == "kaplanMeier") {
    rankData <- .sapProbabilityPlotKaplanMeierAdjustedRanks(orderedData)
  } else {
    rankData <- .sapProbabilityPlotJohnsonAdjustedRanks(orderedData)
  }

  if (nrow(rankData) == 0)
    return(data.frame(time = numeric(0), probability = numeric(0), label = character(0)))

  rankData <- .sapProbabilityPlotHandleTies(rankData, options[["probabilityPlotTiesHandler"]])

  probability <- switch(
    options[["probabilityPlotPlottingPosition"]],
    "median"      = stats::qbeta(0.5, rankData[["adjustedRank"]], n - rankData[["adjustedRank"]] + 1),
    "benard"      = (rankData[["adjustedRank"]] - 0.3) / (n + 0.4),
    "hazen"       = (rankData[["adjustedRank"]] - 0.5) / n,
    "mean"        = rankData[["adjustedRank"]] / (n + 1),
    "kaplanMeier" = .sapProbabilityPlotKaplanMeierPlottingPosition(rankData[["adjustedRank"]], n),
    "blom"        = (rankData[["adjustedRank"]] - 0.375) / (n + 0.25),
    stats::qbeta(0.5, rankData[["adjustedRank"]], n - rankData[["adjustedRank"]] + 1)
  )

  out <- data.frame(time = rankData[["time"]], probability = probability)
  rownames(out) <- NULL

  return(out)
}

.sapProbabilityPlotJohnsonAdjustedRanks <- function(orderedData) {

  n <- nrow(orderedData)
  adjustedRank <- numeric(0)
  failureTime  <- numeric(0)
  previousRank <- 0

  for (i in seq_len(n)) {
    if (!orderedData[["event"]][i])
      next

    reverseRank <- n - i + 1
    previousRank <- previousRank + (n + 1 - previousRank) / (reverseRank + 1)

    adjustedRank <- c(adjustedRank, previousRank)
    failureTime  <- c(failureTime, orderedData[["time"]][i])
  }

  return(data.frame(time = failureTime, adjustedRank = adjustedRank))
}

.sapProbabilityPlotKaplanMeierAdjustedRanks <- function(orderedData) {

  n <- nrow(orderedData)
  adjustedRank <- numeric(0)
  previousRank <- 0

  for (i in seq_len(n)) {
    if (orderedData[["event"]][i]) {
      currentRank <- 1 - ((1 - previousRank) * (n - i) / (n - i + 1))
    } else {
      currentRank <- previousRank
    }

    adjustedRank <- c(adjustedRank, currentRank)
    previousRank <- currentRank
  }

  out <- data.frame(
    time         = orderedData[["time"]][orderedData[["event"]]],
    adjustedRank = adjustedRank[orderedData[["event"]]]
  )

  # WeibullR follows the Minitab convention of moving a final complete failure
  # just below 100%, so the point remains finite on probability paper.
  if (nrow(out) > 1 && isTRUE(all.equal(out[["adjustedRank"]][nrow(out)], 1)))
    out[["adjustedRank"]][nrow(out)] <- 1 - ((1 - out[["adjustedRank"]][nrow(out) - 1]) / 10)

  out[["adjustedRank"]] <- out[["adjustedRank"]] * n

  return(out)
}

.sapProbabilityPlotHandleTies <- function(data, tiesHandler) {

  if (!tiesHandler %in% c("highest", "lowest", "mean", "sequential"))
    return(data)

  tiedRanks <- stats::aggregate(
    data[["adjustedRank"]],
    by = list(time = data[["time"]]),
    FUN = function(x) c(lowest = min(x), highest = max(x))
  )
  lowest  <- tiedRanks[["x"]][, "lowest"]
  highest <- tiedRanks[["x"]][, "highest"]

  out <- data.frame(
    time         = tiedRanks[["time"]],
    adjustedRank = switch(
      tiesHandler,
      "highest"    = highest,
      "lowest"     = lowest,
      "mean"       = (highest + lowest) / 2,
      "sequential" = highest - cumsum(highest - lowest)
    )
  )

  return(out[order(out[["time"]]), , drop = FALSE])
}

.sapProbabilityPlotKaplanMeierPlottingPosition <- function(adjustedRank, n) {

  probability <- adjustedRank / n

  # Same finite-endpoint convention as WeibullR/SuperSMITH for KM plotting positions.
  if (length(adjustedRank) > 0 && isTRUE(all.equal(adjustedRank[length(adjustedRank)], n)))
    probability[length(probability)] <- length(adjustedRank) / (n + 0.001)

  return(probability)
}

.sapProbabilityPlotCurveData <- function(fit, options, timeSequence) {

  fitList <- .sapProbabilityPlotValidFits(fit)
  if (length(fitList) == 0)
    return(.sapProbabilityPlotEmptyCurveData())

  ciLevel <- .sapProbabilityPlotConfidenceIntervalLevel(options)

  out <- list()
  predictionWarnings <- character(0)
  for (i in seq_along(fitList)) {

    data <- .sapSummaryPredictions(fitList[[i]], type = "survival", t = timeSequence, ci = options[["probabilityPlotConfidenceInterval"]], cl = ciLevel, B = options[["confidenceIntervalSimulationDraws"]], seed = if (options[["setSeed"]]) options[["seed"]])
    predictionWarnings <- c(predictionWarnings, attr(data, "predictionWarnings"))

    for (j in seq_along(data)) {
      colnames(data[[j]]) <- c("time", "survival", "survivalLCI", "survivalUCI")

      data[[j]][["probability"]]  <- 1 - data[[j]][["survival"]]
      data[[j]][["lCi"]]          <- 1 - data[[j]][["survivalUCI"]]
      data[[j]][["uCi"]]          <- 1 - data[[j]][["survivalLCI"]]
      data[[j]][["Level"]]        <- if (length(data) > 1) decodeColNames(names(data)[j]) else NA_character_
      data[[j]][["Distribution"]] <- .sapProbabilityPlotDistributionLabel(fitList[[i]], options)
      data[[j]][["Group"]]        <- paste(data[[j]][["Distribution"]], data[[j]][["Level"]], sep = " | ")

      out[[length(out) + 1]] <- data[[j]][, c("time", "probability", "lCi", "uCi", "Level", "Distribution", "Group"), drop = FALSE]
    }
  }

  if (length(out) == 0)
    return(.sapProbabilityPlotEmptyCurveData())

  out <- do.call(rbind, out)
  out[["probability"]] <- .sapProbabilityPlotCurveProbability(out[["probability"]])
  out[["lCi"]]         <- .sapProbabilityPlotCurveProbability(out[["lCi"]])
  out[["uCi"]]         <- .sapProbabilityPlotCurveProbability(out[["uCi"]])
  out[["time"]][is.infinite(out[["time"]])]               <- NA
  out[["probability"]][is.infinite(out[["probability"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])]                 <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])]                 <- NA
  out <- out[stats::complete.cases(out[, c("time", "probability")]) & out[["time"]] > 0, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "predictionWarnings") <- unique(predictionWarnings)

  return(out)
}

.sapProbabilityPlotDistributionLabel <- function(fit, options) {

  distribution <- .sapSeriesLabel(fit, options)
  if (is.null(distribution) || length(distribution) == 0 || is.na(distribution[1]))
    distribution <- gettext("Fitted")

  return(as.character(distribution[1]))
}

.sapProbabilityPlotEmptyCurveData <- function() {
  return(data.frame(
    time         = numeric(0),
    probability  = numeric(0),
    lCi          = numeric(0),
    uCi          = numeric(0),
    Level        = character(0),
    Distribution = character(0),
    Group        = character(0)
  ))
}

.sapProbabilityPlotAddAxes <- function(plot, empiricalData, curveData, censoringData, options, observedTimeRange = NULL, hasLevel = FALSE) {

  canvas   <- .sapProbabilityPlotCanvasTransform(options[["probabilityPlotCanvas"]])
  detailed <- .sapProbabilityPlotIsDetailed(options)

  if (detailed) {
    axisSetup <- .sapProbabilityPlotDetailedAxisSetup(empiricalData, curveData, censoringData, observedTimeRange, canvas)
    plot <- plot + ggplot2::geom_hline(
      yintercept = canvas[["inverse"]](0),
      linetype   = 3,
      color      = "grey70",
      linewidth  = 0.35
    )
  } else {
    axisSetup <- .sapProbabilityPlotDefaultAxisSetup(empiricalData, curveData, censoringData, canvas)
  }

  plot <- .sapProbabilityPlotCensoringEvents(plot, censoringData, hasLevel)

  xScaleCall <- list(
    trans        = canvas[["xTransform"]],
    breaks       = axisSetup[["xBreaks"]],
    minor_breaks = axisSetup[["xMinor"]],
    limits       = axisSetup[["timeRange"]],
    labels       = .sapProbabilityPlotTimeLabel,
    oob          = scales::oob_keep
  )
  yScaleCall <- list(
    trans        = scales::new_transform(name = canvas[["name"]], transform = canvas[["transform"]], inverse = canvas[["inverse"]]),
    breaks       = axisSetup[["yBreaks"]],
    minor_breaks = axisSetup[["yMinor"]],
    labels       = .sapProbabilityPlotProbabilityLabel,
    limits       = axisSetup[["probabilityRange"]],
    oob          = scales::oob_keep
  )

  if (detailed) {
    xScaleCall[["sec.axis"]] <- ggplot2::dup_axis(name = NULL, labels = .sapProbabilityPlotTimeLabel)
    yScaleCall[["sec.axis"]] <- ggplot2::dup_axis(name = NULL, labels = .sapProbabilityPlotProbabilityLabel)
  }

  plot <- plot +
    do.call(ggplot2::scale_x_continuous, xScaleCall) +
    do.call(ggplot2::scale_y_continuous, yScaleCall) +
    ggplot2::xlab(canvas[["xLabel"]]) +
    ggplot2::ylab(gettextf("Failure Probability (%s scale)", canvas[["label"]]))

  return(plot)
}

.sapProbabilityPlotDefaultAxisSetup <- function(empiricalData, curveData, censoringData, canvas) {

  timeValues <- c(empiricalData[["time"]], curveData[["time"]], censoringData[["time"]])
  timeRange  <- .sapProbabilityPlotTimeRange(timeValues)
  probabilityRange <- .sapProbabilityPlotProbabilityRange()
  yAxisSetup <- .sapProbabilityPlotAxisBreaks(probabilityRange, canvas)

  return(list(
    timeRange        = timeRange,
    probabilityRange = probabilityRange,
    xBreaks          = .sapProbabilityPlotTimeBreaks(timeRange, canvas),
    xMinor           = .sapProbabilityPlotTimeMinorBreaks(timeRange, canvas),
    yBreaks          = yAxisSetup[["major"]],
    yMinor           = yAxisSetup[["minor"]]
  ))
}

.sapProbabilityPlotDetailedAxisSetup <- function(empiricalData, curveData, censoringData, observedTimeRange = NULL, canvas) {

  if (is.null(observedTimeRange)) {
    timeValues <- c(empiricalData[["time"]], curveData[["time"]], censoringData[["time"]])
    observedTimeRange <- .sapProbabilityPlotTimeRange(timeValues)
  }
  timeRange <- .sapProbabilityPlotDetailedTimeRange(observedTimeRange, canvas)

  probabilityValues <- empiricalData[["probability"]]
  if (length(probabilityValues) == 0)
    probabilityValues <- curveData[["probability"]]

  probabilityRange <- .sapProbabilityPlotDetailedProbabilityRange(probabilityValues)
  yAxisSetup <- .sapProbabilityPlotAxisBreaks(probabilityRange, canvas, detailed = TRUE)

  return(list(
    timeRange        = timeRange,
    probabilityRange = probabilityRange,
    xBreaks          = .sapProbabilityPlotTimeBreaks(timeRange, canvas),
    xMinor           = .sapProbabilityPlotTimeMinorBreaks(timeRange, canvas),
    yBreaks          = yAxisSetup[["major"]],
    yMinor           = yAxisSetup[["minor"]]
  ))
}

.sapProbabilityPlotCensoringEvents <- function(plot, censoringData, hasLevel = FALSE) {

  if (is.null(censoringData) || nrow(censoringData) == 0)
    return(plot)

  aesCall <- list(x = as.name("time"), color = if (hasLevel) as.name("Level"))
  geomCall <- list(
    data    = censoringData,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    sides   = "b",
    alpha   = 0.5,
    size    = 0.5
  )
  if (!hasLevel)
    geomCall[["color"]] <- "darkblue"
  plot <- plot + do.call(ggplot2::geom_rug, geomCall)

  return(plot)
}

.sapProbabilityPlotCanvasTransform <- function(canvas) {

  switch(
    canvas,
    "exponential" = list(
      # Exponential probability paper plots cumulative hazard against linear time.
      name       = "exponentialProbability",
      label      = gettext("exponential"),
      xTransform = "identity",
      xLabel     = gettext("Time"),
      transform  = function(p) -log1p(-p),
      inverse    = function(x) -expm1(-x)
    ),
    "lognormal" = list(
      name       = "lognormalProbability",
      label      = gettext("log-normal"),
      xTransform = "log",
      xLabel     = gettext("Time (log scale)"),
      transform  = stats::qnorm,
      inverse    = stats::pnorm
    ),
    "loglogistic" = list(
      name       = "loglogisticProbability",
      label      = gettext("log-logistic"),
      xTransform = "log",
      xLabel     = gettext("Time (log scale)"),
      transform  = stats::qlogis,
      inverse    = stats::plogis
    ),
    list(
      name       = "weibullProbability",
      label      = gettext("Weibull"),
      xTransform = "log",
      xLabel     = gettext("Time (log scale)"),
      transform  = function(p) log(-log1p(-p)),
      inverse    = function(x) -expm1(-exp(x))
    )
  )
}

.sapProbabilityPlotAddTheme <- function(plot, options) {

  if (options[["probabilityPlotTheme"]] == "jasp") {
    plot <- plot +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(size = ggplot2::rel(0.9)),
        axis.text.y  = ggplot2::element_text(size = ggplot2::rel(0.9)),
        axis.title.x = ggplot2::element_text(size = ggplot2::rel(0.9)),
        axis.title.y = ggplot2::element_text(size = ggplot2::rel(0.9))
      )
  } else {
    plot <- plot +
      switch(
        options[["probabilityPlotTheme"]],
        "detailed"        = ggplot2::theme_light(),
        "whiteBackground" = ggplot2::theme_bw(),
        "light"           = ggplot2::theme_light(),
        "minimal"         = ggplot2::theme_minimal(),
        "pubr"            = jaspGraphs::themePubrRaw(legend = "none"),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = "none"),
        ggplot2::theme_light()
      )
  }

  legendTheme <- .sapProbabilityPlotLegendTheme(options[["probabilityPlotLegend"]])
  plot <- plot + legendTheme

  if (options[["probabilityPlotGrid"]]) {
    if (.sapProbabilityPlotIsDetailed(options)) {
      plot <- plot + ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey82", linewidth = 0.25),
        panel.grid.minor = ggplot2::element_line(color = "grey82", linewidth = 0.25)
      )
    } else {
      plot <- plot + ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.3),
        panel.grid.minor = ggplot2::element_line(color = "grey92", linewidth = 0.2)
      )
    }
  } else {
    plot <- plot + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  }

  return(plot)
}

.sapProbabilityPlotLegendTheme <- function(legendPosition) {

  legendPosition <- .sapProbabilityPlotLegendPosition(legendPosition)

  if (legendPosition == "none")
    return(ggplot2::theme(legend.position = "none"))

  return(ggplot2::theme(
    legend.position       = legendPosition,
    legend.background     = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_blank(),
    legend.key            = ggplot2::element_blank()
  ))
}

.sapProbabilityPlotLegendPosition <- function(legendPosition) {

  if (is.null(legendPosition))
    return("right")

  return(switch(
    legendPosition,
    "bottom"      = "bottom",
    "right"       = "right",
    "left"        = "left",
    "top"         = "top",
    "none"        = "none",
    "right"
  ))
}

.sapProbabilityPlotConfidenceIntervalLevel <- function(options) {

  level <- options[["probabilityPlotConfidenceIntervalLevel"]]
  if (is.null(level))
    level <- 0.90
  if (level > 1)
    level <- level / 100

  return(level)
}

.sapProbabilityPlotIsDetailed <- function(options) {
  return(identical(options[["probabilityPlotTheme"]], "detailed"))
}

.sapProbabilityPlotProbabilityRange <- function() {
  return(c(0.001, 0.999))
}

.sapProbabilityPlotDetailedProbabilityRange <- function(probability) {

  probability <- probability[is.finite(probability)]

  if (length(probability) == 0) {
    probabilityRange <- c(0.01, 0.99)
  } else if (min(probability) < 0.01) {
    probabilityRange <- c(signif(min(probability), 1), 0.99)
  } else {
    probabilityRange <- c(0.01, 0.99)
  }

  probabilityRange <- .sapProbabilityPlotClampProbability(probabilityRange)

  return(probabilityRange)
}

.sapProbabilityPlotClampProbability <- function(probability) {

  probabilityRange <- .sapProbabilityPlotProbabilityRange()
  return(pmin(pmax(probability, probabilityRange[1]), probabilityRange[2]))
}

.sapProbabilityPlotCurveProbability <- function(probability) {

  # Keep endpoints finite on probability paper. Leave enough precision in the
  # upper tail for the inverse transformation used by mirrored axes to remain
  # strictly monotonic. This cap is beyond the displayed probability range.
  probability[which(probability == 0)] <- .Machine$double.xmin
  upper <- 1 - sqrt(.Machine$double.eps)
  probability[which(probability > upper)] <- upper

  return(probability)
}

.sapProbabilityPlotTimeRange <- function(time) {

  time <- time[is.finite(time) & time > 0]
  if (length(time) == 0)
    stop(gettext("The probability plot requires positive time values."))

  timeRange <- range(time)
  if (timeRange[1] == timeRange[2])
    timeRange <- c(timeRange[1] * 0.8, timeRange[2] * 1.2)

  return(timeRange)
}

.sapProbabilityPlotDetailedTimeRange <- function(timeRange, canvas) {

  if (!.sapProbabilityPlotUsesLogTime(canvas)) {
    padding <- diff(timeRange) / 2
    return(c(max(0, timeRange[1] - padding), timeRange[2] + padding))
  }

  return(c(
    10^(log10(timeRange[1]) - 0.5),
    10^(log10(timeRange[2]) + 1)
  ))
}

.sapProbabilityPlotTimeBreaks <- function(timeRange, canvas) {

  if (!.sapProbabilityPlotUsesLogTime(canvas)) {
    breaks <- jaspGraphs::getPrettyAxisBreaks(timeRange)
    return(breaks[breaks >= timeRange[1] & breaks <= timeRange[2]])
  }

  exponentRange <- seq(floor(log10(timeRange[1])), ceiling(log10(timeRange[2])))
  breaks <- as.vector(outer(c(1, 2, 5), 10^exponentRange, "*"))
  breaks <- sort(unique(breaks[breaks >= timeRange[1] & breaks <= timeRange[2]]))

  if (length(breaks) < 2) {
    breaks <- jaspGraphs::getPrettyAxisBreaks(timeRange)
    breaks <- breaks[breaks > 0]
  }

  return(breaks)
}

.sapProbabilityPlotSeqLog <- function(from, to, base = c(1, 2, 5)) {

  if (!is.finite(from) || !is.finite(to) || from <= 0 || to <= 0 || from >= to)
    return(numeric(0))

  exponentRange <- seq(floor(log10(from)), floor(log10(to)))
  breaks <- as.vector(outer(base, 10^exponentRange, "*"))
  breaks <- sort(unique(breaks[breaks >= from & breaks <= to & breaks > 0]))

  return(breaks)
}

.sapProbabilityPlotSeqProbability <- function(from, to, base = 1:9) {

  lower <- .sapProbabilityPlotSeqLog(from, 0.9, base)
  upper <- rev(1 - .sapProbabilityPlotSeqLog(1 - to, 0.1, base))
  if (length(upper) > 0)
    upper <- upper[-1]

  breaks <- c(lower, upper)
  breaks <- sort(unique(breaks[breaks >= from & breaks <= to]))

  return(breaks)
}

.sapProbabilityPlotTimeMinorBreaks <- function(timeRange, canvas) {

  if (!.sapProbabilityPlotUsesLogTime(canvas))
    return(numeric(0))

  exponentRange <- seq(floor(log10(timeRange[1])), ceiling(log10(timeRange[2])))
  breaks <- as.vector(outer(1:9, 10^exponentRange, "*"))
  breaks <- sort(unique(breaks[breaks >= timeRange[1] & breaks <= timeRange[2]]))

  return(breaks)
}

.sapProbabilityPlotUsesLogTime <- function(canvas) {
  return(identical(canvas[["xTransform"]], "log"))
}

.sapProbabilityPlotIsExponential <- function(canvas) {
  return(identical(canvas[["name"]], "exponentialProbability"))
}

.sapProbabilityPlotAxisBreaks <- function(probabilityRange, canvas, detailed = FALSE) {

  if (.sapProbabilityPlotIsExponential(canvas)) {
    major <- c(probabilityRange[1], 0.30, 0.50, 0.70, 0.80, 0.90, 0.95, 0.98, 0.99, 0.995, probabilityRange[2])
    major <- sort(unique(major[major >= probabilityRange[1] & major <= probabilityRange[2]]))

    cumulativeHazard <- -log1p(-major)
    minorHazard      <- cumulativeHazard[-length(cumulativeHazard)] + diff(cumulativeHazard) / 2
    minor            <- -expm1(-minorHazard)
  } else if (detailed) {
    probabilityGridRange <- c(probabilityRange[1] / 10, 1 - (1 - probabilityRange[2]) / 10)

    major <- sort(unique(c(
      .sapProbabilityPlotSeqProbability(probabilityGridRange[1], probabilityGridRange[2], c(1, 2, 5)),
      0.9
    )))
    major <- major[major >= probabilityRange[1] & major <= probabilityRange[2]]

    minor <- .sapProbabilityPlotSeqProbability(probabilityGridRange[1], probabilityGridRange[2], 1:9)
    minor <- minor[minor >= probabilityRange[1] & minor <= probabilityRange[2]]
  } else {
    major <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 0.98, 0.99, 0.995, 0.999)
    minor <- sort(unique(c(seq(0.001, 0.009, by = 0.001), seq(0.01, 0.09, by = 0.01), seq(0.10, 0.90, by = 0.10), seq(0.91, 0.99, by = 0.01), 0.995, 0.999)))
  }

  return(list(major = major, minor = minor))
}

.sapProbabilityPlotProbabilityLabel <- function(probability) {

  return(paste0(formatC(100 * probability, format = "fg", digits = 4), "%"))
}

.sapProbabilityPlotTimeLabel <- function(time) {

  return(formatC(time, format = "fg", digits = 4))
}
