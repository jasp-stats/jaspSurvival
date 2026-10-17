# Fitted density and observed histogram diagnostic.
.sapDensityPlot <- function(jaspResults, options) {

  if (!is.null(jaspResults[["densityPlot"]]))
    return()

  if (.sapMergePlotsAcrossFamilies(options, "densityPlot")) {
    fit <- .sapExtractFit(jaspResults, options, type = "byModel", output = "densityPlot")
  } else {
    fit <- .sapNestFit(.sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options))
  }
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  dependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
    "densityPlot", "densityPlotXAxisTransformation", .sapObservedStrataDependencies("densityPlot"), "densityPlotMergePlotsAcrossDistributions",
    "densityPlotObservedData", "densityPlotObservedDataBinWidthType", "densityPlotObservedDataNumberOfBins",
    "densityPlotConfidenceIntervals", "densityPlotConfidenceIntervalsLevel", "confidenceIntervalBounds", .sapSimulationDependencies,
    "colorPalette", "plotLegend", "plotTheme")

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = .sapDensityPlotFun,
    name           = "densityPlot",
    title          = gettext("Density Plot"),
    dependencies   = dependencies,
    position       = 5.45
  )

  return()
}

.sapDensityPlotFun <- function(fit, options) {

  return(.sapDistributionPlotFun(fit, options, "densityPlot", "density"))
}

.sapDistributionPlotFun <- function(fit, options, prefix, type) {

  valid <- Filter(function(model) !is.null(model) && !jaspBase::isTryError(model), fit)
  if (!.saSurvivalReady(options) || length(valid) == 0L)
    return(createJaspPlot(title = gettext("Density")))

  valid     <- .sapObservedStrataFits(valid, options, prefix, meanDesign = prefix == "mixtureComponentPlot")
  labels    <- unique(unlist(lapply(valid, function(model) names(attr(model, paste0(prefix, "Strata")))), use.names = FALSE))
  container <- createJaspContainer()
  for (i in seq_along(labels)) {
    selected <- list()
    for (model in valid) {
      groups <- attr(model, paste0(prefix, "Strata"))
      index  <- match(labels[i], names(groups))
      if (is.na(index))
        next
      rows <- groups[[index]]
      attr(model, "plotDataset") <- .sapPlotObservedDataset(model)[rows, , drop = FALSE]
      if (prefix == "mixtureComponentPlot") {
        attr(model, "predictionX") <- attr(model, "predictionX")[index, , drop = FALSE]
        attr(model, "predictionLabels") <- labels[i]
      } else {
        attr(model, "predictionX") <- NULL
        attr(model, "predictionLabels") <- NULL
      }
      selected[[length(selected) + 1L]] <- model
    }
    dataset <- .sapPlotObservedDataset(selected[[1L]])
    plot    <- .sapDistributionPlotForData(selected, options, dataset, prefix, type)
    if (length(labels) == 1L && !nzchar(labels[i]))
      return(plot)
    plot$title <- decodeColNames(labels[i])
    plot$position <- i
    container[[paste0("plot", i)]] <- plot
  }
  return(container)
}

.sapDistributionPlotForData <- function(fit, options, dataset, prefix, type) {

  options[["predictionsDisplayConfidenceIntervals"]] <- prefix == "densityPlot" && options[["densityPlotConfidenceIntervals"]]
  if (prefix == "densityPlot")
    options[["predictionsDisplayConfidenceIntervalsLevel"]] <- options[["densityPlotConfidenceIntervalsLevel"]]
  plotData <- try({
    times  <- if (prefix == "densityPlot") .sapDensityPlotTimes(dataset, options)
    curves <- lapply(fit, function(model) {
      data <- .sapmComponentPlotData(model, options, .sapPlotObservedDataset(model), prefix = prefix, type = type, times = times)
      data[["Distribution"]] <- attr(model, "distribution")
      return(data)
    })
    warnings <- unique(unlist(lapply(curves, attr, "predictionWarnings")))
    data     <- do.call(rbind, curves)
    data[["Distribution"]] <- factor(data[["Distribution"]], levels = unique(data[["Distribution"]]))
    attr(data, "predictionWarnings") <- warnings
    data
  })

  return(.sapmComponentPlotForData(fit[[1]], options, dataset, prefix = prefix, type = type, plotData = plotData))
}

.sapDensityPlotTimes <- function(dataset, options) {

  time <- .saExtractSurvTimes(dataset, options)
  if (options[["densityPlotObservedData"]] && options[["censoringType"]] == "right") {
    histogram <- .sapmObservedDensity(dataset, options, prefix = "densityPlot")
    time      <- c(time, histogram[["lower"]], histogram[["upper"]])
  }

  if (options[["densityPlotXAxisTransformation"]] == "log") {
    limits <- .sapProbabilityPlotTimeRange(time)
    return(exp(seq(log(limits[1]), log(limits[2]), length.out = 101L)))
  }

  limits <- range(jaspGraphs::getPrettyAxisBreaks(range(c(0, time[is.finite(time)]))))
  return(seq(limits[1], limits[2], length.out = 101L))
}
