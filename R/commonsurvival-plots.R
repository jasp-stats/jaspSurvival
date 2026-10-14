# Trim CI limits only, retaining every estimate and the full plotted bands.
# Callers must use oob_keep. Adaptive grids are weighted by displayed x span.
.saPlotEstimateRange <- function(estimate, lCi = numeric(0), uCi = numeric(0), bounded = FALSE,
                                 trimLower = FALSE, at = NULL, group = rep(1, length(at))) {

  estimate <- estimate[is.finite(estimate)]
  ci       <- c(lCi, uCi)
  weights  <- if (is.null(at)) rep(1, length(ci)) else {
    nodes <- .saPlotCiWeights(at, is.finite(lCi) | is.finite(uCi), group)
    c(if (length(lCi) > 0) nodes, if (length(uCi) > 0) nodes)
  }
  keep     <- is.finite(ci) & weights > 0
  ci       <- ci[keep]
  weights  <- weights[keep]
  full     <- range(c(estimate, ci))

  if (bounded || length(ci) == 0)
    return(full)

  # Each curve has equal weight, independently of its adaptive point count.
  order         <- order(ci)
  cumulative    <- cumsum(weights[order]) / sum(weights)
  probabilities <- if (trimLower) c(0.025, 0.975) else c(0, 0.95)
  ciLimits      <- vapply(probabilities, function(p) ci[order[which(cumulative >= p)[1]]], numeric(1))
  limits        <- range(c(estimate, ciLimits))

  if (length(estimate) > 0) {
    lineRange <- range(estimate)
    lineSpan  <- diff(lineRange)
    if (lineSpan == 0)
      lineSpan <- max(abs(lineRange))
    padding <- 0.25 * lineSpan
    # Limit trimming rather than enlarge a band that originally had less room.
    if (limits[1] > full[1])
      limits[1] <- max(full[1], min(limits[1], lineRange[1] - padding))
    if (limits[2] < full[2])
      limits[2] <- min(full[2], max(limits[2], lineRange[2] + padding))
  }

  return(limits)
}

.saPlotCiWeights <- function(at, hasCi, group) {

  weights <- numeric(length(at))
  indices <- which(is.finite(at) & hasCi)
  for (index in split(indices, factor(group[indices], exclude = NULL))) {
    index <- index[order(at[index])]
    gaps  <- diff(at[index])
    if (length(gaps) == 0 || sum(gaps) == 0) {
      weights[index] <- 1 / length(index)
    } else {
      # Trapezoidal weights assign each endpoint half its neighbouring intervals.
      weights[index] <- (c(0, gaps) + c(gaps, 0)) / (2 * sum(gaps))
    }
  }
  return(weights)
}

# Residual diagnostics shared by Cox and parametric models.
.saResidualsPredictors      <- function(predictorsFit, modelFrame, factors, contrasts = attr(predictorsFit, "contrasts")) {

  predictors <- as.data.frame(predictorsFit)
  factors    <- intersect(unlist(factors), names(modelFrame))
  if (ncol(predictors) == 0 || length(factors) == 0)
    return(predictors)

  # Match fitted factor contrast columns exactly; binary covariates remain numeric.
  if (!is.null(contrasts))
    contrasts <- contrasts[intersect(names(contrasts), factors)]
  factorMatrix  <- stats::model.matrix(stats::reformulate(factors), data = modelFrame[, factors, drop = FALSE], contrasts.arg = contrasts)
  factorColumns <- intersect(setdiff(colnames(factorMatrix), "(Intercept)"), colnames(predictors))
  for (column in factorColumns)
    predictors[[column]] <- factor(predictors[[column]])

  return(predictors)
}
.saResidualsPlot            <- function(x, y, xlab, ylab) {

  yTicks <- jaspGraphs::getPrettyAxisBreaks(y)

  tempPlot <- ggplot2::ggplot() +
    jaspGraphs::geom_point(mapping = ggplot2::aes(x = x, y = y),
                          position = if (is.factor(x)) ggplot2::position_jitter(width = 0.1, height = 0, seed = 1) else "identity") +
    ggplot2::labs(
      x     = xlab,
      y     = ylab
    )
  if (is.factor(x)) {
    tempPlot <- tempPlot + ggplot2::scale_x_discrete()
  } else {
    xTicks   <- jaspGraphs::getPrettyAxisBreaks(x)
    tempPlot <- tempPlot + jaspGraphs::scale_x_continuous(limits = range(xTicks), breaks = xTicks)
  }
  tempPlot <- tempPlot + jaspGraphs::scale_y_continuous(limits = range(yTicks), breaks = yTicks)

  tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()

  return(tempPlot)
}
.saResidualsPlotName        <- function(options) {
  switch(
    options[["residualPlotResidualType"]],
    "response"         = gettext("Response"),
    "coxSnell"         = gettext("Cox-Snell"),
    "martingale"       = gettext("Martingale Residuals"),
    "deviance"         = gettext("Deviance Residuals"),
    "score"            = gettext("Score Residuals"),
    "schoenfeld"       = gettext("Schoenfeld Residuals"),
    "scaledSchoenfeld" = gettext("Scaled Schoenfeld Residuals")
  )
}

# Kaplan-Meier and Cox survival plots.
.saGetSurvivalPlotHeight <- function(options) {
  if (!options[["plotRiskTable"]])
    return(400)
  else if (!options[["plotRiskTableAsASingleLine"]])
    return(450)
  else
    return(400 + 50 * sum(c(
      options[["plotRiskTableNumberAtRisk"]],
      options[["plotRiskTableCumulativeNumberOfObservedEvents"]],
      options[["plotRiskTableCumulativeNumberOfCensoredObservations"]],
      options[["plotRiskTableNumberOfEventsInTimeInterval"]],
      options[["plotRiskTableNumberOfCensoredObservationsInTimeInterval"]]
    )))
}
.saSurvivalPlot          <- function(jaspResults, dataset, options, type) {

  if (!is.null(jaspResults[["surivalPlot"]]))
    return()

  surivalPlot <- createJaspPlot(title = switch(
    options[["plotType"]],
    "survival"             = gettext("Survival Plot"),
    "risk"                 = gettext("Risk Plot"),
    "cumulativeHazard"     = gettext("Cumulative Hazard Plot"),
    "complementaryLogLog"  = gettext("Complementary Log-Log Plot")
  ), width = 450, height = .saGetSurvivalPlotHeight(options))
  surivalPlot$dependOn(c(if (type == "Cox") .saspDependencies else .sanpDependencies, "plot", "plotType", "plotCi", "plotRiskTable",
                         "plotRiskTableNumberAtRisk", "plotRiskTableCumulativeNumberOfObservedEvents",
                         "plotRiskTableCumulativeNumberOfCensoredObservations", "plotRiskTableNumberOfEventsInTimeInterval",
                         "plotRiskTableNumberOfCensoredObservationsInTimeInterval", "plotRiskTableAsASingleLine",
                         "plotAddQuantile", "plotAddQuantileValue",
                         "colorPalette", "plotLegend", "plotTheme"))
  surivalPlot$position <- switch(
    type,
    "KM"  = 3,
    "Cox" = 7
  )
  jaspResults[["surivalPlot"]] <- surivalPlot

  if (is.null(jaspResults[["fit"]]))
    return()

  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit))
    return()

  .ggsurvfit2JaspPlot <- function(x) {
    grDevices::png(f <- tempfile())
    on.exit({
      grDevices::dev.off()
      if (file.exists(f))
        file.remove(f)
    })
    return(ggsurvfit::ggsurvfit_build(x))
  }

  tempPlot <- try(ggsurvfit::ggsurvfit(
    x = if (type == "KM") ggsurvfit::survfit2(.saGetFormula(options, type = type), data = dataset)
        else ggsurvfit::survfit2(fit),
    type = switch(options[["plotType"]],
      "survival"            = "survival",
      "risk"                = "risk",
      "cumulativeHazard"    = "cumhaz",
      "complementaryLogLog" = "cloglog"
    ),
    linewidth = 1
  ))

  if (jaspBase::isTryError(tempPlot)) {
    surivalPlot$setError(tempPlot)
    return()
  }

  if (options[["plotCi"]])
    tempPlot <- tempPlot + ggsurvfit::add_confidence_interval()

  if (options[["plotRiskTable"]]) {
    riskTableStatistics <-  c(
      if (options[["plotRiskTableNumberAtRisk"]])                               "n.risk",
      if (options[["plotRiskTableCumulativeNumberOfObservedEvents"]])           "cum.event",
      if (options[["plotRiskTableCumulativeNumberOfCensoredObservations"]])     "cum.censor",
      if (options[["plotRiskTableNumberOfEventsInTimeInterval"]])               "n.event",
      if (options[["plotRiskTableNumberOfCensoredObservationsInTimeInterval"]]) "n.censor"
    )

    if (length(riskTableStatistics) > 0) {
      if (options[["plotRiskTableAsASingleLine"]])
        riskTableStatistics <- paste0("{", riskTableStatistics, "}", collapse = ", ")

      tempPlot <- tempPlot + ggsurvfit::add_risktable(risktable_stats = riskTableStatistics)
    }
  }

  if (options[["plotAddQuantile"]])
    tempPlot <- tempPlot + ggsurvfit::add_quantile(y_value = options[["plotAddQuantileValue"]], color = "gray50", linewidth = 0.75)

  if (options[["plotTheme"]] == "jasp")
    tempPlot <- tempPlot +
      jaspGraphs::geom_rangeframe(sides = "bl") +
      jaspGraphs::themeJaspRaw(legend.position = options[["plotLegend"]])
  else
    tempPlot <- tempPlot + ggplot2::theme(legend.position = options[["plotLegend"]])

  # scaling and formatting
  tempPlot <- tempPlot +
    jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])

  yScaleCall <- list()
  if (options[["plotCi"]] && options[["plotType"]] %in% c("cumulativeHazard", "complementaryLogLog") &&
      any(is.finite(tempPlot$data[["estimate"]]))) {
    yRange <- .saPlotEstimateRange(
      estimate  = tempPlot$data[["estimate"]],
      lCi       = tempPlot$data[["conf.low"]],
      uCi       = tempPlot$data[["conf.high"]],
      trimLower = options[["plotType"]] == "complementaryLogLog",
      at        = if (options[["plotType"]] == "complementaryLogLog") log(tempPlot$data[["time"]]) else tempPlot$data[["time"]],
      group     = if ("strata" %in% names(tempPlot$data)) tempPlot$data[["strata"]] else rep(1, nrow(tempPlot$data)))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)
    yScaleCall <- list(limits = range(yBreaks), breaks = yBreaks, oob = scales::oob_keep)
  }

  if (options[["plotType"]] == "complementaryLogLog") {
    tempPlot <- tempPlot + ggplot2::scale_x_continuous(transform = "log") + ggplot2::xlab(gettext("log(Time)"))
    if (length(yScaleCall) > 0)
      tempPlot <- tempPlot + do.call(ggplot2::scale_y_continuous, yScaleCall)
  } else {
    tempPlot <- tempPlot + ggsurvfit::scale_ggsurvfit(y_scales = yScaleCall)
  }

  tempPlot <- try(.ggsurvfit2JaspPlot(tempPlot))
  if (jaspBase::isTryError(tempPlot)) {
    surivalPlot$setError(tempPlot)
    return()
  }

  surivalPlot$plotObject <- tempPlot

  return()
}
