.sargResidualPlotOptions <- c("residualPlotResidualVsTime", "residualPlotResidualVsPredicted", "residualPlotResidualHistogram")
.sargProbabilityDependencies <- c("probabilityPlot", "probabilityPlotCanvas", "probabilityPlotEmpiricalPoints",
  "probabilityPlotEmpiricalPointsCoordinates", "probabilityPlotReferenceLine", "probabilityPlotMergePlotsAcrossSubgroups",
  "probabilityPlotReferenceEnvelope", "probabilityPlotReferenceEnvelopeLevel", "probabilityPlotGrid",
  "probabilityPlotPlottingPosition", "probabilityPlotLegend", "probabilityPlotColorPalette", "probabilityPlotTheme")

.sargIntervalMean <- function(fit) {

  mean <- diff(c(0, .sargFittedCumulativeFailures(fit)))
  if (any(!is.finite(mean) | mean <= 0))
    stop(gettext("The fitted curve does not give finite positive expected counts in every interval. Count residuals and probability diagnostics require a valid increasing NHPP cumulative mean."))
  return(mean)
}

.sargResidualLabel <- function(type) {

  return(switch(type, regression = gettext("Regression Residuals"), response = gettext("Response Residuals"),
    pearson = gettext("Pearson Residuals"), deviance = gettext("Deviance Residuals"), quantile = gettext("Randomized Quantile Residuals")))
}

.sargResidualValue <- function(fit, type, quantile = NULL) {

  if (type == "quantile") {
    if (jaspBase::isTryError(quantile))
      stop(jaspBase::.extractErrorMessage(quantile))
    return(quantile)
  }
  if (type == "regression") {
    if (fit[["method"]] != "LS")
      stop(gettext("Regression residuals are available only for least-squares fits. Choose another residual type."))
    return(as.numeric(stats::residuals(fit[["native"]][["model"]])))
  }
  mean <- .sargIntervalMean(fit)
  observed <- fit[["counts"]]
  difference <- observed - mean
  return(switch(type,
    response = difference,
    pearson = difference / sqrt(mean),
    deviance = .sargPoissonDeviance(observed, mean)
  ))
}

.sargPoissonDeviance <- function(observed, mean) {

  deviance <- stats::poisson()[["dev.resids"]](observed, mean, rep(1, length(mean)))
  close <- observed > 0 & abs(mean - observed) < .01 * observed
  ratio <- (mean[close] - observed[close]) / observed[close]
  # r - log1p(r) loses precision near zero; use its series through r^12.
  polynomial <- rep(1 / 12, length(ratio))
  for (power in 11:2)
    polynomial <- (-1)^power / power + ratio * polynomial
  deviance[close] <- 2 * observed[close] * ratio^2 * polynomial
  return(sign(observed - mean) * sqrt(pmax(0, deviance)))
}

.sargPoissonQuantiles <- function(observed, mean, uniform = stats::runif(length(observed))) {

  # Dunn-Smyth randomization within each observed count's Poisson CDF jump.
  # Evaluate both tails in log space to retain extreme finite residuals.
  mass <- stats::dpois(observed, mean, log = TRUE)
  lower <- .sapmLogSumExp(cbind(stats::ppois(observed - 1, mean, log.p = TRUE), log(uniform) + mass))
  upper <- .sapmLogSumExp(cbind(stats::ppois(observed, mean, lower.tail = FALSE, log.p = TRUE), log1p(-uniform) + mass))
  left <- lower <= upper
  residual <- numeric(length(observed))
  residual[left] <- stats::qnorm(lower[left], log.p = TRUE)
  residual[!left] <- stats::qnorm(upper[!left], lower.tail = FALSE, log.p = TRUE)
  if (any(!is.finite(residual)))
    stop(gettext("Finite randomized quantile residuals could not be calculated for this fitted count model."))
  return(residual)
}

.sargQuantileState <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["quantileResiduals"]]))
    return(jaspResults[["quantileResiduals"]]$object)
  state <- createJaspState()
  state$dependOn(.sargFitDependencies)
  jaspResults[["quantileResiduals"]] <- state
  jaspBase::.setSeedJASP(options)
  state$object <- lapply(fits, function(fit) try(.sargPoissonQuantiles(fit[["counts"]], .sargIntervalMean(fit)), silent = TRUE))
  return(state$object)
}

.sargDiagnostics <- function(jaspResults, options) {

  requested <- vapply(.sargResidualPlotOptions, function(name) options[[name]], logical(1))
  if (!any(requested) && !options[["probabilityPlot"]])
    return()
  fits <- .sargFits(jaspResults, valid = TRUE)
  if (!length(fits))
    return()
  quantiles <- if ((any(requested) && options[["residualPlotResidualType"]] == "quantile") || options[["probabilityPlot"]])
    .sargQuantileState(jaspResults, options, fits)
  for (i in which(requested))
    .sargResidualPlots(jaspResults, options, fits, quantiles, c("time", "predicted", "histogram")[i], .sargResidualPlotOptions[i], 6 + i / 10)
  if (options[["probabilityPlot"]])
    .sargProbabilityPlot(jaspResults, options, fits, quantiles)
  return()
}

.sargDiagnosticSection <- function(jaspResults, options, key, title, dependencies, position, fits, builder, width = 440, height = 320) {

  if (!is.null(jaspResults[[key]]))
    return()
  if (length(fits) == 1L && nzchar(fits[[1L]][["label"]]) && .sapHasSubgroups(options))
    title <- gettextf("%1$s: %2$s", title, fits[[1L]][["label"]])
  output <- if (length(fits) == 1L) createJaspPlot(title = title, width = width, height = height) else createJaspContainer(title = title)
  output$dependOn(c(.sargFitDependencies, dependencies))
  output$position <- position
  jaspResults[[key]] <- output
  for (i in seq_along(fits)) {
    plot <- if (length(fits) == 1L) output else createJaspPlot(title = fits[[i]][["label"]], width = width, height = height)
    if (length(fits) > 1L) {
      plot$position <- i
      output[[paste0("plot", i)]] <- plot
    }
    value <- try(builder(i), silent = TRUE)
    if (jaspBase::isTryError(value))
      plot$setError(jaspBase::.extractErrorMessage(value))
    else
      plot$plotObject <- value
  }
  return()
}

.sargResidualPlots <- function(jaspResults, options, fits, quantiles, kind, option, position) {

  key <- switch(kind, time = "residualsVsTimePlot", predicted = "residualVsPredictedPlot", histogram = "residualHistogram")
  title <- switch(kind, time = gettext("Residuals vs. Time"), predicted = gettext("Residuals vs. Predicted"), histogram = gettext("Residual Histogram"))
  .sargDiagnosticSection(jaspResults, options, key, title, c(option, "residualPlotResidualType"), position, fits, function(i) {
    fit <- fits[[i]]
    type <- options[["residualPlotResidualType"]]
    residual <- .sargResidualValue(fit, type, quantiles[[i]])
    if (kind == "histogram")
      return(.saPlotCanvasPadding(jaspGraphs::jaspHistogram(residual, xName = .sargResidualLabel(type))))
    if (kind == "time") {
      x <- fit[["time"]]
      label <- gettext("Time")
    } else if (type == "regression") {
      x <- as.numeric(stats::fitted(fit[["native"]][["model"]]))
      label <- if (fit[["model"]] == "duane") gettext("Fitted Log Cumulative Mean Time Between Events") else gettext("Fitted Log Cumulative Events")
    } else {
      x <- .sargIntervalMean(fit)
      label <- gettext("Fitted Interval Events")
    }
    axisLabel <- paste(strwrap(label, width = 30L), collapse = "\n")
    return(.saPlotCanvasPadding(.saResidualsPlot(x, residual, axisLabel, .sargResidualLabel(type))))
  })
  return()
}

.sargProbabilityData <- function(fit, residuals, options) {

  if (jaspBase::isTryError(residuals))
    stop(jaspBase::.extractErrorMessage(residuals))
  n <- length(residuals)
  probability <- .saProbabilityPlottingPositions(seq_len(n), n, options[["probabilityPlotPlottingPosition"]])
  transform <- if (options[["probabilityPlotCanvas"]] == "normal") stats::qnorm else identity
  alpha <- (1 - options[["probabilityPlotReferenceEnvelopeLevel"]]) / 2
  return(data.frame(theoretical = transform(probability), observed = if (options[["probabilityPlotCanvas"]] == "normal")
    sort(residuals) else sort(stats::pnorm(residuals)),
    lower = transform(stats::qbeta(alpha, seq_len(n), n - seq_len(n) + 1)),
    upper = transform(stats::qbeta(1 - alpha, seq_len(n), n - seq_len(n) + 1)), subgroup = fit[["label"]]))
}

.sargProbabilityPlot <- function(jaspResults, options, fits, quantiles) {

  if (!is.null(jaspResults[["probabilityPlot"]]))
    return()
  groups <- if (options[["probabilityPlotMergePlotsAcrossSubgroups"]]) list(seq_along(fits)) else as.list(seq_along(fits))
  selected <- lapply(groups, function(indices) list(label = if (length(indices) == 1L) fits[[indices]][["label"]] else ""))
  .sargDiagnosticSection(jaspResults, options, "probabilityPlot", gettext("Probability Plot"), .sargProbabilityDependencies, 6.4, selected, function(i) {
    indices <- groups[[i]]
    data <- do.call(rbind, lapply(indices, function(index) .sargProbabilityData(fits[[index]], quantiles[[index]], options)))
    return(.sargProbabilityFigure(data, options))
  }, width = if (options[["probabilityPlotMergePlotsAcrossSubgroups"]] && length(fits) > 1L) 650 else 520, height = 420)
  jaspResults[["probabilityPlot"]]$addCitation("Dunn, P. K., & Smyth, G. K. (1996). Randomized quantile residuals. Journal of Computational and Graphical Statistics, 5(3), 236-244. https://doi.org/10.1080/10618600.1996.10474708")
  return()
}

.sargProbabilityFigure <- function(data, options) {

  data[["subgroup"]] <- factor(data[["subgroup"]], levels = unique(data[["subgroup"]]))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = theoretical, y = observed, colour = subgroup, group = subgroup))
  if (!options[["probabilityPlotEmpiricalPoints"]] && !options[["probabilityPlotReferenceEnvelope"]])
    plot <- plot + ggplot2::geom_blank()
  if (options[["probabilityPlotReferenceEnvelope"]])
    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = subgroup),
      alpha = .15, colour = NA, show.legend = FALSE)
  if (options[["probabilityPlotReferenceLine"]])
    plot <- plot + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed")
  if (options[["probabilityPlotEmpiricalPoints"]]) {
    plot <- plot + jaspGraphs::geom_point()
    if (options[["probabilityPlotEmpiricalPointsCoordinates"]])
      plot <- plot + ggplot2::geom_text(ggplot2::aes(label = sprintf("(%.3f, %.3f)", theoretical, observed)),
        vjust = -.8, size = 2.5, show.legend = FALSE)
  }
  colourScale <- jaspGraphs::scale_JASPcolor_discrete(options[["probabilityPlotColorPalette"]])
  fillScale   <- jaspGraphs::scale_JASPfill_discrete(options[["probabilityPlotColorPalette"]])
  colourScale$palette <- .sapPlotPalette(colourScale$palette, nlevels(data[["subgroup"]]))
  fillScale$palette   <- .sapPlotPalette(fillScale$palette, nlevels(data[["subgroup"]]))
  plot <- plot + colourScale + fillScale
  uniform <- options[["probabilityPlotCanvas"]] == "uniform"
  plot <- plot + ggplot2::labs(x = if (uniform) gettext("Theoretical Probability") else gettext("Theoretical Normal Quantile"),
    y = if (uniform) gettext("Observed Probability") else gettext("Observed Quantile Residual"), colour = gettext("Subgroup"), fill = gettext("Subgroup"),
    caption = if (options[["probabilityPlotReferenceEnvelope"]]) gettext("Pointwise reference envelope; fitted-parameter uncertainty excluded.") else NULL)
  if (uniform)
    plot <- plot + ggplot2::scale_x_continuous(limits = c(0, 1)) + ggplot2::scale_y_continuous(limits = c(0, 1))
  plot <- .sapProbabilityPlotAddTheme(plot, options)
  if (nlevels(data[["subgroup"]]) == 1L)
    plot <- plot + ggplot2::theme(legend.position = "none")
  return(.sapPredictionPrettyAxes(plot, quantileAxis = uniform, probabilityAxis = uniform))
}
