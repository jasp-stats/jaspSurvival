# Probability diagnostics map failures and censoring bounds to each stratum's
# frequency-weighted reference model design.

.sapProbabilityPlotReferenceTimes <- function(fit, logSurvival, reference) {

  logFailure <- ifelse(logSurvival < -log(2), log1p(-exp(logSurvival)), log(-expm1(logSurvival)))
  evaluate <- function(rows) tryCatch({
    parameters <- as.list(as.data.frame(reference[rows, , drop = FALSE]))
    adjusted <- suppressWarnings(do.call(fit[["dfns"]][["q"]], c(list(logFailure[rows]), parameters,
      list(lower.tail = TRUE, log.p = TRUE), fit[["aux"]])))
    mapped <- suppressWarnings(do.call(fit[["dfns"]][["p"]], c(list(adjusted), parameters,
      list(lower.tail = FALSE, log.p = TRUE), fit[["aux"]])))
    accurate <- is.finite(adjusted) & !is.na(mapped) &
      abs(mapped - logSurvival[rows]) <= 1e-10 * (1 + abs(logSurvival[rows]))
    adjusted[which(is.na(accurate) | !accurate)] <- NA_real_
    adjusted
  }, error = function(error) rep(NA_real_, length(rows)))
  adjusted <- evaluate(seq_along(logSurvival))
  # Retry native scalar calls only; never reconstruct a failed quantile locally.
  if (length(adjusted) > 1L)
    for (row in which(is.na(adjusted)))
      adjusted[row] <- evaluate(row)
  return(adjusted)
}

.sapProbabilityPlotDisplayDataset <- function(dataset, fit, options) {

  if (fit[["ncoveffs"]] == 0L)
    return(dataset)
  if (is.null(attr(fit, "probabilityPlotStrata")))
    fit <- .sapProbabilityPlotPrepareFits(list(fit), options)[[1L]]
  design <- stats::model.matrix(fit)
  rows   <- match(rownames(dataset), rownames(design))
  stopifnot(!anyNA(rows), ncol(design) == fit[["ncoveffs"]])
  design     <- design[rows, , drop = FALSE]
  individual <- .sapPredictionParameters(fit, design, .sapPredictionPointCoefficients(fit))
  reference  <- .sapPredictionParameters(fit, attr(fit, "predictionX"), .sapPredictionPointCoefficients(fit))
  parameters <- fit[["dlist"]][["pars"]]
  time       <- .saExtractSurvTimes(dataset, options)
  groupIndex <- match(attr(fit, "probabilityPlotMembership"), names(attr(fit, "probabilityPlotStrata")))
  reference  <- reference[groupIndex, parameters, drop = FALSE]
  if (all(abs(individual - reference) <= 1e-12 * pmax(1, abs(reference))))
    return(dataset)

  name            <- fit[["dlist"]][["name"]]
  location        <- fit[["dlist"]][["location"]]
  ancillary       <- setdiff(parameters, location)
  commonAncillary <- all(abs(individual[, ancillary, drop = FALSE] - reference[, ancillary, drop = FALSE]) <=
    1e-12 * pmax(1, abs(reference[, ancillary, drop = FALSE])))
  aft <- name %in% c("exp", "weibull", "gamma", "lnorm", "llogis", "gengamma", "gengamma.orig", "genf", "genf.orig")
  if (aft && commonAncillary) {
    # Exact AFT adjustment also preserves observations beyond native CDF tail
    # precision. It is equivalent to quantile matching for common ancillary terms.
    offset <- if (location %in% c("mu", "meanlog")) reference[, location] - individual[, location] else
      if (location == "rate") log(individual[, location]) - log(reference[, location]) else
        log(reference[, location]) - log(individual[, location])
    adjustedTime <- exp(log(time) + offset)
  } else {
    logSurvival <- do.call(fit[["dfns"]][["p"]], c(list(time), as.list(as.data.frame(individual[, parameters, drop = FALSE])),
      list(lower.tail = FALSE, log.p = TRUE), fit[["aux"]]))
    adjustedTime <- .sapProbabilityPlotReferenceTimes(fit, logSurvival, reference)
  }
  positive <- is.finite(time) & time > 0
  missing <- positive & (!is.finite(adjustedTime) | adjustedTime <= 0)
  if (any(missing)) {
    # Keep the full cohort's probability ordering for empirical ranks, even
    # when the inverse CDF cannot provide a displayed time for some rows.
    logSurvival <- do.call(fit[["dfns"]][["p"]], c(list(time), as.list(as.data.frame(individual[, parameters, drop = FALSE])),
      list(lower.tail = FALSE, log.p = TRUE), fit[["aux"]]))
    attr(dataset, "probabilityPlotRankTime") <- stats::setNames(-logSurvival, rownames(dataset))
    attr(dataset, "probabilityPlotOriginalTime") <- stats::setNames(time, rownames(dataset))
    attr(dataset, "probabilityPlotWarnings") <- gettext("Some observed times could not be mapped to the reference profile and are omitted from this diagnostic. Available points and fitted curves are retained.")
    if (anyNA(logSurvival[positive]))
      attr(dataset, "probabilityPlotWarnings") <- c(attr(dataset, "probabilityPlotWarnings"),
        gettext("Empirical points for affected strata are unavailable because their fitted probabilities could not be evaluated. Fitted curves are retained where available."))
    adjustedTime[missing] <- NA_real_
  }
  # The same increasing transformation applies to failures and censoring bounds.
  dataset[[options[["timeToEvent"]]]] <- adjustedTime
  attr(dataset, "probabilityPlotAdjusted") <- TRUE
  return(dataset)
}

.sapProbabilityPlotPredictions <- function(fit, options, times, ci) {

  arguments <- list(type = "survival", t = times, ci = ci, cl = options[["probabilityPlotConfidenceIntervalsLevel"]],
    B         = options[["confidenceIntervalSimulationDraws"]], seed = if (options[["setSeed"]]) options[["seed"]],
    direction = options[["confidenceIntervalBounds"]], context = options[["simulationContext"]], transform = function(value) 1 - value)
  labels <- attr(fit, "predictionLabels")
  if (fit[["ncoveffs"]] == 0L && length(labels) > 1L) {
    # Native intercept-only summaries ignore X; repeat their single curve for
    # the shared observed strata when merging models with different predictors.
    attr(fit, "predictionX") <- attr(fit, "predictionLabels") <- NULL
    data   <- do.call(.sapSummaryPredictions, c(list(fit), arguments))
    result <- stats::setNames(rep(data, length(labels)), labels)
    attr(result, "predictionWarnings") <- attr(data, "predictionWarnings")
    return(result)
  }
  return(do.call(.sapSummaryPredictions, c(list(fit), arguments)))
}
