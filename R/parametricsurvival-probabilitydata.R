# Probability diagnostics map failures and censoring bounds to each stratum's
# frequency-weighted reference model design.

.sapProbabilityPlotReferenceTimes <- function(fit, logSurvival, reference, time) {

  # Lower-tail log probabilities avoid native wrappers that mishandle upper-tail
  # log quantiles. Keep both tails accurate when converting the probability.
  logFailure    <- logSurvival
  smallSurvival <- logSurvival < -log(2)
  logFailure[smallSurvival] <- log1p(-exp(logSurvival[smallSurvival]))
  logFailure[!smallSurvival] <- log(-expm1(logSurvival[!smallSurvival]))
  adjusted <- suppressWarnings(do.call(fit[["dfns"]][["q"]], c(list(logFailure),
    as.list(as.data.frame(reference)), list(lower.tail = TRUE, log.p = TRUE), fit[["aux"]])))
  mapped <- suppressWarnings(do.call(fit[["dfns"]][["p"]], c(list(adjusted),
    as.list(as.data.frame(reference)), list(lower.tail = FALSE, log.p = TRUE), fit[["aux"]])))
  inaccurate <- !is.finite(adjusted) | is.na(mapped) | abs(mapped - logSurvival) > 1e-10 * (1 + abs(logSurvival))
  inaccurate[is.na(inaccurate)] <- TRUE
  # Native quantile wrappers can round a valid extreme probability to one.
  # Refine only affected rows on log time using the fitted log-survival function.
  for (row in which(inaccurate & is.finite(time) & time > 0)) {
    parameters <- as.list(as.data.frame(reference[row, , drop = FALSE]))
    target     <- logSurvival[row]
    adjusted[row] <- NA_real_
    at <- function(value) do.call(fit[["dfns"]][["p"]], c(list(value), parameters,
      list(lower.tail = FALSE, log.p = TRUE), fit[["aux"]]))
    limit <- suppressWarnings(at(Inf))
    if (!is.finite(target) || (!is.na(limit) && target <= limit))
      next
    evaluate <- function(value) at(exp(value)) - target
    lower    <- upper <- log(time[row])
    minimum  <- log(.Machine$double.xmin) + log(.Machine$double.eps)
    maximum  <- log(.Machine$double.xmax) - log(2)
    step     <- 1
    for (iteration in seq_len(16L)) {
      low  <- suppressWarnings(evaluate(lower))
      high <- suppressWarnings(evaluate(upper))
      if (is.na(low) || is.na(high))
        break
      if (low == 0) {
        adjusted[row] <- exp(lower)
        break
      }
      if (high == 0) {
        adjusted[row] <- exp(upper)
        break
      }
      if (low >= 0 && high <= 0) {
        adjusted[row] <- exp(stats::uniroot(evaluate, c(lower, upper), f.lower = low, f.upper = high, tol = 1e-10)$root)
        break
      }
      if (low < 0)
        lower <- max(minimum, lower - step)
      if (high > 0)
        upper <- min(maximum, upper + step)
      step <- step * 2
    }
  }
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
    adjustedTime <- .sapProbabilityPlotReferenceTimes(fit, logSurvival, reference, time)
  }
  positive <- is.finite(time) & time > 0
  if (any(!is.finite(adjustedTime[positive]) | adjustedTime[positive] <= 0)) {
    message <- gettext("Some observations cannot be mapped to positive, finite times at the automatic reference profile. Stratify by the predictors associated with cure fractions or extreme tails, or simplify the model.")
    stop(structure(list(message = message, call = NULL), class = c("probabilityPlotError", "error", "condition")))
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
