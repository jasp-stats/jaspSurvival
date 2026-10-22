# Standardize responses over observed profiles, retaining their joint distribution.
# flexsurv 2.3.2 standsurv is not a replacement for this shared-draw path:
# identical at scenarios gave bootstrap ratio CI [.89696, 1.12995] instead of [1, 1];
# bootstrap cl=.8 looks for hardcoded 2.5%/97.5% columns, and partially fixed
# delta intervals have a dimension mismatch. With main ~x and shape ~z,
# at(x=value) can collapse unweighted z profiles to the first row, changing
# predictions when rows reverse. Keep common draws, one-sided bounds and log tails.
.sapPopulationDesigns <- function(fit, data, options, used) {

  training  <- attr(fit, "dataset")
  variables <- .sapModelVariables(attr(fit, "modelTerms"))
  assigned  <- intersect(variables, used)
  weights   <- .sapCaseWeights(training, options)
  return(lapply(seq_len(nrow(data)), function(i) {
    if (fit[["ncovs"]] == 0L)
      return(list(X = matrix(numeric(0), 1L, 0L), weights = 1))
    profiles <- training[, variables, drop = FALSE]
    for (variable in assigned)
      profiles[[variable]] <- rep(data[[variable]][i], nrow(profiles))
    for (transformations in .sapModelTransformations(attr(fit, "modelTerms")))
      for (variable in names(transformations))
        if (any(.sapTransformationInvalid(as.numeric(as.character(profiles[[variable]])), transformations[[variable]])))
          stop(gettextf("The prediction value of %1$s is outside its transformation domain.", jaspBase::decodeColNames(variable)))
    X <- attr(.sapPredictionPointParameters(fit, newdata = profiles), "X")
    # Hexadecimal doubles distinguish exact design rows without rounding profiles.
    keys     <- apply(X, 1L, function(row) paste(sprintf("%a", row), collapse = "\r"))
    unique   <- !duplicated(keys)
    group    <- match(keys, keys[unique])
    mass     <- as.numeric(rowsum(weights, group, reorder = FALSE))
    positive <- mass > 0
    return(list(X = X[unique, , drop = FALSE][positive, , drop = FALSE], weights = mass[positive] / sum(mass)))
  }))
}

.sapPopulationLogProbability <- function(fit, parameters, time, lowerTail) {

  if (!identical(attr(fit, "family"), "weibull"))
    return(do.call(
      fit[["dfns"]][["p"]],
      c(list(time), as.list(as.data.frame(parameters)), fit[["aux"]], list(lower.tail = lowerTail, log.p = TRUE))
    ))
  components    <- if ("shape" %in% colnames(parameters)) 1L else sum(grepl("^shape[0-9]+$", colnames(parameters)))
  if (components > 1L) {
    probabilities <- .sapmStickBreaking(parameters[, paste0("v", seq_len(components - 1L)), drop = FALSE], nrow(parameters))
    logs          <- matrix(NA_real_, nrow(parameters), components)
    componentLogs <- logs
  }
  for (k in seq_len(components)) {
    suffix <- if ("shape" %in% colnames(parameters)) "" else k
    shape  <- parameters[, paste0("shape", suffix)]
    scale  <- parameters[, paste0("scale", suffix)]
    values <- stats::pweibull(time, shape, scale, lower.tail = lowerTail, log.p = TRUE)
    ratio  <- time / scale
    tail   <- which(is.finite(time) & time > 0 & is.finite(shape) & shape > 0 & is.finite(scale) & scale > 0 & (ratio == 0 | is.infinite(ratio)))
    # Native pweibull divides before taking powers; extreme ratios lose valid tails.
    logPower <- shape[tail] * (log(time[tail]) - log(scale[tail]))
    power    <- exp(logPower)
    values[tail] <- if (lowerTail) ifelse(power == 0, logPower, log(-expm1(-power))) else -power
    if (components == 1L)
      return(values)
    componentLogs[, k] <- values
    componentLogs[probabilities[, k] == 0, k] <- 0
    logs[, k] <- log(probabilities[, k]) + values
    logs[probabilities[, k] == 0, k] <- -Inf
  }
  result  <- .sapmLogSumExp(logs)
  nearOne <- which(!is.na(result) & result > -log(2))
  result[nearOne] <- log1p(rowSums(probabilities[nearOne, , drop = FALSE] * expm1(componentLogs[nearOne, , drop = FALSE])) /
    rowSums(probabilities[nearOne, , drop = FALSE]))
  return(pmin(result, 0))
}

.sapPopulationTotals <- function(fit, parameters, weights, time, density = FALSE, lowerTail = FALSE) {

  draws          <- nrow(parameters) / length(weights)
  arguments      <- c(list(rep(rep_len(time, draws), length(weights))), as.list(as.data.frame(parameters)), fit[["aux"]])
  probabilities  <- matrix(.sapPopulationLogProbability(fit, parameters, arguments[[1L]], lowerTail), draws)
  logProbability <- .sapmLogSumExp(sweep(probabilities, 2L, log(weights), "+"))
  # log1p/expm1 retain small departures from one when profiles nearly all survive.
  nearOne <- which(!is.na(logProbability) & logProbability > -log(2))
  logProbability[nearOne] <- log1p(as.vector(expm1(probabilities[nearOne, , drop = FALSE]) %*% weights) / sum(weights))
  logProbability <- pmin(logProbability, 0)
  logProbability[which(rep_len(time, draws) == 0)] <- if (lowerTail) -Inf else 0
  logDensity <- if (density) {
    values <- do.call(fit[["dfns"]][["d"]], c(arguments, list(log = TRUE)))
    .sapmLogSumExp(sweep(matrix(values, draws), 2L, log(weights), "+"))
  } else NULL
  return(list(logProbability = logProbability, logDensity = logDensity))
}

# Invert the weighted population distribution separately for each coefficient draw.
# A tested flexsurv 2.3.2 qgeneric adapter still needed explicit draw indexing,
# endpoint/cure screening, log-tail residual checks and failed-row retries;
# it offered neither less scaffolding nor a speed benefit on the tested draws.
.sapPopulationQuantile <- function(fit, parameters, weights, probability, initial = NULL) {

  draws <- nrow(parameters) / length(weights)
  if (probability == 0)
    return(rep(0, draws))
  if (probability == 1)
    return(rep(Inf, draws))

  lowerTail <- probability < .5
  target    <- if (lowerTail) log(probability) else log1p(-probability)
  lower     <- rep(log(.Machine$double.xmin * .Machine$double.eps), draws)
  upper     <- rep(log(.Machine$double.xmax), draws)
  anchor    <- attr(fit, "populationAnchor")
  current   <- rep(min(max(anchor, lower[1L]), upper[1L]), draws)
  tight     <- rep(FALSE, draws)
  selectParameters <- function(rows)
    parameters[as.vector(outer(rows, (seq_along(weights) - 1L) * draws, `+`)), , drop = FALSE]

  # A population quantile lies between its smallest and largest profile
  # quantiles. Finite Weibull log quantiles also establish that no cure or
  # floating-point endpoint check is needed for these draws.
  if (identical(attr(fit, "family"), "weibull") && "shape" %in% colnames(parameters)) {
    logQuantiles <- matrix(log(parameters[, "scale"]) + log(-log1p(-probability)) / parameters[, "shape"], draws)
    minimum <- apply(logQuantiles, 1L, min)
    maximum <- apply(logQuantiles, 1L, max)
    tight <- is.finite(minimum) & is.finite(maximum) & minimum > lower + 1e-8 & maximum < upper - 1e-8
    lower[tight] <- minimum[tight] - 1e-8
    upper[tight] <- maximum[tight] + 1e-8
    current[tight] <- as.vector(logQuantiles[tight, , drop = FALSE] %*% weights)
  }

  result <- rep(NA_real_, draws)
  active <- tight
  rows   <- which(!tight)
  if (length(rows)) {
    part     <- selectParameters(rows)
    cure     <- .sapPopulationTotals(fit, part, weights, Inf)$logProbability
    smallest <- .sapPopulationTotals(fit, part, weights, exp(lower[rows]), lowerTail = lowerTail)$logProbability
    largest  <- .sapPopulationTotals(fit, part, weights, exp(upper[rows]), lowerTail = lowerTail)$logProbability
    value    <- rep(NA_real_, length(rows))
    value[which(probability >= -expm1(cure))] <- Inf
    if (lowerTail) {
      value[which(target >= log(-expm1(cure)) | largest < target)] <- Inf
      value[which(smallest > target)] <- 0
    } else {
      value[which(target <= cure | largest > target)] <- Inf
      value[which(smallest < target)] <- 0
    }
    result[rows] <- value
    active[rows] <- is.na(value) & !is.na(smallest) & !is.na(largest)
  }
  # Consecutive percentiles share exact solved values and inverse-CDF slopes.
  # This supplies only a starting point; every result meets the same root test.
  if (!is.null(initial)) {
    step <- initial$logTime + (probability - initial$probability) * exp(-initial$logTime - initial$logDensity)
    valid <- is.finite(step) & step > lower & step < upper
    current[valid] <- step[valid]
  }
  solvedDensity <- rep(NA_real_, draws)
  for (iteration in seq_len(100L)) {
    if (!any(active))
      break
    rows       <- which(active)
    evaluated  <- .sapPopulationTotals(fit, selectParameters(rows), weights, exp(current[rows]), density = TRUE, lowerTail = lowerTail)
    difference <- correction <- rep(NA_real_, draws)
    difference[rows] <- evaluated$logProbability - target
    correction[rows] <- difference[rows] * exp(evaluated$logProbability - evaluated$logDensity - current[rows])
    accurate <- active & !is.na(difference) & abs(difference) <= 1e-9 &
      ((is.finite(correction) & abs(correction) <= 1e-10) | upper - lower <= 1e-10)
    result[which(accurate)] <- exp(current[which(accurate)])
    solvedDensity[rows[accurate[rows]]] <- evaluated$logDensity[accurate[rows]]
    active <- active & !accurate & !is.na(difference)
    if (!any(active))
      break
    below <- which(active & if (lowerTail) difference < 0 else difference > 0)
    above <- which(active & if (lowerTail) difference >= 0 else difference <= 0)
    lower[below] <- current[below]
    upper[above] <- current[above]
    step     <- current + (if (lowerTail) -1 else 1) * correction
    valid    <- is.finite(step) & step > lower & step < upper
    nextTime <- ifelse(valid, step, (lower + upper) / 2)
    active   <- active & nextTime != current
    current  <- nextTime
  }
  attr(result, "populationQuantileState") <- list(logTime = log(result), logDensity = solvedDensity, probability = probability)
  return(result)
}

.sapPopulationMeans <- function(fit, parameters) {

  name     <- attr(fit, "family")
  infinite <- rep(FALSE, nrow(parameters))
  if (length(name) && name %in% c("llogis", "gengamma", "genf", "genf.orig")) {
    family        <- .sapmFamily(name)
    components    <- attr(fit, "components")
    probabilities <- .sapmStickBreaking(
      if (components > 1L) parameters[, paste0("v", seq_len(components - 1L)), drop = FALSE],
      nrow(parameters)
    )
    for (k in seq_len(components)) {
      suffix    <- if (family[["pars"]][1L] %in% colnames(parameters)) "" else k
      part      <- stats::setNames(as.list(as.data.frame(parameters[, paste0(family[["pars"]], suffix), drop = FALSE])), family[["pars"]])
      divergent <- switch(name,
        llogis    = part$shape > 0 & part$shape <= 1,
        gengamma  = 1 + part$sigma * part$Q <= 0,
        genf.orig = part$s2 <= part$sigma,
        genf      = {
          delta     <- sqrt(part$Q^2 + 2 * part$P)
          threshold <- ifelse(part$Q >= 0, (delta + part$Q) / part$P, 2 / (delta - part$Q))
          threshold[which(part$P == 0 & part$Q >= 0)] <- Inf
          part$sigma >= threshold
        })
      infinite[which(divergent & probabilities[, k] > 0)] <- TRUE
    }
  }
  # Positive lifetime mixtures have infinite mean if any contributing mean diverges.
  values <- rep(Inf, nrow(parameters))
  finite <- which(!infinite)
  if (length(finite))
    values[finite] <- do.call(.sapNativePredictionFunction(fit, "mean"),
      c(list(t = 0, start = 0), as.list(as.data.frame(parameters[finite, , drop = FALSE])), fit[["aux"]]))
  return(values)
}

# Aggregate response quantities over observed profiles using their case weights.
.sapPopulationValues <- function(fit, parameters, weights, type, at) {

  draws <- nrow(parameters) / length(weights)
  if (type == "mean") {
    values <- .sapPopulationMeans(fit, parameters)
    return(matrix(as.vector(matrix(values, draws) %*% weights), draws, length(at)))
  }
  if (type %in% c("quantile", "median")) {
    probabilities <- if (type == "median") rep(.5, length(at)) else at
    values <- matrix(NA_real_, draws, length(at))
    initial <- NULL
    for (i in order(probabilities)) {
      value <- .sapPopulationQuantile(fit, parameters, weights, probabilities[i], initial)
      values[, i] <- value
      initial <- attr(value, "populationQuantileState")
    }
    return(values)
  }
  return(vapply(at, function(time) {
    if (type == "rmst") {
      values <- do.call(
        fit[["dfns"]][["rmst"]],
        c(list(t = rep(time, nrow(parameters)), start = 0), as.list(as.data.frame(parameters)), fit[["aux"]])
      )
      return(as.vector(matrix(values, draws) %*% weights))
    }
    totals <- .sapPopulationTotals(fit, parameters, weights, time, density = type == "hazard", lowerTail = type == "failure")
    return(switch(type, survival = exp(totals$logProbability), failure = exp(totals$logProbability),
      cumhaz = -totals$logProbability, hazard = exp(totals$logDensity - totals$logProbability)))
  }, numeric(draws)))
}

.sapPopulationDrawValues <- function(fit, point, type, rawsim, transform = identity) {

  populations <- attr(point, "population")
  draws       <- nrow(rawsim)
  times       <- lapply(point, function(row) if (type %in% c("mean", "median")) rep(0, nrow(row)) else row[[1L]])
  values      <- matrix(NA_real_, draws, sum(lengths(times)))
  column      <- 0L
  for (i in seq_along(populations)) {
    population <- populations[[i]]
    # Bound temporary profile-by-draw parameter storage; never subsample units.
    block <- max(1L, floor(250000 / (nrow(population$X) * length(fit[["dlist"]][["pars"]]))))
    for (first in seq.int(1L, draws, by = block)) {
      selected   <- seq.int(first, min(draws, first + block - 1L))
      parameters <- .sapPredictionParameters(fit, population$X, rawsim[selected, , drop = FALSE])
      values[selected, column + seq_along(times[[i]])] <- transform(.sapPopulationValues(fit, parameters, population$weights, type, times[[i]]))
    }
    column <- column + length(times[[i]])
  }
  return(values)
}

.sapPopulationPointSummary <- function(fit, type, at) {

  point <- lapply(
    attr(fit, "predictionPopulation"),
    function(population) data.frame(time = at, est = NA_real_, lcl = NA_real_, ucl = NA_real_)
  )
  attr(point, "population") <- attr(fit, "predictionPopulation")
  values <- .sapPopulationDrawValues(fit, point, type, .sapPredictionPointCoefficients(fit))
  for (i in seq_along(point))
    point[[i]][["est"]] <- values[1L, (i - 1L) * length(at) + seq_along(at)]
  return(point)
}

# Assemble population point estimates and bounds in the ordinary summary layout.
.sapPopulationSummary <- function(fit, options, type, at, fast = FALSE) {

  messages       <- character(0)
  captureWarning <- function(warning) {
    messages <<- c(messages, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
  point <- withCallingHandlers(.sapPopulationPointSummary(fit, type, at), warning = captureWarning)

  # Point estimates and uncertainty both average over the same weighted population.
  ci    <- options[["predictionsDisplayConfidenceIntervals"]] && !fast
  if (ci && .sapUsableCovariance(fit)) {
    B      <- options[["confidenceIntervalSimulationDraws"]]
    draws  <- .sapJointDraws(fit, B, if (options[["setSeed"]]) options[["seed"]], options[["simulationContext"]])
    bounds <- withCallingHandlers(.sapPredictionBounds(fit, point, type, NULL, 0, B, draws, identity, options[["simulationContext"]],
      options[["predictionsDisplayConfidenceIntervalsLevel"]], options[["confidenceIntervalBounds"]]), warning = captureWarning)
    messages <- c(messages, attr(bounds, "predictionWarnings"))
    for (i in seq_along(point)) {
      columns <- (i - 1L) * length(at) + seq_along(at)
      point[[i]][["lcl"]] <- bounds["lower", columns]
      point[[i]][["ucl"]] <- bounds["upper", columns]
    }
  } else if (ci && fit[["npars"]] == 0L && !.sapConstraintActive(fit)) {
    for (i in seq_along(point))
      for (field in .sapBoundFields(options[["confidenceIntervalBounds"]]))
        point[[i]][[field]] <- point[[i]][["est"]]
  }
  if (ci && !.sapConstraintActive(fit) && anyNA(unlist(lapply(point, function(row) row[.sapBoundFields(options[["confidenceIntervalBounds"]])]))))
    messages <- c(messages, gettext("Some confidence intervals could not be evaluated and are shown as missing."))
  if (anyNA(unlist(lapply(point, `[[`, "est"))))
    messages <- c(messages, gettext("Some population predictions could not be evaluated and are shown as missing."))
  if (any(is.infinite(unlist(lapply(point, `[[`, "est")))))
    messages <- c(messages, gettext("Some requested quantities are infinite or exceed numerical range."))
  attr(point, "predictionWarnings") <- unique(messages)
  return(point)
}
