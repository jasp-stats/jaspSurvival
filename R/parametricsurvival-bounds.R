.sapBoundProbabilities <- function(level, direction) {

  if (direction == "twoSided")
    return(c(lower = (1 - level) / 2, upper = (1 + level) / 2))
  if (direction == "lower")
    return(c(lower = 1 - level))
  return(c(upper = level))
}

.sapBoundCritical <- function(level, direction) {

  return(stats::qnorm(if (direction == "twoSided") (1 + level) / 2 else level))
}

.sapBoundTitle <- function(level, direction) {

  return(switch(direction,
    twoSided = gettextf("%1$s%% CI", 100 * level),
    lower    = gettextf("%1$s%% Lower Bound", 100 * level),
    upper    = gettextf("%1$s%% Upper Bound", 100 * level)))
}

.sapBoundFields <- function(direction, lower = "lcl", upper = "ucl") {

  return(if (direction == "twoSided") c(lower, upper) else if (direction == "lower") lower else upper)
}

.sapPlotBoundData <- function(data, direction) {

  # One-sided error bars run from the estimate to the available bound.
  data[["boundLower"]] <- if (direction == "upper") data[["estimate"]] else data[["lCi"]]
  data[["boundUpper"]] <- if (direction == "lower") data[["estimate"]] else data[["uCi"]]
  return(data)
}

.sapAddBoundColumns <- function(table, level, direction, lower = "lower", upper = "upper", overtitle = NULL) {

  if (is.null(overtitle))
    overtitle <- .sapBoundTitle(level, direction)
  if (direction != "upper")
    table$addColumnInfo(name = lower, title = gettext("Lower"), type = "number", overtitle = overtitle)
  if (direction != "lower")
    table$addColumnInfo(name = upper, title = gettext("Upper"), type = "number", overtitle = overtitle)
}

.sapAddModelBoundLayer <- function(plot, data, level, direction, x = "at", lower = "lCi", upper = "uCi",
  groups = character(0), color = NULL, fill = NULL, defaultFill = NULL, alpha = .30, twoSidedMapping = NULL, na.rm = FALSE) {
  fields    <- .sapBoundFields(direction, lower, upper)
  available <- if (direction == "twoSided") any(is.finite(data[[lower]]) & is.finite(data[[upper]])) else any(is.finite(data[[fields]]))
  if (!available)
    return(plot)
  if (direction != "twoSided" || is.null(twoSidedMapping))
    data[[".boundGroup"]] <- if (length(groups)) interaction(lapply(data[groups], factor, exclude = NULL), drop = TRUE) else 1L
  if (direction == "twoSided") {
    mapping <- list(x = as.name(x), ymin = as.name(lower), ymax = as.name(upper),
      group = as.name(".boundGroup"), fill = if (!is.null(fill)) as.name(fill))
    if (!is.null(twoSidedMapping))
      mapping <- twoSidedMapping
    arguments <- list(
      mapping = do.call(ggplot2::aes, mapping[!vapply(mapping, is.null, logical(1))]),
      data    = data,
      colour  = NA,
      alpha   = alpha,
      na.rm   = na.rm
    )
    if (is.null(fill) && !is.null(defaultFill))
      arguments[["fill"]] <- defaultFill
    return(plot + do.call(ggplot2::geom_ribbon, arguments))
  }
  mapping   <- list(x = as.name(x), y = as.name(fields), group = as.name(".boundGroup"), color = if (!is.null(color)) as.name(color))
  arguments <- list(mapping = do.call(ggplot2::aes, mapping[!vapply(mapping, is.null, logical(1))]),
    data = data, linetype = 2L, alpha = .7, show.legend = FALSE)
  if (is.null(color))
    arguments[["colour"]] <- "grey40"
  return(plot + do.call(jaspGraphs::geom_line, arguments) + ggplot2::labs(caption = .sapBoundTitle(level, direction)))
}

.sapUsableCovariance <- function(fit) {

  return(!.sapConstraintActive(fit) && fit[["npars"]] > 0L && !is.null(dim(fit[["cov"]])) &&
    identical(dim(fit[["cov"]]), c(as.integer(fit[["npars"]]), as.integer(fit[["npars"]]))) && all(is.finite(fit[["cov"]])))
}

.sapFitSimulationSignature <- function(fit) {

  return(list(names = rownames(fit[["res.t"]]), coefficients = fit[["res.t"]][, "est"],
    free = fit[["optpars"]], freeCoefficients = fit[["opt"]][["par"]], fixed = fit[["fixedpars"]], covariance = fit[["cov"]]))
}

.sapFitSimulationKey <- function(fit) {

  return(paste(attr(fit, "subgroup"), attr(fit, "family"), attr(fit, "components"), attr(fit, "modelId"), sep = "\r"))
}

# Persist the simulation seed; keep draw and evaluation caches within this invocation.
.sapSimulationContext <- function(jaspResults, options) {

  if (is.null(jaspResults[["simulationState"]])) {
    state <- createJaspState()
    state$dependOn(c("setSeed", "seed"))
    state$object <- list(seed = if (options[["setSeed"]]) options[["seed"]] else NULL)
    jaspResults[["simulationState"]] <- state
  }

  # Cached arrays are deliberately transient; only the seed belongs in jaspResults.
  context <- new.env(parent = emptyenv())
  context$state <- jaspResults[["simulationState"]]
  context$records <- context$state$object
  context$draws <- context$conditional <- context$evaluations <- list()
  return(context)
}

.sapReleaseSimulationContext <- function(context) {

  # Plot quosures may retain an output builder's options environment.
  # Empty transient storage before the invocation is serialized by the engine.
  context$draws <- context$conditional <- context$evaluations <- NULL
  context$state <- context$records <- NULL
}

# Reuse joint coefficient draws so outputs and reference ratios share uncertainty.
.sapJointDraws <- function(fit, B, seed, context) {

  signature <- .sapFitSimulationSignature(fit)
  key       <- .sapFitSimulationKey(fit)
  if (!is.null(context)) {
    current <- context$draws[[key]]
    if (!is.null(current) && identical(current$signature, signature) && identical(current$B, B))
      return(current$rawsim)
    if (is.null(context$records$seed))
      context$records$seed <- sample.int(.Machine$integer.max, 1L)
    seed <- context$records$seed
    context$state$object <- context$records
  }

  # Restart from the shared seed when a model needs a fresh joint draw matrix.
  if (!is.null(seed))
    jaspBase::.setSeedJASP(list(setSeed = TRUE, seed = seed))
  rawsim <- flexsurv::normboot.flexsurvreg(fit, B = B, raw = TRUE, transform = TRUE)
  if (!is.null(context))
    context$draws[[key]] <- list(signature = signature, B = B, rawsim = rawsim)
  return(rawsim)
}

.sapNativePredictionFunction <- function(fit, type) {

  return(switch(type,
    survival = function(t, start, ...) {
      value <- exp(fit[["dfns"]][["p"]](t, ..., lower.tail = FALSE, log.p = TRUE) -
        fit[["dfns"]][["p"]](start, ..., lower.tail = FALSE, log.p = TRUE))
      value[t < start] <- 1
      value
    },
    failure = function(t, start, ...) {
      value <- if (all(start == 0)) fit[["dfns"]][["p"]](t, ...) else
        -expm1(fit[["dfns"]][["p"]](t, ..., lower.tail = FALSE, log.p = TRUE) -
          fit[["dfns"]][["p"]](start, ..., lower.tail = FALSE, log.p = TRUE))
      value[t < start] <- 0
      value
    },
    quantile = function(t, start, ...) {
      p <- fit[["dfns"]][["p"]](start, ...)
      fit[["dfns"]][["q"]](p + (1 - p) * t, ...)
    },
    median = function(t, start, ...) {
      p <- fit[["dfns"]][["p"]](start, ...)
      fit[["dfns"]][["q"]](p + (1 - p) / 2, ...)
    },
    mean = function(t, start, ...) {
      meanFunction <- if (fit[["dlist"]][["name"]] == "gompertz") .sapMeanGompertz else fit[["dfns"]][["mean"]]
      meanFunction(...)
    },
    hazard = function(t, start, ...) {
      value <- fit[["dfns"]][["h"]](t, ...)
      value[t < start] <- 0
      value
    },
    cumhaz = function(t, start, ...) {
      value <- fit[["dfns"]][["H"]](t, ...) - fit[["dfns"]][["H"]](start, ...)
      value[t < start] <- 0
      value
    },
    rmst = function(t, start, ...) fit[["dfns"]][["rmst"]](t, start = start, ...)))
}

# Return draws by prediction cells, with each scenario's times kept together.
.sapPredictionDrawValues <- function(fit, point, type, fn, start, B, rawsim, transform, context) {

  if (!is.null(attr(point, "population"))) {
    stopifnot(all(start == 0))
    return(.sapPopulationDrawValues(fit, point, type, rawsim, transform))
  }
  X         <- attr(point, "X")
  signature <- .sapFitSimulationSignature(fit)
  key       <- .sapFitSimulationKey(fit)
  times     <- lapply(point, function(row) if (type %in% c("mean", "median")) rep(0, nrow(row)) else row[[1L]])
  target    <- list(signature = signature, B = B, X = X, type = type, fn = fn, start = start, times = times, transform = transform)
  if (!is.null(context)) {
    cached <- context$evaluations[[key]]
    if (!is.null(cached) && identical(cached$target, target))
      return(cached$values)
    conditional <- context$conditional[[key]]
    if (!is.null(conditional) && identical(conditional$signature, signature) && identical(conditional$B, B) && identical(conditional$X, X))
      simulations <- conditional$values
    else
      simulations <- NULL
  } else
    simulations <- NULL

  # Project joint coefficient draws only when this scenario design is not cached.
  if (is.null(simulations)) {
    simulations <- flexsurv::normboot.flexsurvreg(fit, B = B, X = X, rawsim = rawsim, transform = FALSE)
    if (!is.null(context))
      context$conditional[[key]] <- list(signature = signature, B = B, X = X, values = simulations)
  }
  if (is.matrix(simulations))
    simulations <- list(simulations)
  if (is.null(fn))
    fn <- .sapNativePredictionFunction(fit, type)
  nx     <- length(point)
  nt     <- nrow(point[[1L]])
  shared <- identical(attr(fn, "sharedScenarioValue"), TRUE)
  values <- matrix(NA_real_, B, nt * if (shared) 1L else nx)
  # Keep every draw; blocking bounds temporary function argument memory only.
  block <- max(1L, min(nt, floor(250000 / (B * nx))))
  for (first in seq.int(1L, nt, by = block)) {
    selected   <- seq.int(first, min(nt, first + block - 1L))
    time       <- unlist(lapply(times, `[`, selected), use.names = FALSE)
    parameters <- do.call(rbind, lapply(simulations, function(simulation)
      simulation[rep(seq_len(B), length(selected)), fit[["dlist"]][["pars"]], drop = FALSE]))
    arguments <- c(list(t = rep(time, each = B), start = rep(rep_len(start, length(time)), each = B)),
      as.list(as.data.frame(parameters)), fit[["aux"]])
    result <- transform(do.call(fn, arguments))
    result <- matrix(result, nrow = B)
    if (shared)
      values[, selected] <- result[, seq_along(selected), drop = FALSE]
    else
      for (i in seq_len(nx))
        values[, (i - 1L) * nt + selected] <- result[, (i - 1L) * length(selected) + seq_along(selected), drop = FALSE]
  }
  attr(values, "sharedScenarioValue") <- shared
  if (!is.null(context))
    context$evaluations[[key]] <- list(target = target, values = values)
  return(values)
}

# Reduce joint draws to confidence bounds in blocks of scenarios and times.
.sapPredictionBounds <- function(fit, point, type, fn, start, B, rawsim, transform, context, level, direction, reference = NULL) {

  shared <- !is.null(fn) && identical(attr(fn, "sharedScenarioValue"), TRUE)
  nx     <- if (shared) 1L else length(point)
  nt     <- nrow(point[[1L]])
  bounds <- matrix(NA_real_, 2L, nx * nt, dimnames = list(c("lower", "upper"), NULL))
  failed <- evaluated <- 0L
  # Retain all draws, but calculate their quantiles before the next grid block.
  cells         <- max(250000L, B)
  rowsPerBlock  <- max(1L, min(nx, floor(cells / B)))
  timesPerBlock <- max(1L, min(nt, floor(cells / (B * rowsPerBlock))))
  subsetPoint   <- function(value, rows, times) {
    # Shared functions average scenarios jointly; bounds still need every design.
    if (shared)
      rows <- seq_along(value)
    part <- lapply(value[rows], function(frame) frame[times, , drop = FALSE])
    X    <- attr(value, "X")
    attr(part, "X") <- if (!is.null(X)) X[rows, , drop = FALSE]
    attr(part, "population") <- attr(value, "population")[rows]
    return(part)
  }
  for (first in seq.int(1L, nx, by = rowsPerBlock)) {
    rows <- seq.int(first, min(nx, first + rowsPerBlock - 1L))
    for (initial in seq.int(1L, nt, by = timesPerBlock)) {
      times     <- seq.int(initial, min(nt, initial + timesPerBlock - 1L))
      columns   <- as.vector(t(outer(rows - 1L, times, function(row, time) row * nt + time)))
      partStart <- if (length(start) == 1L) start else start[(columns - 1L) %% length(start) + 1L]
      part      <- subsetPoint(point, rows, times)
      values    <- .sapPredictionDrawValues(fit, part, type, fn, partStart, B, rawsim, transform, context)

      # Pair reference and scenario values draw by draw before taking ratio quantiles.
      if (!is.null(reference)) {
        baseline  <- subsetPoint(reference[["point"]], rows, times)
        numerator <- .sapPredictionDrawValues(reference[["fit"]], baseline, type, fn, partStart, B, rawsim, transform, context)
        values    <- numerator / values
      }
      failed    <- failed + sum(is.na(values))
      evaluated <- evaluated + length(values)
      bounds[, columns] <- .sapDrawBounds(values, level, direction)
    }
  }

  # Report failed evaluations without discarding them from the simulated distribution.
  messages <- character(0)
  if (failed > 0L)
    messages <- gettextf(
      "Some confidence bounds are missing because simulated predictions could not be evaluated (%1$i of %2$i evaluations).",
      failed,
      evaluated
    )
  if (any(is.infinite(bounds)))
    messages <- c(messages, gettext("Some confidence bounds are infinite or exceed numerical range."))
  if (length(messages))
    attr(bounds, "predictionWarnings") <- messages
  return(bounds)
}

.sapDrawBounds <- function(values, level, direction) {

  probabilities <- .sapBoundProbabilities(level, direction)
  bounds        <- matrix(NA_real_, 2L, ncol(values), dimnames = list(c("lower", "upper"), NULL))
  for (j in seq_len(ncol(values))) {
    # Omitting failed draws changes the simulated distribution and can shrink
    # its upper tail. Withhold affected bounds; retain valid infinite values.
    if (anyNA(values[, j]))
      next
    bounds[names(probabilities), j] <- stats::quantile(values[, j], probabilities, type = 7L, names = FALSE)
  }
  return(bounds)
}

.sapCoefficientBounds <- function(fit, level, direction) {

  bounds <- matrix(NA_real_, nrow(fit[["res.t"]]), 2L, dimnames = list(rownames(fit[["res.t"]]), c("lower", "upper")))
  if (!.sapUsableCovariance(fit))
    return(bounds)

  critical  <- .sapBoundCritical(level, direction)
  estimates <- fit[["res.t"]][, "est"]
  error     <- fit[["res.t"]][, "se"]
  for (side in .sapBoundFields(direction, "lower", "upper")) {
    value <- estimates + if (side == "lower") -critical * error else critical * error
    for (parameter in fit[["dlist"]][["pars"]]) {
      i <- match(parameter, rownames(fit[["res.t"]]))
      value[i] <- fit[["dlist"]][["inv.transforms"]][[match(parameter, fit[["dlist"]][["pars"]])]](value[i])
    }
    value[fit[["fixedpars"]]] <- NA_real_
    bounds[, side] <- value
  }
  return(bounds)
}
