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

# Component parameters and native mixture distribution functions.
.sapmOrderedInits               <- function(fit, family, components, link = FALSE) {

  inits <- fit[[if (link) "res.t" else "res"]][, "est"]

  # Fixed values identify components; median ordering would assign them to other components.
  if (length(setdiff(fit[["fixedpars"]], match(names(attr(fit, "structuralParameters")), rownames(fit[["res"]])))) > 0L)
    return(unname(inits))

  # order estimates before flexsurvreg constructs their covariance and confidence intervals
  estimates <- .sapmComponentEstimates(fit, family, components)
  order     <- .sapmComponentOrder(family, estimates[["base"]])
  if (components == 1 || identical(order, seq_len(components)))
    return(unname(inits))

  original <- inits
  for (k in seq_len(components))
    for (parameter in family[["pars"]]) {
      native <- paste0(parameter, k)
      source <- paste0(parameter, order[k])
      inits[native] <- original[source]
      targetIndex <- fit[["covpars"]][fit[["mx"]][[native]]]
      sourceIndex <- fit[["covpars"]][fit[["mx"]][[source]]]
      if (length(targetIndex) != length(sourceIndex))
        stop("Component equation designs differ during component reordering.")
      inits[targetIndex] <- original[sourceIndex]
    }
  weightIndex <- match(.sapmParameterNames(family, components)[["weightPars"]], names(inits))
  reordered <- .sapmReorderedWeights(fit[["res.t"]][weightIndex, "est"], order)
  inits[weightIndex] <- if (link) reordered else stats::plogis(reordered)

  return(unname(inits))
}

.sapmFixedNativeParameters      <- function(fixed) {

  return(unlist(lapply(seq_along(fixed), function(k) {
    if (length(fixed[[k]]) == 0L)
      return(numeric(0))
    return(stats::setNames(fixed[[k]], paste0(names(fixed[[k]]), k)))
  }), use.names = TRUE))
}

.sapmReorderedWeights           <- function(estimates, order) {

  # retain small remaining probabilities when re-expressing the fitted component order
  remaining     <- c(0, cumsum(stats::plogis(estimates, lower.tail = FALSE, log.p = TRUE)))
  probabilities <- (c(stats::plogis(estimates, log.p = TRUE), 0) + remaining)[order]

  return(vapply(seq_along(estimates), function(k) {
    probabilities[k] - .sapmLogSumExp(matrix(probabilities[seq.int(k + 1, length(probabilities))], nrow = 1))
  }, numeric(1)))
}

.sapmInits                      <- function(mixture, base, beta, probabilities, specification = NULL) {

  # flexsurvreg order: component parameters, stick-breaking weights, location effects of the first component, location effects of the remaining components
  components <- length(base)
  weights    <- numeric(0)
  remaining  <- 1
  for (k in seq_len(components - 1)) {
    weights   <- c(weights, min(max(probabilities[k] / remaining, 1e-6), 1 - 1e-6))
    remaining <- remaining - probabilities[k]
  }

  inits <- c(
    stats::setNames(unlist(base, use.names = FALSE), mixture[["componentPars"]]),
    stats::setNames(weights, mixture[["weightPars"]]),
    unlist(beta, use.names = FALSE)
  )
  if (!is.null(specification)) {
    slopes <- numeric(0)
    for (native in names(specification[["equations"]])) {
      equation <- specification[["equations"]][[native]]
      values   <- beta[[equation[["component"]]]]
      if (length(equation[["slopeNames"]]) == 0L)
        values <- numeric(0)
      else if (is.list(values))
        values <- values[[equation[["parameter"]]]]
      if (length(values) != length(equation[["slopeNames"]]))
        stop("Component starting coefficients differ from the native equation design.")
      slopes <- c(slopes, stats::setNames(as.numeric(values), equation[["slopeNames"]]))
    }
    inits <- c(inits[mixture[["dlist"]][["pars"]]], slopes)
    inits <- inits[specification[["parameterNames"]]]
  }
  return(inits)
}

.sapmComponentOrder             <- function(family, base) {

  # ascending baseline median lifetime, ties are broken by the remaining parameters
  medians <- vapply(base, function(x) .sapmComponentMedian(family, x), numeric(1))
  ties    <- lapply(seq_along(family[["pars"]]), function(i) vapply(base, function(x) x[[i]], numeric(1)))

  return(do.call(order, c(list(medians), ties)))
}

.sapmComponentMedian            <- function(family, base) {

  return(do.call(family[["q"]], c(list(0.5), as.list(base))))
}

.sapmParameterNames             <- function(family, components) {

  return(list(
    componentPars = as.vector(outer(family[["pars"]], seq_len(components), paste0)),
    weightPars    = if (components > 1) paste0("v", seq_len(components - 1)) else character(0)
  ))
}

.sapmBaseParameters             <- function(estimates, family, components) {

  # natural baseline parameters (covariates at zero) and mixing probabilities of the transformed estimates
  # of a fitted mixture, whose names carry the flexsurvreg order
  base <- lapply(seq_len(components), function(k) {
    stats::setNames(vapply(seq_along(family[["pars"]]), function(i) {
      family[["inv.transforms"]][[i]](estimates[[paste0(family[["pars"]][i], k)]])
    }, numeric(1)), family[["pars"]])
  })
  weights       <- .sapmParameterNames(family, components)[["weightPars"]]
  probabilities <- as.vector(.sapmStickBreaking(if (length(weights) > 0) matrix(stats::plogis(estimates[weights]), nrow = 1), 1))

  return(list(base = base, probabilities = probabilities))
}

.sapmComponentEstimates         <- function(fit, family, components) {

  estimates  <- fit[["res.t"]][, "est"]
  parameters <- .sapmBaseParameters(estimates, family, components)

  return(list(
    base          = parameters[["base"]],
    probabilities = parameters[["probabilities"]],
    beta          = lapply(seq_len(components), function(k) {
      index <- fit[["mx"]][[paste0(family[["location"]], k)]]
      if (length(index) == 0)
        return(numeric(0))
      return(estimates[fit[["covpars"]][index]])
    }),
    # exclude the location, whose scale changes with the time units
    collapsed     = .sapmConditionalMagnitude(fit, family, components)
  ))
}

.sapmObservationParameters <- function(fit, family, components) {

  base <- .sapmBaseParameters(fit[["res.t"]][, "est"], family, components)[["base"]]
  return(lapply(seq_len(components), function(k) {
    baseline <- stats::setNames(fit[["res.t"]][paste0(family[["pars"]], k), "est"], family[["pars"]])
    return(.sapmParameters(family, base[[k]], .sapEquationBetas(fit, family, k, components),
      .sapEquationMatrices(fit, family, k, components), baseline))
  }))
}

.sapmPosterior                  <- function(fit, family, components, survObject) {

  return(.sapmPosteriorProbabilities(
    family, survObject,
    .sapmObservationParameters(fit, family, components),
    .sapmComponentEstimates(fit, family, components)[["probabilities"]]
  ))
}

.sapmDuplicatedComponents       <- function(fit, family, components, survObject) {

  # components coincide if their distribution functions differ by less than one percentage point at every observed time,
  # such components cannot be distinguished by the data and their mixing probabilities are not identified
  if (components == 1)
    return(matrix(integer(0), ncol = 2))

  time         <- .sapmObservedTimes(survObject)
  parameters   <- .sapmObservationParameters(fit, family, components)
  distribution <- vapply(parameters, function(x) do.call(family[["p"]], c(list(time), x)), numeric(length(time)))
  distribution <- matrix(distribution, nrow = length(time))

  pairs <- t(utils::combn(components, 2))
  keep  <- apply(pairs, 1, function(pair) {
    difference <- abs(distribution[, pair[1]] - distribution[, pair[2]])
    difference <- difference[is.finite(difference)]
    return(length(difference) > 0 && max(difference) < 0.01)
  })

  return(pairs[keep, , drop = FALSE])
}

.sapmStickBreaking              <- function(weights, nObs) {

  # stick-breaking weights v1, ..., v(K-1) to mixing probabilities p1, ..., pK
  if (is.matrix(weights) && nrow(weights) == 0)
    return(matrix(numeric(0), nrow = 0, ncol = ncol(weights) + 1L))
  if (is.null(weights) || length(weights) == 0)
    return(matrix(1, nObs, 1))

  weights       <- as.matrix(weights)
  probabilities <- matrix(NA_real_, nrow(weights), ncol(weights) + 1)
  remaining     <- rep(1, nrow(weights))
  for (k in seq_len(ncol(weights))) {
    probabilities[, k] <- remaining * weights[, k]
    remaining          <- remaining - probabilities[, k]
  }
  probabilities[, ncol(weights) + 1] <- remaining

  return(probabilities)
}

.sapmMixtureDistribution        <- function(family, components) {

  names         <- .sapmParameterNames(family, components)
  componentPars <- names[["componentPars"]]
  weightPars    <- names[["weightPars"]]

  # flexsurv passes the parameters as scalars without covariates and as vectors with covariates
  splitArguments <- function(arguments, n) {
    n         <- max(n, lengths(arguments))
    arguments <- lapply(arguments, rep_len, length.out = n)
    return(list(
      n             = n,
      components    = lapply(seq_len(components), function(k) stats::setNames(arguments[paste0(family[["pars"]], k)], family[["pars"]])),
      probabilities = .sapmStickBreaking(if (components > 1) do.call(cbind, arguments[weightPars]), n)
    ))
  }
  dMixture <- function(x, ..., log = FALSE) {
    arguments <- splitArguments(list(...), length(x))
    x         <- rep_len(x, arguments[["n"]])
    logs      <- vapply(seq_len(components), function(k) {
      contribution <- log(arguments[["probabilities"]][, k]) + do.call(family[["d"]], c(list(x), arguments[["components"]][[k]], list(log = TRUE)))
      contribution[arguments[["probabilities"]][, k] == 0] <- -Inf
      return(contribution)
    }, numeric(arguments[["n"]]))
    out       <- .sapmLogSumExp(matrix(logs, nrow = arguments[["n"]]))
    return(if (log) out else exp(out))
  }
  pMixture <- function(q, ..., lower.tail = TRUE, log.p = FALSE) {
    arguments <- splitArguments(list(...), length(q))
    q         <- rep_len(q, arguments[["n"]])
    if (!log.p) {
      return(Reduce(`+`, lapply(seq_len(components), function(k) {
        arguments[["probabilities"]][, k] * do.call(family[["p"]], c(list(q), arguments[["components"]][[k]], list(lower.tail = lower.tail)))
      })))
    }
    logs      <- vapply(seq_len(components), function(k) {
      log(arguments[["probabilities"]][, k]) + do.call(family[["p"]], c(list(q), arguments[["components"]][[k]], list(lower.tail = lower.tail, log.p = TRUE)))
    }, numeric(arguments[["n"]]))
    return(.sapmLogSumExp(matrix(logs, nrow = arguments[["n"]])))
  }
  # flexsurv 2.3.2 defaults use 1 - p: rates 1/2, weight .5, t=50 give Inf
  # instead of h=1 and H=50.69314718. At rates 2/3 and t=1e17, log d - log S
  # also loses log(2). Combine native component log hazards, centering survival
  # before adding log weights so equal huge survivals retain their mixing weights.
  hMixture <- function(x, ...) {
    parameters <- list(...)
    arguments  <- splitArguments(parameters, length(x))
    n          <- arguments[["n"]]
    x          <- rep_len(x, n)
    if (n == 0L)
      return(numeric(0))

    logSurvival <- matrix(vapply(seq_len(components), function(k)
      do.call(family[["p"]], c(list(x), arguments[["components"]][[k]], list(lower.tail = FALSE, log.p = TRUE))), numeric(n)), nrow = n)
    activeSurvival <- logSurvival
    activeSurvival[arguments[["probabilities"]] == 0] <- -Inf
    maximum <- Reduce(pmax, lapply(seq_len(components), function(k) activeSurvival[, k]))
    rows    <- which(is.finite(x) & is.finite(maximum) & rowSums(is.na(logSurvival)) == 0)
    out     <- rep(NA_real_, n)

    # Retain invalid and nonfinite-time classifications rather than inventing limits.
    other <- setdiff(seq_len(n), rows)
    if (length(other)) {
      pars <- lapply(parameters, function(value) rep_len(value, n)[other])
      out[other] <- exp(do.call(dMixture, c(list(x[other]), pars, list(log = TRUE))) -
        do.call(pMixture, c(list(x[other]), pars, list(lower.tail = FALSE, log.p = TRUE))))
    }
    if (length(rows)) {
      logHazard <- matrix(vapply(seq_len(components), function(k) {
        pars <- lapply(arguments[["components"]][[k]], function(value) value[rows])
        if (!"log" %in% names(formals(family[["h"]])))
          return(do.call(family[["d"]], c(list(x[rows]), pars, list(log = TRUE))) - logSurvival[rows, k])
        value   <- do.call(family[["h"]], c(list(x[rows]), pars, list(log = TRUE)))
        missing <- which(!is.finite(value))
        # Native gamma hazards can form 0/0 while log density/survival stay finite.
        if (length(missing)) {
          difference <- do.call(family[["d"]], c(list(x[rows[missing]]), lapply(pars, function(value) value[missing]), list(log = TRUE))) -
            logSurvival[rows[missing], k]
          finite <- is.finite(difference)
          value[missing[finite]] <- difference[finite]
        }
        return(value)
      }, numeric(length(rows))), nrow = length(rows))
      mass      <- log(arguments[["probabilities"]][rows, , drop = FALSE]) + (logSurvival[rows, , drop = FALSE] - maximum[rows])
      numerator <- mass + logHazard
      numerator[mass == -Inf & !is.na(mass)] <- -Inf
      out[rows] <- exp(.sapmLogSumExp(numerator) - .sapmLogSumExp(mass))
    }
    return(out)
  }
  HMixture <- function(x, ...) {
    out <- -pMixture(x, ..., lower.tail = FALSE, log.p = TRUE)
    # Remove log-sum roundoff at nonpositive times without replacing invalid rows.
    out[which(rep_len(x, length(out)) <= 0 & is.finite(out))] <- 0
    return(out)
  }
  qMixture <- function(p, ..., lower.tail = TRUE, log.p = FALSE) {
    if (!length(p))
      return(numeric(0))
    logLower  <- if (log.p) if (lower.tail) p else log(-expm1(p)) else if (lower.tail) log(p) else log1p(-p)
    logUpper  <- if (log.p) if (lower.tail) log(-expm1(p)) else p else if (lower.tail) log1p(-p) else log(p)
    arguments <- list(...)
    n         <- max(length(p), lengths(arguments))
    arguments <- lapply(arguments, rep_len, length.out = n)
    logLower  <- rep_len(logLower, n)
    logUpper  <- rep_len(logUpper, n)
    out       <- rep(NA_real_, n)
    out[which(logLower == -Inf)] <- 0
    out[which(logUpper == -Inf)] <- Inf

    # Negative Gompertz shapes can leave a cure fraction. Quantiles at or above
    # the finite-event probability are infinite, rather than numerical failures.
    logCure  <- do.call(pMixture, c(list(q = Inf, lower.tail = FALSE, log.p = TRUE), arguments))
    interior <- is.finite(logLower) & is.finite(logUpper) & logLower <= 0 & logUpper <= 0 & !is.na(logCure)
    out[which(interior & logUpper <= logCure)] <- Inf
    index <- which(interior & logUpper > logCure)
    # A percentile beyond the largest representable time is overflow, not a
    # failed draw. Keep it infinite so confidence bounds retain the upper tail.
    logSurvival <- do.call(pMixture, c(list(q = .Machine$double.xmax, lower.tail = FALSE, log.p = TRUE), arguments))
    overflow    <- which(interior & logUpper > logCure & logSurvival > logUpper)
    out[overflow] <- Inf
    index <- setdiff(index, overflow)
    if (length(index) == 0)
      return(out)

    parameters <- lapply(arguments, function(x) x[index])
    upperTail  <- logLower[index] > log(.5)
    target     <- ifelse(upperTail, logUpper[index], logLower[index])
    # Scale log-tail probabilities to an ordinary CDF for the native solver.
    # Its target stays away from 0/1, retaining small upper-tail probabilities.
    logTimeCdf <- function(q, targetLog, upperTail, ...) {
      lower <- pMixture(exp(q), ..., log.p = TRUE)
      upper <- pMixture(exp(q), ..., lower.tail = FALSE, log.p = TRUE)
      value <- ifelse(upperTail, upper, lower) / -targetLog
      return(ifelse(upperTail, -expm1(value), exp(value)))
    }
    solveQuantiles <- function(rows) {
      values <- rep(NA_real_, length(rows))
      pars <- lapply(parameters, function(x) x[rows])
      quantiles <- try(do.call(flexsurv::qgeneric, c(list(pdist = logTimeCdf,
        p = ifelse(upperTail[rows], -expm1(-1), exp(-1)), targetLog = target[rows],
        upperTail = upperTail[rows]), pars)), silent = TRUE)
      if (inherits(quantiles, "try-error"))
        return(values)
      quantiles   <- exp(quantiles)
      probability <- do.call(pMixture, c(list(q = quantiles, log.p = TRUE), pars))
      if (any(upperTail[rows])) {
        survival <- do.call(pMixture, c(list(q = quantiles, lower.tail = FALSE, log.p = TRUE), pars))
        probability[upperTail[rows]] <- survival[upperTail[rows]]
      }
      accurate <- is.finite(quantiles) & quantiles > 0 & is.finite(probability) & abs(probability - target[rows]) <= 1e-7
      values[accurate] <- quantiles[accurate]
      return(values)
    }
    quantiles <- solveQuantiles(seq_along(index))
    # A failed vector root must not erase valid percentiles in other rows.
    # Retry only missing entries; successful vector results remain unchanged.
    if (length(index) > 1L)
      for (i in which(is.na(quantiles)))
        quantiles[i] <- solveQuantiles(i)
    out[index] <- quantiles
    if (anyNA(out[index]))
      warning(gettext("Some mixture quantiles could not be evaluated accurately and are shown as missing."), call. = FALSE)

    return(out)
  }
  # the restricted mean survival time and the mean are mixtures of the component quantities (without left-truncation)
  rmstMixture <- function(t, ..., start = 0) {
    if (any(start > 0))
      return(flexsurv::rmst_generic(pMixture, t = t, start = start, ...))
    arguments <- splitArguments(list(...), length(t))
    t         <- rep_len(t, arguments[["n"]])
    return(Reduce(`+`, lapply(seq_len(components), function(k) {
      contribution <- arguments[["probabilities"]][, k] * do.call(family[["rmst"]], c(list(t), arguments[["components"]][[k]]))
      contribution[arguments[["probabilities"]][, k] == 0] <- 0
      return(contribution)
    })))
  }
  meanMixture <- function(..., start = 0) {
    if (any(start > 0))
      return(flexsurv::rmst_generic(pMixture, t = Inf, start = start, ...))
    arguments <- splitArguments(list(...), 1)
    return(Reduce(`+`, lapply(seq_len(components), function(k) {
      contribution <- arguments[["probabilities"]][, k] * do.call(family[["mean"]], arguments[["components"]][[k]])
      contribution[arguments[["probabilities"]][, k] == 0] <- 0
      return(contribution)
    })))
  }
  rMixture <- function(n, ...) {
    arguments  <- splitArguments(list(...), n)
    membership <- vapply(seq_len(n), function(i) sample.int(components, 1, prob = arguments[["probabilities"]][i, ]), integer(1))
    uniform    <- stats::runif(n)
    out        <- numeric(n)
    for (k in seq_len(components)) {
      index <- membership == k
      if (any(index))
        out[index] <- do.call(family[["q"]], c(list(uniform[index]), lapply(arguments[["components"]][[k]], function(x) x[index])))
    }
    return(out)
  }

  return(list(
    dlist         = list(
      name           = paste0("mixture.", family[["family"]], ".", components),
      pars           = c(componentPars, weightPars),
      location       = paste0(family[["location"]], 1),
      transforms     = c(rep(family[["transforms"]], components),     rep(list(stats::qlogis), components - 1)),
      inv.transforms = c(rep(family[["inv.transforms"]], components), rep(list(stats::plogis), components - 1))
    ),
    dfns          = list(d = dMixture, p = pMixture, h = hMixture, H = HMixture, q = qMixture, r = rMixture, rmst = rmstMixture, mean = meanMixture),
    componentPars = componentPars,
    weightPars    = weightPars
  ))
}

.sapmDWeibull <- function(x, shape, scale = 1, log = FALSE) {

  if (length(x) == 0 || length(shape) == 0 || length(scale) == 0)
    return(numeric(0))

  n        <- max(length(x), length(shape), length(scale))
  x        <- rep_len(x, n)
  shape    <- rep_len(shape, n)
  scale    <- rep_len(scale, n)
  valid    <- is.finite(x) & x > 0 & is.finite(shape) & shape > 0 & is.finite(scale) & scale > 0
  logRatio <- rep(0, n)
  logRatio[valid] <- log(x[valid]) - log(scale[valid])
  logPower <- shape * logRatio
  # Native dweibull forms powers before taking logs, which can yield Inf - Inf
  # (or Inf * 0) in the tails of narrow components, including valid CI draws.
  logMaximum   <- log(.Machine$double.xmax)
  logMinimum   <- log(.Machine$double.xmin)
  logPrefactor <- (shape[valid] - 1) * logRatio[valid]
  logLeading   <- log(shape[valid]) - log(scale[valid]) + logPrefactor
  tail         <- rep(FALSE, n)
  tail[valid] <- logRatio[valid] < logMinimum | logRatio[valid] > logMaximum |
    logPower[valid] < logMinimum | logPower[valid] > logMaximum |
    logPrefactor < logMinimum | logPrefactor > logMaximum |
    logLeading < logMinimum | logLeading > logMaximum
  out <- rep(NA_real_, n)
  out[!tail] <- stats::dweibull(x[!tail], shape[!tail], scale[!tail], log = log)
  logDensity <- log(shape[tail]) - log(scale[tail]) + (shape[tail] - 1) * logRatio[tail] - exp(logPower[tail])
  logDensity[is.infinite(logPower[tail]) & logPower[tail] > 0] <- -Inf
  out[tail] <- if (log) logDensity else exp(logDensity)

  return(out)
}

.sapMeanGompertz <- function(shape, rate = 1) {

  # A negative shape leaves positive survival at infinity: unrestricted mean
  # lifetime is infinite even when numerical integration returns a finite value.
  size     <- max(length(shape), length(rate))
  shape    <- rep_len(shape, size)
  rate     <- rep_len(rate, size)
  infinite <- !is.na(shape) & shape < 0 & !is.na(rate) & rate > 0
  value    <- rep(Inf, size)
  if (any(!infinite))
    value[!infinite] <- flexsurv::mean_gompertz(shape[!infinite], rate[!infinite])
  return(value)
}

.sapmFamily <- function(distribution) {

  specification <- flexsurv::flexsurv.dists[[distribution]]
  functions     <- switch(distribution,
    "exp" = list(d = stats::dexp, p = stats::pexp, q = stats::qexp, h = flexsurv::hexp,
      rmst = flexsurv::rmst_exp, mean = flexsurv::mean_exp),
    "gamma" = list(d = stats::dgamma, p = stats::pgamma, q = stats::qgamma, h = flexsurv::hgamma,
      rmst = flexsurv::rmst_gamma, mean = flexsurv::mean_gamma),
    "genf" = list(d = flexsurv::dgenf, p = flexsurv::pgenf, q = flexsurv::qgenf, h = flexsurv::hgenf,
      rmst = flexsurv::rmst_genf, mean = flexsurv::mean_genf),
    "gengamma" = list(d = flexsurv::dgengamma, p = flexsurv::pgengamma, q = flexsurv::qgengamma, h = flexsurv::hgengamma,
      rmst = flexsurv::rmst_gengamma, mean = flexsurv::mean_gengamma),
    "gompertz" = list(d = flexsurv::dgompertz, p = flexsurv::pgompertz, q = flexsurv::qgompertz, h = flexsurv::hgompertz,
      rmst = flexsurv::rmst_gompertz, mean = .sapMeanGompertz),
    "llogis" = list(d = flexsurv::dllogis, p = flexsurv::pllogis, q = flexsurv::qllogis, h = flexsurv::hllogis,
      rmst = flexsurv::rmst_llogis, mean = flexsurv::mean_llogis),
    "lnorm" = list(d = stats::dlnorm, p = stats::plnorm, q = stats::qlnorm, h = flexsurv::hlnorm,
      rmst = flexsurv::rmst_lnorm, mean = flexsurv::mean_lnorm),
    "weibull" = list(d = .sapmDWeibull, p = stats::pweibull, q = stats::qweibull, h = flexsurv::hweibull,
      rmst = flexsurv::rmst_weibull, mean = flexsurv::mean_weibull),
    "gengamma.orig" = list(d = flexsurv::dgengamma.orig, p = flexsurv::pgengamma.orig, q = flexsurv::qgengamma.orig,
      h = flexsurv::hgengamma.orig, rmst = flexsurv::rmst_gengamma.orig, mean = flexsurv::mean_gengamma.orig),
    "genf.orig" = list(d = flexsurv::dgenf.orig, p = flexsurv::pgenf.orig, q = flexsurv::qgenf.orig,
      h = flexsurv::hgenf.orig, rmst = flexsurv::rmst_genf.orig, mean = flexsurv::mean_genf.orig)
  )

  return(c(list(
    family         = distribution,
    pars           = specification[["pars"]],
    location       = specification[["location"]],
    transforms     = specification[["transforms"]],
    inv.transforms = specification[["inv.transforms"]],
    # Families with a survreg equivalent use the faster weighted survreg M-step.
    survreg        = switch(
      distribution,
      "exp" = "exponential",
      "lnorm" = "lognormal",
      "llogis" = "loglogistic",
      "weibull" = "weibull",
      NULL
    )
  ), functions))
}

.sapmConditionalMagnitude <- function(fit, family, components) {

  parameters <- .sapmObservationParameters(fit, family, components)
  selected   <- family[["pars"]][vapply(family[["transforms"]], function(link) identical(link, log), logical(1)) &
    family[["pars"]] != family[["location"]]]
  return(vapply(parameters, function(parameter) any(vapply(parameter[selected], function(value)
    any(!is.finite(log(value)) | abs(log(value)) > 15), logical(1))), logical(1)))
}

.sapmDivergingEffects <- function(fit, family, components) {

  selected <- family[["pars"]][family[["pars"]] == family[["location"]] |
    vapply(family[["transforms"]], function(link) identical(link, log), logical(1))]
  return(vapply(seq_len(components), function(k) {
    beta     <- .sapEquationBetas(fit, family, k, components)
    matrices <- .sapEquationMatrices(fit, family, k, components)
    any(vapply(selected, function(parameter) {
      if (length(beta[[parameter]]) == 0L)
        return(FALSE)
      value <- as.vector(matrices[[parameter]] %*% beta[[parameter]])
      return(any(!is.finite(value)) || max(abs(value - mean(value))) > 15)
    }, logical(1)))
  }, logical(1)))
}
