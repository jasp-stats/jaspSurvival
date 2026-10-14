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
.sapmOrderedInits               <- function(fit, family, components) {

  # order estimates before flexsurvreg constructs their covariance and confidence intervals
  estimates <- .sapmComponentEstimates(fit, family, components)
  order     <- .sapmComponentOrder(family, estimates[["base"]])
  inits     <- fit[["res"]][, "est"]
  if (components == 1 || identical(order, seq_len(components)))
    return(unname(inits))

  parameters  <- length(family[["pars"]])
  effects     <- fit[["ncoveffs"]] / components
  baseIndex   <- as.vector(vapply(order, function(k) (k - 1) * parameters + seq_len(parameters), numeric(parameters)))
  weightIndex <- components * parameters + seq_len(components - 1)
  effectIndex <- if (effects > 0) as.vector(vapply(order, function(k) max(weightIndex) + (k - 1) * effects + seq_len(effects), numeric(effects))) else numeric(0)
  inits <- inits[c(baseIndex, weightIndex, effectIndex)]
  inits[weightIndex] <- stats::plogis(.sapmReorderedWeights(fit[["res.t"]][weightIndex, "est"], order))

  return(unname(inits))
}
.sapmReorderedWeights           <- function(estimates, order) {

  # retain small remaining probabilities when re-expressing the fitted component order
  remaining     <- c(0, cumsum(stats::plogis(estimates, lower.tail = FALSE, log.p = TRUE)))
  probabilities <- (c(stats::plogis(estimates, log.p = TRUE), 0) + remaining)[order]

  return(vapply(seq_along(estimates), function(k) {
    probabilities[k] - .sapmLogSumExp(matrix(probabilities[seq.int(k + 1, length(probabilities))], nrow = 1))
  }, numeric(1)))
}
.sapmInits                      <- function(mixture, base, beta, probabilities) {

  # flexsurvreg order: component parameters, stick-breaking weights, location effects of the first component, location effects of the remaining components
  components <- length(base)
  weights    <- numeric(0)
  remaining  <- 1
  for (k in seq_len(components - 1)) {
    weights   <- c(weights, min(max(probabilities[k] / remaining, 1e-6), 1 - 1e-6))
    remaining <- remaining - probabilities[k]
  }

  return(c(
    stats::setNames(unlist(base, use.names = FALSE), mixture[["componentPars"]]),
    stats::setNames(weights, mixture[["weightPars"]]),
    unlist(beta, use.names = FALSE)
  ))
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
  weights <- .sapmParameterNames(family, components)[["weightPars"]]
  probabilities <- as.vector(.sapmStickBreaking(if (length(weights) > 0) matrix(stats::plogis(estimates[weights]), nrow = 1), 1))

  return(list(base = base, probabilities = probabilities))
}
.sapmComponentEstimates         <- function(fit, family, components) {

  estimates  <- fit[["res.t"]][, "est"]
  parameters <- .sapmBaseParameters(estimates, family, components)
  isLog      <- vapply(family[["transforms"]], function(f) identical(f, log), logical(1)) & family[["pars"]] != family[["location"]]

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
    collapsed     = vapply(seq_len(components), function(k) {
      any(abs(estimates[paste0(family[["pars"]], k)][isLog]) > 15)
    }, logical(1))
  ))
}
.sapmObservationParameters      <- function(fit, family, components) {

  # natural parameters of each component for each observation of the fitted mixture
  estimates  <- .sapmComponentEstimates(fit, family, components)
  covariates <- fit[["data"]][["mml"]][[paste0(family[["location"]], 1)]]
  if (!is.null(covariates))
    covariates <- covariates[, -1, drop = FALSE]

  return(.sapmComponentParameters(family, estimates, covariates))
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
      log(arguments[["probabilities"]][, k]) + do.call(family[["d"]], c(list(x), arguments[["components"]][[k]], list(log = TRUE)))
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
  qMixture <- function(p, ..., lower.tail = TRUE, log.p = FALSE) {
    if (log.p)
      p <- exp(p)
    if (!lower.tail)
      p <- 1 - p
    arguments <- list(...)
    n         <- max(length(p), lengths(arguments))
    arguments <- lapply(arguments, rep_len, length.out = n)
    p         <- rep_len(p, n)
    out       <- rep(NA_real_, n)
    out[which(p == 0)] <- 0
    out[which(p == 1)] <- Inf

    # Negative Gompertz shapes can leave a cure fraction. Quantiles at or above
    # the finite-event probability are infinite, rather than numerical failures.
    limit    <- do.call(pMixture, c(list(q = Inf), arguments))
    interior <- is.finite(p) & p > 0 & p < 1 & is.finite(limit)
    out[which(interior & p >= limit)] <- Inf
    index <- which(interior & p < limit)
    if (length(index) == 0)
      return(out)

    parameters <- lapply(arguments, function(x) x[index])
    # Invert on log time with flexsurv's native solver so units do not set the root tolerance.
    logTimeCdf <- function(q, ...) pMixture(exp(q), ...)
    quantiles  <- try(do.call(flexsurv::qgeneric, c(list(pdist = logTimeCdf, p = p[index]), parameters)), silent = TRUE)
    if (!inherits(quantiles, "try-error")) {
      quantiles   <- exp(quantiles)
      probability <- do.call(pMixture, c(list(q = quantiles), parameters))
      upperTail   <- p[index] > 0.5
      if (any(upperTail)) {
        survival <- do.call(pMixture, c(list(q = quantiles, lower.tail = FALSE), parameters))
        probability[upperTail] <- survival[upperTail]
      }
      target   <- ifelse(upperTail, 1 - p[index], p[index])
      accurate <- is.finite(quantiles) & quantiles > 0 & is.finite(probability) & abs(probability - target) <= 1e-7 * target
      out[index[accurate]] <- quantiles[accurate]
    }
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
      arguments[["probabilities"]][, k] * do.call(family[["rmst"]], c(list(t), arguments[["components"]][[k]]))
    })))
  }
  meanMixture <- function(..., start = 0) {
    if (any(start > 0))
      return(flexsurv::rmst_generic(pMixture, t = Inf, start = start, ...))
    arguments <- splitArguments(list(...), 1)
    return(Reduce(`+`, lapply(seq_len(components), function(k) {
      arguments[["probabilities"]][, k] * do.call(family[["mean"]], arguments[["components"]][[k]])
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
    dfns          = list(d = dMixture, p = pMixture, q = qMixture, r = rMixture, rmst = rmstMixture, mean = meanMixture),
    componentPars = componentPars,
    weightPars    = weightPars
  ))
}
.sapmDWeibull <- function(x, shape, scale = 1, log = FALSE) {

  if (length(x) == 0 || length(shape) == 0 || length(scale) == 0)
    return(numeric(0))
  n     <- max(length(x), length(shape), length(scale))
  x     <- rep_len(x, n)
  shape <- rep_len(shape, n)
  scale <- rep_len(scale, n)
  valid <- is.finite(x) & x > 0 & is.finite(shape) & shape > 0 & is.finite(scale) & scale > 0
  logRatio <- rep(0, n)
  logRatio[valid] <- log(x[valid]) - log(scale[valid])
  logPower <- shape * logRatio
  # Native dweibull forms powers before taking logs, which can yield Inf - Inf
  # (or Inf * 0) in the tails of narrow components, including valid CI draws.
  logMaximum <- log(.Machine$double.xmax)
  tail <- rep(FALSE, n)
  tail[valid] <- abs(logRatio[valid]) > logMaximum | logPower[valid] > logMaximum |
    (shape[valid] - 1) * logRatio[valid] > logMaximum |
    log(shape[valid]) - log(scale[valid]) + (shape[valid] - 1) * logRatio[valid] > logMaximum
  out <- rep(NA_real_, n)
  out[!tail] <- stats::dweibull(x[!tail], shape[!tail], scale[!tail], log = log)
  logDensity <- log(shape[tail]) - log(scale[tail]) + (shape[tail] - 1) * logRatio[tail] - exp(logPower[tail])
  logDensity[is.infinite(logPower[tail]) & logPower[tail] > 0] <- -Inf
  out[tail] <- if (log) logDensity else exp(logDensity)

  return(out)
}
.sapmFamily <- function(distribution) {

  specification <- flexsurv::flexsurv.dists[[distribution]]
  functions <- switch(distribution,
    "exp" = list(d = stats::dexp, p = stats::pexp, q = stats::qexp, h = flexsurv::hexp,
                  rmst = flexsurv::rmst_exp, mean = flexsurv::mean_exp),
    "gamma" = list(d = stats::dgamma, p = stats::pgamma, q = stats::qgamma, h = flexsurv::hgamma,
                    rmst = flexsurv::rmst_gamma, mean = flexsurv::mean_gamma),
    "genf" = list(d = flexsurv::dgenf, p = flexsurv::pgenf, q = flexsurv::qgenf, h = flexsurv::hgenf,
                   rmst = flexsurv::rmst_genf, mean = flexsurv::mean_genf),
    "gengamma" = list(d = flexsurv::dgengamma, p = flexsurv::pgengamma, q = flexsurv::qgengamma, h = flexsurv::hgengamma,
                       rmst = flexsurv::rmst_gengamma, mean = flexsurv::mean_gengamma),
    "gompertz" = list(d = flexsurv::dgompertz, p = flexsurv::pgompertz, q = flexsurv::qgompertz, h = flexsurv::hgompertz,
                       rmst = flexsurv::rmst_gompertz, mean = flexsurv::mean_gompertz),
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
    survreg        = switch(distribution, "exp" = "exponential", "lnorm" = "lognormal", "llogis" = "loglogistic", "weibull" = "weibull", NULL)
  ), functions))
}
