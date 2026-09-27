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

# the parametric mixture survival analysis shares the fitting, selection, and output of the parametric survival analysis
# (see .sapRun), this file contains the mixture estimator and the mixture specific output
.sapmDependencies <- c(
  "mixtureComponents", "mixtureMaximumComponents",
  "mixtureStartKmeans", "mixtureStartQuantiles", "mixtureStartSplit", "mixtureStartRandom", "mixtureStartRandomCount",
  "mixtureEmIterations", "setSeed", "seed", "compareModelsAcrossComponents",
  "mixtureConstrainSpread", "mixtureMinimumLogTimeSd"
)

.sapmCheckDataset               <- function(dataset, options) {

  hasMixtures <- any(.sapComponents(options) > 1)
  if (!hasMixtures && !options[["mixtureConstrainSpread"]])
    return()

  if (hasMixtures && !options[["mixtureStartKmeans"]] && !options[["mixtureStartQuantiles"]] && !options[["mixtureStartSplit"]] && !options[["mixtureStartRandom"]])
    .quitAnalysis(gettext("At least one starting value method must be selected."))

  # the starting values cluster log event times
  if (options[["censoringType"]] == "interval") {
    exact <- !is.na(dataset[[options[["intervalStart"]]]]) & !is.na(dataset[[options[["intervalEnd"]]]]) & dataset[[options[["intervalStart"]]]] == dataset[[options[["intervalEnd"]]]]
    time  <- dataset[[options[["intervalStart"]]]][exact]
  } else {
    time  <- dataset[[if (options[["censoringType"]] == "counting") options[["intervalEnd"]] else options[["timeToEvent"]]]]
    time  <- time[dataset[[options[["eventStatus"]]]]]
  }

  if (any(time <= 0))
    .quitAnalysis(gettext("The mixture model requires all event times to be positive."))

  return()
}

# mixture estimator
# flexsurvreg fits the custom mixture distribution from every starting value;
# short EM runs initialize its native optimizer
.sapmFitModel                   <- function(dataset, options, distribution, modelTerms, components, previous = NULL) {

  fit <- try(.sapmFitMixture(dataset, options, distribution, modelTerms, components, previous))

  return(fit)
}
.sapmFitMixture                 <- function(dataset, options, distribution, modelTerms, components, previous = NULL) {

  # seeding each number of components makes the starting values independent of the order in which the models are fitted
  jaspBase::.setSeedJASP(options)

  family      <- .sapmFamily(distribution)
  constraint  <- .sapmConstraintSpec(options, distribution)
  formula     <- .sapGetFormula(options, modelTerms)
  survObject  <- .saGetSurvObject(options, dataset)
  caseWeights <- if (options[["weights"]] != "") dataset[[options[["weights"]]]] else rep(1, nrow(dataset))
  truncated   <- options[["censoringType"]] == "counting"

  # the truncated mixture likelihood does not separate into weighted component fits, the EM algorithm therefore
  # maximizes the untruncated likelihood of left-truncated data and only refines the starting values
  emOptions <- options
  if (truncated) {
    emOptions[["censoringType"]] <- "right"
    emOptions[["timeToEvent"]]   <- options[["intervalEnd"]]
  }
  emSurvObject <- .saGetSurvObject(emOptions, dataset)

  # the M-steps always estimate the intercept (flexsurvreg ignores its removal as well)
  termLabels  <- attr(stats::terms(formula), "term.labels")
  emFormula   <- stats::reformulate(if (length(termLabels) > 0) termLabels else "1", response = .sapGetFormula(emOptions, modelTerms)[[2]])
  covariates  <- stats::model.matrix(emFormula, stats::model.frame(emFormula, dataset))[, -1, drop = FALSE]
  mixture     <- .sapmMixtureDistribution(family, components)

  # the solution with one component fewer supplies the split starts
  previousSolution <- NULL
  if (options[["mixtureStartSplit"]])
    previousSolution <- .sapmPreviousSolution(dataset, options, distribution, modelTerms, components, previous, family, covariates, survObject)

  starts <- .sapmStarts(options, family, survObject, emSurvObject, covariates, components, previousSolution)
  if (length(starts) == 0)
    stop(gettext("No starting values could be constructed for the mixture model."))

  # every starting value is refined by a few EM iterations and the likelihood is then maximized directly
  # from the state after the first and after the last EM iteration
  candidates <- list()
  fitError   <- NULL
  precisionRejected <- 0L
  for (start in starts) {

    states <- try(.sapmEm(
      emFormula   = emFormula,
      dataset     = dataset,
      survObject  = emSurvObject,
      covariates  = covariates,
      family      = family,
      components  = components,
      caseWeights = caseWeights,
      posterior   = start[["posterior"]],
      iterations  = options[["mixtureEmIterations"]],
      constraint  = constraint
    ), silent = TRUE)

    if (jaspBase::isTryError(states)) {
      if (is.null(fitError))
        fitError <- .sapmCleanError(states)
      next
    }

    for (state in states) {

      order  <- .sapmComponentOrder(family, state[["base"]])
      inits  <- .sapmInits(mixture, state[["base"]][order], state[["beta"]][order], state[["probabilities"]][order])
      native <- .sapmNativeFit(formula, dataset, options, family, components, mixture, inits, caseWeights, constraint = constraint)
      if (jaspBase::isTryError(native[["fit"]])) {
        fitError <- .sapmCleanError(native[["fit"]])
        next
      }

      fit       <- native[["fit"]]
      precision <- try(.sapmCheckPrecision(fit, family, components, survObject), silent = TRUE)
      if (jaspBase::isTryError(precision)) {
        precisionRejected <- precisionRejected + 1L
        fitError <- .sapmCleanError(precision)
        next
      }
      diagnostics <- try(.sapmCandidateDiagnostics(fit, family, components, survObject, caseWeights), silent = TRUE)
      if (jaspBase::isTryError(diagnostics)) {
        fitError <- .sapmCleanError(diagnostics)
        next
      }

      candidates[[length(candidates) + 1]] <- c(
        list(
          start     = start[["name"]],
          iteration = state[["iteration"]],
          logLik    = fit[["loglik"]],
          converged = fit[["opt"]][["convergence"]] == 0,
          inits     = .sapmOrderedInits(fit, family, components),
          warnings  = native[["warnings"]]
        ),
        diagnostics
      )
    }
  }

  if (length(candidates) == 0)
    stop(gettextf("The mixture model could not be estimated from any starting value: %1$s", if (is.null(fitError)) gettext("the optimizer failed.") else fitError))

  selection <- .sapmSelectCandidate(candidates, constrained = !is.null(constraint))
  best      <- candidates[[selection[["selected"]]]]

  # native covariance/CI construction at the selected estimates, without another optimization
  native <- .sapmNativeFit(formula, dataset, options, family, components, mixture, best[["inits"]], caseWeights, hessian = TRUE, constraint = constraint)
  if (jaspBase::isTryError(native[["fit"]]))
    stop(gettextf("The mixture model could not be finalized: %1$s", .sapmCleanError(native[["fit"]])))

  fit <- native[["fit"]]
  .sapmCheckPrecision(fit, family, components, survObject)
  .sapmCheckFinalPoint(fit, best[["inits"]], best[["logLik"]])
  constraintInfo <- .sapmConstraintInfo(fit, constraint, family, components)
  fit <- .sapmApplyConstraintInference(fit, constraintInfo)

  # the constructed call contains the data and the distribution functions
  fit[["call"]] <- NULL
  estimates     <- .sapmComponentEstimates(fit, family, components)
  posterior     <- .sapmPosterior(fit, family, components, survObject)
  sizes         <- .sapmEffectiveSizes(posterior, caseWeights, .sapmEventIndicator(survObject))
  hessianPositiveDefinite <- .sapmHessianPositiveDefinite(fit)

  # a component also collapses if the covariate effects on its (log-time scale) location diverge within the observed covariate range
  divergingEffects <- vapply(seq_len(components), function(k) {
    length(estimates[["beta"]][[k]]) > 0 && max(abs(covariates %*% estimates[["beta"]][[k]])) > 15
  }, logical(1))

  attr(fit, "mixture") <- list(
    family          = family[["family"]],
    components      = components,
    truncated       = truncated,
    candidates      = selection[["candidates"]],
    starts          = selection[["starts"]],
    replication     = selection[["replication"]],
    nextBest        = selection[["nextBest"]],
    degenerate      = selection[["degenerate"]],
    allDegenerate   = selection[["allDegenerate"]],
    selectedDegenerate = best[["degenerate"]],
    precisionRejected = precisionRejected,
    # retain the optimization status of the selected candidate, not the Hessian-only call
    converged       = best[["converged"]],
    minEss          = min(sizes[["ess"]]),
    minEvents       = min(sizes[["events"]]),
    hessianPositiveDefinite = hessianPositiveDefinite,
    hessianWarning  = if (!is.null(constraintInfo) && constraintInfo[["active"]]) FALSE else native[["hessianWarning"]] || !isTRUE(hessianPositiveDefinite) || any(!is.finite(fit[["cov"]])),
    warnings        = unique(c(best[["warnings"]], native[["warnings"]])),
    collapsed       = which(estimates[["probabilities"]] < 1e-3 | estimates[["collapsed"]] | divergingEffects),
    duplicated      = .sapmDuplicatedComponents(fit, family, components, survObject),
    posterior       = posterior
  )

  return(fit)
}
.sapmPreviousSolution           <- function(dataset, options, distribution, modelTerms, components, previous, family, covariates, survObject) {

  # the solution with one component fewer of the same cell: the fit of the analysis when it is available,
  # otherwise the chain (K-1, ..., 1) is fitted here and discarded afterwards
  if (is.null(previous) || jaspBase::isTryError(previous)) {
    previous <- if (components == 2 && options[["mixtureConstrainSpread"]])
      try(.sapmFitSingle(dataset, options, distribution, modelTerms), silent = TRUE)
    else if (components == 2)
      try(flexsurv::flexsurvreg(
        formula = .sapGetFormula(options, modelTerms),
        data    = dataset,
        dist    = distribution,
        weights = if (options[["weights"]] != "") dataset[[options[["weights"]]]],
        cl      = options[["coefficientsConfidenceIntervalLevel"]]
      ), silent = TRUE)
    else
      try(.sapmFitMixture(dataset, options, distribution, modelTerms, components - 1), silent = TRUE)
  }

  if (jaspBase::isTryError(previous))
    return(NULL)

  # the single component fit is not wrapped as a mixture and its parameters have to be assembled
  if (components == 2) {
    base <- stats::setNames(previous[["res"]][family[["pars"]], "est"], family[["pars"]])
    beta <- if (length(previous[["covpars"]]) > 0) previous[["res"]][previous[["covpars"]], "est"] else numeric(0)
    return(list(
      parameters = list(.sapmParameters(family, base, beta, covariates)),
      posterior  = matrix(1, nrow(survObject), 1)
    ))
  }

  return(list(
    parameters = .sapmObservationParameters(previous, family, components - 1),
    posterior  = .sapmPosterior(previous, family, components - 1, survObject)
  ))
}
.sapmStarts                     <- function(options, family, survObject, emSurvObject, covariates, components, previous) {

  logTime <- .sapmLogTimes(emSurvObject)
  starts  <- list()
  add     <- function(name, posterior) starts[[length(starts) + 1]] <<- list(name = name, posterior = posterior)

  # the random draws are seeded so that the starting values do not depend on the preceding fits
  # (the solution with one component fewer is fitted first when it is not part of the analysis)
  if (options[["mixtureStartKmeans"]]) {
    jaspBase::.setSeedJASP(options)
    add("kmeans", .sapmSoftPosterior(.sapmKmeansMembership(logTime, components), components))
  }

  if (options[["mixtureStartQuantiles"]]) {
    add("quantiles", .sapmSoftPosterior(.sapmQuantileMembership(logTime, components), components))
    # the equal-rank partition is complemented by partitions that isolate the tails
    shares <- .sapmTailShares(components)
    for (i in seq_along(shares))
      add(paste0("tails", i), .sapmSoftPosterior(.sapmTailMembership(logTime, shares[[i]], components), components))
  }

  if (options[["mixtureStartSplit"]] && !is.null(previous))
    for (j in seq_len(components - 1)) {
      posterior <- try(.sapmSplitPosterior(family, survObject, previous, j), silent = TRUE)
      if (!jaspBase::isTryError(posterior))
        add(paste0("split", j), posterior)
    }

  if (options[["mixtureStartRandom"]]) {
    jaspBase::.setSeedJASP(options)
    events <- .sapmEventIndicator(emSurvObject)
    for (i in seq_len(options[["mixtureStartRandomCount"]]))
      add(paste0("random", i), .sapmSoftPosterior(.sapmRandomMembership(logTime, events, components), components))
  }

  return(starts)
}
.sapmSoftPosterior              <- function(membership, components) {

  # soft start avoids empty components
  nObs      <- length(membership)
  posterior <- matrix(0.05 / (components - 1), nObs, components)
  posterior[cbind(seq_len(nObs), membership)] <- 0.95

  return(posterior)
}
.sapmLogTimes                   <- function(survObject) {
  time <- .sapmObservedTimes(survObject)
  return(log(pmax(time, min(time[time > 0]))))
}
.sapmEventIndicator             <- function(survObject) {

  type <- attr(survObject, "type")
  if (type %in% c("right", "counting"))
    return(survObject[, "status"] == 1)

  # exact, left-censored and interval-censored observations all establish that the event occurred
  return(survObject[, "status"] %in% c(1, 2, 3))
}
.sapmKmeansMembership           <- function(logTime, components) {

  if (length(unique(logTime)) <= components)
    return(.sapmQuantileMembership(logTime, components))

  clusters <- stats::kmeans(logTime, centers = components, nstart = 20)

  return(match(clusters[["cluster"]], order(clusters[["centers"]][, 1])))
}
.sapmQuantileMembership         <- function(logTime, components) {
  return(as.integer(cut(rank(logTime, ties.method = "first"), components, labels = FALSE)))
}
.sapmTailShares                 <- function(components) {
  return(switch(
    as.character(components),
    "2" = list(c(0.15, 0.85), c(0.85, 0.15)),
    "3" = list(c(0.15, 0.70, 0.15)),
    "4" = list(c(0.10, 0.40, 0.40, 0.10)),
    list()
  ))
}
.sapmTailMembership             <- function(logTime, shares, components) {

  nObs                   <- length(logTime)
  breaks                 <- unique(c(0, round(cumsum(shares) * nObs)))
  breaks[length(breaks)] <- nObs
  membership             <- as.integer(cut(rank(logTime, ties.method = "first"), breaks = breaks, labels = FALSE))
  membership[is.na(membership)] <- 1L

  return(pmin(pmax(membership, 1L), components))
}
.sapmRandomMembership           <- function(logTime, events, components) {

  if (length(unique(logTime)) < components)
    return(.sapmQuantileMembership(logTime, components))

  # centres are drawn from the event times whenever there are enough of them
  pool <- unique(logTime[events])
  if (length(pool) < components + 1)
    pool <- unique(logTime)
  centers <- sort(sample(pool, components))

  return(apply(abs(outer(logTime, centers, "-")), 1, which.min))
}
.sapmSplitPosterior             <- function(family, survObject, previous, j) {

  # component j of the previous solution is split into a lower and an upper child at its median: events are
  # assigned by their observed time and censored observations by the probability of the censored interval
  parameters <- previous[["parameters"]][[j]]
  nObs       <- nrow(survObject)
  type       <- attr(survObject, "type")
  expand     <- function() lapply(parameters, function(x) if (length(x) > 1) x else rep(x, nObs))
  quantileAt <- function(p) do.call(family[["q"]], c(list(rep(p, nObs)), expand()))
  survivalAt <- function(q) do.call(family[["p"]], c(list(q), expand(), list(lower.tail = FALSE)))
  failureAt  <- function(q) do.call(family[["p"]], c(list(q), expand(), list(lower.tail = TRUE)))

  lower <- ifelse(.sapmObservedTimes(survObject) <= quantileAt(0.5), 0.95, 0.05)

  if (type %in% c("right", "counting")) {
    censored <- survObject[, "status"] != 1
    survival <- survivalAt(survObject[, if (type == "right") "time" else "stop"])
    lower[censored] <- pmin(pmax(pmax(0, (survival - 0.5) / survival), 0.05), 0.95)[censored]
  } else {
    status   <- survObject[, "status"]
    survival <- survivalAt(survObject[, "time1"])
    failure  <- failureAt(survObject[, "time1"])
    lower[status == 0] <- pmin(pmax(pmax(0, (survival - 0.5) / survival), 0.05), 0.95)[status == 0]
    lower[status == 2] <- pmin(pmax(pmin(1, 0.5 / failure), 0.05), 0.95)[status == 2]
  }
  lower[!is.finite(lower)] <- 0.5

  parent    <- previous[["posterior"]]
  posterior <- cbind(
    parent[, seq_len(j - 1), drop = FALSE],
    parent[, j] * lower,
    parent[, j] * (1 - lower),
    parent[, -seq_len(j), drop = FALSE]
  )
  posterior <- pmax(posterior, 1e-12)

  return(posterior / rowSums(posterior))
}
.sapmComponentParameters        <- function(family, parts, covariates) {

  # natural parameters of every component for every observation (covariates act on the location)
  return(lapply(seq_along(parts[["base"]]), function(k) .sapmParameters(family, parts[["base"]][[k]], parts[["beta"]][[k]], covariates)))
}
.sapmLikelihoodMatrix           <- function(family, survObject, parameters, log = FALSE) {
  return(matrix(
    vapply(parameters, function(x) .sapmComponentLikelihood(family, survObject, x, log = log), numeric(nrow(survObject))),
    nrow = nrow(survObject)
  ))
}
.sapmLogSumExp                  <- function(x) {

  maximum <- Reduce(pmax, lapply(seq_len(ncol(x)), function(k) x[, k]))
  out     <- maximum + log(rowSums(exp(x - maximum)))
  out[!is.finite(maximum)] <- maximum[!is.finite(maximum)]

  return(out)
}
.sapmPosteriorProbabilities     <- function(family, survObject, parameters, probabilities) {

  # posterior component membership of every observation
  likelihood <- .sapmLikelihoodMatrix(family, survObject, parameters, log = TRUE)
  joint      <- sweep(likelihood, 2, log(probabilities), "+")

  posterior <- exp(joint - .sapmLogSumExp(joint))
  if (any(!is.finite(posterior)))
    stop(gettext("The component probabilities could not be evaluated accurately. Try a different distribution or fewer components."))

  return(posterior)
}
.sapmEffectiveSizes             <- function(posterior, caseWeights, events) {

  # effective sample size and effective number of events of each component
  return(list(
    ess    = colSums(posterior * caseWeights),
    events = colSums(posterior[events, , drop = FALSE] * caseWeights[events])
  ))
}
.sapmCandidateDiagnostics       <- function(fit, family, components, survObject, caseWeights) {

  parts      <- .sapmComponentEstimates(fit, family, components)
  theta      <- fit[["res.t"]][, "est"]
  posterior  <- suppressWarnings(.sapmPosterior(fit, family, components, survObject))
  sizes      <- .sapmEffectiveSizes(posterior, caseWeights, .sapmEventIndicator(survObject))
  ess        <- sizes[["ess"]]
  eventEss   <- sizes[["events"]]

  # a spike component concentrates on a handful of tied observations: its baseline interquartile range vanishes
  logIqr <- vapply(parts[["base"]], function(base) {
    quantiles <- try(suppressWarnings(do.call(family[["q"]], c(list(c(0.25, 0.75)), as.list(base)))), silent = TRUE)
    if (jaspBase::isTryError(quantiles) || any(!is.finite(quantiles)) || any(quantiles <= 0))
      return(NA_real_)
    return(log(quantiles[2]) - log(quantiles[1]))
  }, numeric(1))
  finiteLogIqr <- logIqr[is.finite(logIqr)]
  logIqrRatio  <- if (length(finiteLogIqr) > 1 && max(finiteLogIqr) > 0) min(finiteLogIqr) / max(finiteLogIqr) else NA_real_

  # dimensionless shape/spread parameters can diverge; the location depends on the time units
  isLog       <- vapply(family[["transforms"]], function(f) identical(f, log), logical(1)) & family[["pars"]] != family[["location"]]
  logScale    <- if (any(isLog)) max(abs(theta[as.vector(outer(family[["pars"]][isLog], seq_len(components), paste0))])) else 0

  # a cure fraction can make the upper quartile infinite; unavailable quartiles do not establish collapse
  degenerate <- any(!is.finite(theta)) || any(!is.finite(ess)) || min(ess) < 3 ||
    (is.finite(logIqrRatio) && logIqrRatio < 0.01) || !is.finite(logScale) || logScale > 15 || fit[["opt"]][["convergence"]] != 0

  return(list(
    degenerate = degenerate,
    minEss     = min(ess),
    minEvents  = min(eventEss)
  ))
}
.sapmSelectCandidate            <- function(candidates, constrained = FALSE) {

  logLik     <- vapply(candidates, function(x) x[["logLik"]], numeric(1))
  degenerate <- vapply(candidates, function(x) x[["degenerate"]], logical(1))
  start      <- vapply(candidates, function(x) x[["start"]], character(1))
  converged  <- vapply(candidates, function(x) x[["converged"]], logical(1))

  # With explicit constraints the feasible, converged maximum is selected; heuristic
  # component diagnostics remain warnings and do not change the fitted objective.
  if (constrained && !any(converged))
    stop(gettext("No constrained candidate converged. Try more starting values, a different distribution, or fewer components."))
  eligible <- if (constrained) converged else if (any(!degenerate)) !degenerate else rep(TRUE, length(candidates))
  selected <- which(eligible)[which.max(logLik[eligible])]

  # solutions within 0.01 log-likelihood units are the same solution: the number of starts that reached it
  # is the replication of the reported solution
  comparable  <- if (constrained) converged else !degenerate
  reached     <- comparable & logLik > logLik[selected] - 0.01
  distinct    <- comparable & logLik <= logLik[selected] - 0.01

  return(list(
    selected      = selected,
    starts        = length(unique(start)),
    replication   = length(unique(start[reached])),
    nextBest      = if (any(distinct)) max(logLik[distinct]) else NA_real_,
    degenerate    = sum(degenerate),
    allDegenerate = all(degenerate),
    candidates    = data.frame(
      start      = start,
      iteration  = vapply(candidates, function(x) x[["iteration"]], numeric(1)),
      logLik     = logLik,
      degenerate = degenerate,
      converged  = vapply(candidates, function(x) x[["converged"]], logical(1)),
      selected   = seq_along(candidates) == selected
    )
  ))
}
.sapmHessianPositiveDefinite    <- function(fit) {

  hessian <- fit[["opt"]][["hessian"]]

  if (is.null(hessian) || any(!is.finite(hessian)))
    return(NA)

  hessian    <- (hessian + t(hessian)) / 2
  eigenvalue <- try(eigen(hessian, symmetric = TRUE, only.values = TRUE)[["values"]], silent = TRUE)
  if (jaspBase::isTryError(eigenvalue) || any(!is.finite(eigenvalue)))
    return(NA)

  return(min(eigenvalue) > 0)
}
.sapmEm                         <- function(emFormula, dataset, survObject, covariates, family, components, caseWeights, posterior, iterations, constraint = NULL) {

  nObs          <- nrow(survObject)
  probabilities <- colSums(posterior * caseWeights) / sum(caseWeights)
  mSteps        <- NULL
  logLik        <- -Inf
  states        <- list()

  for (iteration in seq_len(iterations)) {

    # M-step: weighted fit of each component
    newMSteps <- try(lapply(seq_len(components), function(k) .sapmMStep(
      emFormula  = emFormula,
      dataset    = dataset,
      covariates = covariates,
      family     = family,
      weights    = posterior[, k] * caseWeights,
      previous   = mSteps[[k]],
      constraint = constraint
    )), silent = TRUE)

    # E-step: posterior probabilities of component membership
    if (!jaspBase::isTryError(newMSteps)) {
      newLikelihood <- .sapmLikelihoodMatrix(family, survObject, lapply(newMSteps, function(mStep) mStep[["parameters"]]), log = TRUE)

      # generalized EM: a component keeps its previous estimates if its M-step did not converge to better ones
      if (!is.null(mSteps)) for (k in seq_len(components)) {
        relevant        <- posterior[, k] > 0
        newContribution <- sum(posterior[relevant, k] * caseWeights[relevant] * newLikelihood[relevant, k])
        contribution    <- sum(posterior[relevant, k] * caseWeights[relevant] * likelihood[relevant, k])
        if (!is.na(newContribution) && is.finite(contribution) && newContribution < contribution) {
          newMSteps[[k]]     <- mSteps[[k]]
          newLikelihood[, k] <- likelihood[, k]
        }
      }

      joint      <- sweep(newLikelihood, 2, log(probabilities), "+")
      marginal   <- .sapmLogSumExp(joint)
      newLogLik  <- sum(caseWeights * marginal)
    }

    # a degenerated component ends the EM algorithm at the last valid state
    if (jaspBase::isTryError(newMSteps) || !is.finite(newLogLik)) {
      if (is.null(mSteps))
        stop(if (jaspBase::isTryError(newMSteps)) .sapmCleanError(newMSteps) else gettext("The log-likelihood is not finite."))
      break
    }

    # numerical safeguard: the EM iterations cannot decrease the likelihood
    if (iteration > 1 && newLogLik < logLik - 1e-6 * abs(logLik))
      break

    mSteps        <- newMSteps
    likelihood    <- newLikelihood
    posterior     <- exp(joint - marginal)
    probabilities <- colSums(posterior * caseWeights) / sum(caseWeights)

    # the state after the first iteration and the last valid state are both maximized directly
    state <- list(
      iteration     = iteration,
      base          = lapply(mSteps, function(mStep) mStep[["base"]]),
      beta          = lapply(mSteps, function(mStep) mStep[["beta"]]),
      probabilities = probabilities
    )
    if (iteration == 1)
      states[["first"]] <- state
    states[["last"]] <- state

    if (is.finite(logLik) && abs(newLogLik - logLik) < 1e-8 * abs(newLogLik)) {
      logLik <- newLogLik
      break
    }
    logLik <- newLogLik
  }

  if (length(states) == 0)
    stop(gettext("The EM algorithm produced no valid state."))

  if (states[["last"]][["iteration"]] == states[["first"]][["iteration"]])
    states <- states["first"]

  return(unname(states))
}
.sapmMStep                      <- function(emFormula, dataset, covariates, family, weights, previous, constraint = NULL) {

  # posterior probabilities can underflow to zero which is not allowed as a weight
  weights <- pmax(weights, 1e-10)

  if (!is.null(constraint) && is.null(family[["survreg"]])) {

    bounds <- .sapmConstraintBounds(constraint, family, 1L, length(family[["pars"]]) + ncol(covariates))
    fitCall <- list(
      formula = emFormula,
      data    = dataset,
      weights = weights,
      dist    = .sapmConstraintDistribution(family[["family"]], constraint, family),
      method  = "L-BFGS-B",
      lower   = bounds[["lower"]],
      upper   = bounds[["upper"]],
      hessian = FALSE,
      control = list(maxit = 1000, factr = 1e5, ndeps = rep(1e-6, length(bounds[["lower"]])), fnscale = sum(weights), pgtol = 1e-6)
    )
    if (!is.null(previous))
      fitCall[["inits"]] <- .sapmFeasibleInits(c(previous[["base"]], previous[["beta"]]), constraint, family, 1L)
    else {
      # flexsurvreg normally rescales times by case weights for its initializer.
      # Tiny memberships can make those pseudo-times misleading; initialize this
      # weighted gamma fit from the native initializer on actual observation times.
      times <- .sapmObservedTimes(stats::model.response(stats::model.frame(emFormula, dataset)))
      initial <- fitCall[["dist"]][["inits"]](t = times, mf = NULL, mml = NULL, aux = NULL)
      fitCall[["inits"]] <- c(initial, rep(0, ncol(covariates)))
    }
    fit <- try(suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, fitCall))), silent = TRUE)
    if (jaspBase::isTryError(fit)) {
      # This weighted component fit supplies starts only. Native BFGS can backtrack
      # through a nonfinite trial where L-BFGS-B aborts; project its result below.
      fitCall[["method"]]  <- "BFGS"
      fitCall[["lower"]]   <- NULL
      fitCall[["upper"]]   <- NULL
      fitCall[["control"]] <- list(maxit = 1000, reltol = 1e-10, ndeps = rep(1e-6, length(bounds[["lower"]])), fnscale = sum(weights))
      fit <- suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, fitCall)))
    }
    base <- fit[["res"]][family[["pars"]], "est"]
    beta <- fit[["res"]][fit[["covpars"]], "est"]

    projected <- .sapmFeasibleInits(base, constraint, family, 1L)
    if (family[["family"]] == "gamma")
      projected[["rate"]] <- base[["rate"]] * (projected[["shape"]] / base[["shape"]])
    base <- projected

  } else if (!is.null(family[["survreg"]])) {

    # survreg evaluates weights non-standardly, the call needs to be constructed
    fitCall <- list(
      formula = emFormula,
      data    = dataset,
      weights = weights,
      dist    = family[["survreg"]]
    )
    if (is.null(constraint)) {
      fit <- suppressWarnings(do.call(survival::survreg, fitCall))
    } else {
      fit <- try(suppressWarnings(do.call(survival::survreg, fitCall)), silent = TRUE)
      minimumScale <- if (family[["family"]] == "lnorm") constraint[["naturalBound"]] else 1 / constraint[["naturalBound"]]
      if (jaspBase::isTryError(fit) || any(!is.finite(stats::coef(fit))) || !is.finite(fit[["scale"]]) || fit[["scale"]] < minimumScale) {
        # The public fixed-scale fit supplies feasible location/covariate starts
        # without subtracting extreme component CDFs in flexsurvreg's likelihood.
        fitCall[["scale"]] <- minimumScale
        fit <- suppressWarnings(do.call(survival::survreg, fitCall))
      }
    }

    coefficients <- stats::coef(fit)
    base         <- switch(
      family[["family"]],
      "exp"     = c(rate    = exp(-coefficients[[1]])),
      "lnorm"   = c(meanlog = coefficients[[1]],  sdlog = fit[["scale"]]),
      "llogis"  = c(shape   = 1 / fit[["scale"]], scale = exp(coefficients[[1]])),
      "weibull" = c(shape   = 1 / fit[["scale"]], scale = exp(coefficients[[1]]))
    )
    # survreg models the log time whereas the exponential rate is its reciprocal
    beta         <- coefficients[-1] * if (family[["family"]] == "exp") -1 else 1

  } else {

    fitCall <- list(
      formula = emFormula,
      data    = dataset,
      weights = weights,
      dist    = family[["family"]]
    )
    # start from the previous M-step estimates
    if (!is.null(previous))
      fitCall[["inits"]] <- c(previous[["base"]], previous[["beta"]])

    fit  <- suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, fitCall)))
    base <- fit[["res"]][family[["pars"]], "est"]
    beta <- fit[["res"]][fit[["covpars"]], "est"]
  }

  if (any(!is.finite(base)) || any(!is.finite(beta)))
    stop(gettext("A mixture component degenerated during the estimation. Consider fewer components."))

  return(list(
    base       = base,
    beta       = beta,
    parameters = .sapmParameters(family, base, beta, covariates)
  ))
}
.sapmParameters                 <- function(family, base, beta, covariates) {

  # natural parameters of a component for each observation (covariates act on the transformed location parameter)
  parameters <- as.list(base)

  if (length(beta) > 0) {
    location <- which(family[["pars"]] == family[["location"]])
    parameters[[location]] <- family[["inv.transforms"]][[location]](
      family[["transforms"]][[location]](base[[location]]) + as.vector(covariates %*% beta)
    )
  }

  return(parameters)
}
.sapmComponentLikelihood        <- function(family, survObject, parameters, log = FALSE) {

  subsetParameters <- function(index) lapply(parameters, function(x) if (length(x) > 1) x[index] else x)
  density          <- function(x, index) do.call(family[["d"]], c(list(x), subsetParameters(index), list(log = log)))
  distribution     <- function(x, index, lowerTail = TRUE, logProbability = log) do.call(family[["p"]], c(list(x), subsetParameters(index), list(lower.tail = lowerTail, log.p = logProbability)))

  type <- attr(survObject, "type")
  out  <- numeric(nrow(survObject))

  # left-truncation does not affect the posterior component memberships (the truncation probability cancels out)
  if (type %in% c("right", "counting")) {

    time  <- survObject[, if (type == "right") "time" else "stop"]
    event <- survObject[, "status"] == 1

    if (any(event))
      out[event]  <- density(time[event], event)
    if (any(!event))
      out[!event] <- distribution(time[!event], !event, lowerTail = FALSE)

  } else if (type == "interval") {

    # status: 0 = right censored, 1 = exact, 2 = left censored, 3 = interval censored
    status <- survObject[, "status"]
    time1  <- survObject[, "time1"]
    time2  <- survObject[, "time2"]

    for (s in 0:3) {
      index <- status == s
      if (!any(index))
        next
      out[index] <- switch(
        as.character(s),
        "0" = distribution(time1[index], index, lowerTail = FALSE),
        "1" = density(time1[index], index),
        "2" = distribution(time1[index], index),
        "3" = {
          probability <- distribution(time2[index], index, logProbability = FALSE) - distribution(time1[index], index, logProbability = FALSE)
          if (log) log(probability) else probability
        }
      )
    }

  } else {
    stop(gettextf("Censoring type '%1$s' is not supported by the mixture model.", type))
  }

  return(out)
}
.sapmObservedTimes              <- function(survObject) {

  # a single representative time of each observation
  type <- attr(survObject, "type")
  if (type == "right") {
    time <- survObject[, "time"]
  } else if (type == "counting") {
    time <- survObject[, "stop"]
  } else {
    # interval censored observations are represented by their midpoints and left censored observations by half of their upper limit
    status <- survObject[, "status"]
    time   <- survObject[, "time1"]
    time[status == 3] <- (survObject[status == 3, "time1"] + survObject[status == 3, "time2"]) / 2
    time[status == 2] <- survObject[status == 2, "time1"] / 2
  }

  return(time)
}
.sapmNativeFit                  <- function(formula, dataset, options, family, components, mixture, inits, caseWeights, hessian = FALSE, constraint = NULL) {

  hessianWarning <- FALSE
  warnings       <- character(0)
  if (!hessian)
    inits <- .sapmFeasibleInits(inits, constraint, family, components)
  fitCall        <- list(
    formula = formula,
    data    = dataset,
    dist    = mixture[["dlist"]],
    dfns    = mixture[["dfns"]],
    inits   = inits,
    method  = "BFGS",
    control = if (hessian) list(maxit = 0) else list(maxit = 1000, reltol = 1e-10),
    hessian = hessian,
    cl      = options[["coefficientsConfidenceIntervalLevel"]]
  )
  # the covariates enter the location parameter of every component
  if (components > 1 && length(inits) > length(mixture[["dlist"]][["pars"]]))
    fitCall[["anc"]] <- stats::setNames(rep(list(formula[-2]), components - 1), paste0(family[["location"]], 2:components))
  if (options[["weights"]] != "")
    fitCall[["weights"]] <- caseWeights

  if (!is.null(constraint)) {
    point <- .sapmConstraintPoint(inits, constraint, family, components)
    if (!point[["feasible"]])
      return(list(fit = try(stop(gettext("The initial parameters do not satisfy the minimum log-time standard deviation.")), silent = TRUE), hessianWarning = FALSE, warnings = warnings))
    if (hessian) {
      # BFGS with no bounds and maxit=0 evaluates the selected point without moving it.
      # L-BFGS-B may take a step even with maxit=0; its bounds must not reach this call.
      fitCall[["hessian"]] <- !any(point[["active"]])
    } else {
      bounds <- .sapmConstraintBounds(constraint, family, components, length(inits))
      fitCall[["method"]]  <- "L-BFGS-B"
      fitCall[["lower"]]   <- bounds[["lower"]]
      fitCall[["upper"]]   <- bounds[["upper"]]
      fitCall[["control"]] <- list(maxit = 1000, factr = 1e5, ndeps = rep(1e-6, length(inits)), fnscale = sum(caseWeights), pgtol = 1e-6)
    }
  }

  fit <- try(withCallingHandlers(
    suppressMessages(do.call(flexsurv::flexsurvreg, fitCall)),
    warning = function(w) {
      message <- conditionMessage(w)
      if (grepl("hessian|covariance", message, ignore.case = TRUE))
        hessianWarning <<- TRUE
      else
        warnings <<- unique(c(warnings, message))
      invokeRestart("muffleWarning")
    }
  ), silent = TRUE)

  return(list(fit = fit, hessianWarning = hessianWarning, warnings = warnings))
}
.sapmCheckFinalPoint            <- function(fit, expected, logLik) {

  actual    <- unname(fit[["res"]][, "est"])
  tolerance <- sqrt(.Machine$double.eps) * pmax(abs(expected), .Machine$double.xmin)
  if (length(actual) != length(expected) || any(!is.finite(actual)) || any(abs(actual - expected) > tolerance) ||
      !is.finite(fit[["loglik"]]) || abs(fit[["loglik"]] - logLik) > sqrt(.Machine$double.eps) * max(1, abs(logLik)))
    stop(gettext("The selected solution could not be retained while computing its covariance. Try a different distribution or fewer components."))

  return()
}
.sapmCheckPrecision             <- function(fit, family, components, survObject) {

  if (!is.finite(fit[["loglik"]]))
    stop(gettext("The log-likelihood of the mixture model is not finite."))

  parameters <- .sapmObservationParameters(fit, family, components)
  arguments  <- list()
  for (k in seq_len(components))
    arguments[paste0(family[["pars"]], k)] <- parameters[[k]]
  weights <- .sapmParameterNames(family, components)[["weightPars"]]
  arguments[weights] <- as.list(fit[["res"]][weights, "est"])
  distribution <- function(q) do.call(fit[["dfns"]][["p"]], c(list(q = q), arguments))
  losesPrecision <- function(upper, lower) {
    difference <- upper - lower
    # A relative separation below sqrt(epsilon) risks losing at least half the significant digits.
    # This is a conditioning check of native probabilities, not another likelihood evaluator.
    return(any(!is.finite(difference) | difference <= 0 |
      difference <= sqrt(.Machine$double.eps) * pmax(abs(upper), abs(lower))))
  }

  type     <- attr(survObject, "type")
  censored <- survObject[, "status"] != 1
  risk     <- FALSE
  if (any(censored)) {
    lower <- rep(0, nrow(survObject))
    upper <- rep(Inf, nrow(survObject))
    if (type %in% c("right", "counting")) {
      lower[censored] <- survObject[censored, if (type == "right") "time" else "stop"]
    } else {
      status <- survObject[, "status"]
      lower[status %in% c(0, 3)] <- survObject[status %in% c(0, 3), "time1"]
      upper[status == 2]        <- survObject[status == 2, "time1"]
      upper[status == 3]        <- survObject[status == 3, "time2"]
    }
    pLower <- distribution(lower)
    pUpper <- distribution(upper)
    pUpper[upper == Inf] <- 1
    risk <- losesPrecision(pUpper[censored], pLower[censored])
  }
  if (type == "counting")
    risk <- risk || losesPrecision(rep(1, nrow(survObject)), distribution(survObject[, "start"]))

  if (risk)
    stop(gettext("The likelihood may lose numerical precision at these censoring or entry times. Results cannot be reported reliably. Try a different distribution or fewer components."))

  return()
}
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
.sapmFamily                     <- function(distribution) {

  specification <- flexsurv::flexsurv.dists[[distribution]]

  return(list(
    family         = distribution,
    pars           = specification[["pars"]],
    location       = specification[["location"]],
    transforms     = specification[["transforms"]],
    inv.transforms = specification[["inv.transforms"]],
    # families with a survreg equivalent use the faster weighted survreg M-step
    survreg        = switch(distribution, "exp" = "exponential", "lnorm" = "lognormal", "llogis" = "loglogistic", "weibull" = "weibull", NULL),
    d              = switch(
      distribution,
      "exp"           = stats::dexp,
      "gamma"         = stats::dgamma,
      "genf"          = flexsurv::dgenf,
      "gengamma"      = flexsurv::dgengamma,
      "gompertz"      = flexsurv::dgompertz,
      "llogis"        = flexsurv::dllogis,
      "lnorm"         = stats::dlnorm,
      "weibull"       = stats::dweibull,
      "gengamma.orig" = flexsurv::dgengamma.orig,
      "genf.orig"     = flexsurv::dgenf.orig
    ),
    p              = switch(
      distribution,
      "exp"           = stats::pexp,
      "gamma"         = stats::pgamma,
      "genf"          = flexsurv::pgenf,
      "gengamma"      = flexsurv::pgengamma,
      "gompertz"      = flexsurv::pgompertz,
      "llogis"        = flexsurv::pllogis,
      "lnorm"         = stats::plnorm,
      "weibull"       = stats::pweibull,
      "gengamma.orig" = flexsurv::pgengamma.orig,
      "genf.orig"     = flexsurv::pgenf.orig
    ),
    q              = switch(
      distribution,
      "exp"           = stats::qexp,
      "gamma"         = stats::qgamma,
      "genf"          = flexsurv::qgenf,
      "gengamma"      = flexsurv::qgengamma,
      "gompertz"      = flexsurv::qgompertz,
      "llogis"        = flexsurv::qllogis,
      "lnorm"         = stats::qlnorm,
      "weibull"       = stats::qweibull,
      "gengamma.orig" = flexsurv::qgengamma.orig,
      "genf.orig"     = flexsurv::qgenf.orig
    ),
    h              = switch(
      distribution,
      "exp"           = flexsurv::hexp,
      "gamma"         = flexsurv::hgamma,
      "genf"          = flexsurv::hgenf,
      "gengamma"      = flexsurv::hgengamma,
      "gompertz"      = flexsurv::hgompertz,
      "llogis"        = flexsurv::hllogis,
      "lnorm"         = flexsurv::hlnorm,
      "weibull"       = flexsurv::hweibull,
      "gengamma.orig" = flexsurv::hgengamma.orig,
      "genf.orig"     = flexsurv::hgenf.orig
    ),
    rmst           = switch(
      distribution,
      "exp"           = flexsurv::rmst_exp,
      "gamma"         = flexsurv::rmst_gamma,
      "genf"          = flexsurv::rmst_genf,
      "gengamma"      = flexsurv::rmst_gengamma,
      "gompertz"      = flexsurv::rmst_gompertz,
      "llogis"        = flexsurv::rmst_llogis,
      "lnorm"         = flexsurv::rmst_lnorm,
      "weibull"       = flexsurv::rmst_weibull,
      "gengamma.orig" = flexsurv::rmst_gengamma.orig,
      "genf.orig"     = flexsurv::rmst_genf.orig
    ),
    mean           = switch(
      distribution,
      "exp"           = flexsurv::mean_exp,
      "gamma"         = flexsurv::mean_gamma,
      "genf"          = flexsurv::mean_genf,
      "gengamma"      = flexsurv::mean_gengamma,
      "gompertz"      = flexsurv::mean_gompertz,
      "llogis"        = flexsurv::mean_llogis,
      "lnorm"         = flexsurv::mean_lnorm,
      "weibull"       = flexsurv::mean_weibull,
      "gengamma.orig" = flexsurv::mean_gengamma.orig,
      "genf.orig"     = flexsurv::mean_genf.orig
    )
  ))
}
.sapmCleanError                 <- function(error) {
  return(conditionMessage(attr(error, "condition")))
}

# mixture output
.sapmComponentsTable            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureComponentsTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution / components
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureComponentsTable", "coefficientsConfidenceInterval", "coefficientsConfidenceIntervalLevel")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapmComponentsTableFun,
    name          = "mixtureComponentsTable",
    title         = gettext("Component Mean and Median"),
    dependencies  = outputDependencies,
    position      = 2.2
  )

  return()
}
.sapmClassificationTable        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureClassificationTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureClassificationTable")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapmClassificationTableFun,
    name          = "mixtureClassificationTable",
    title         = gettext("Mixture Classification"),
    dependencies  = outputDependencies,
    position      = 2.3
  )

  return()
}
.sapmDiagnosticsTable           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureDiagnosticsTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureDiagnosticsTable")

  # every mixture model is a row of a single table
  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = list(fit),
    tableFunction = .sapmDiagnosticsTableFun,
    name          = "mixtureDiagnosticsTable",
    title         = gettext("Estimation Diagnostics"),
    dependencies  = outputDependencies,
    position      = 2.4
  )

  return()
}
.sapmComponentPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureComponentPlot"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()
  fit <- .sapNestFit(fit)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureComponentPlot", "mixtureComponentPlotType", "mixtureComponentPlotKaplanMeier",
                          "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeCustom",
                          "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapmComponentPlotFun,
    name          = "mixtureComponentPlot",
    title         = gettext("Mixture Components"),
    dependencies  = outputDependencies,
    position      = 3.5
  )

  return()
}
.sapmFilterMixtures             <- function(fit, options) {

  # mixture specific output is shown only for models with multiple components
  if (!.saSurvivalReady(options))
    return(fit)

  return(Filter(function(x) attr(x, "components") > 1, fit))
}

.sapmComponentsTableFun         <- function(fit, options) {

  # create the table
  componentsTable <- createJaspTable()
  .sapAddColumnSubgroup(     componentsTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( componentsTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   componentsTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        componentsTable, options, output = "coefficientsCovarianceMatrix")
  componentsTable$addColumnInfo(name = "component", title = gettext("Component"),      type = "string")
  componentsTable$addColumnInfo(name = "quantity",  title = "",                        type = "string")
  componentsTable$addColumnInfo(name = "est",       title = gettext("Estimate"),       type = "number")
  componentsTable$addColumnInfo(name = "se",        title = gettext("Standard Error"), type = "number")
  if (options[["coefficientsConfidenceInterval"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["coefficientsConfidenceIntervalLevel"]])
    componentsTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    componentsTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(componentsTable)

  # the mixing probabilities and the component parameters are reported in the coefficients table, the last
  # two quantities of each component are its mean and its median
  mixture    <- attr(fit, "mixture")
  quantities <- length(.sapmFamily(mixture[["family"]])[["pars"]]) + 3
  data       <- .sapmComponentsTableData(fit, options[["coefficientsConfidenceIntervalLevel"]])
  data       <- data[(seq_len(nrow(data)) - 1) %% quantities >= quantities - 2, , drop = FALSE]

  data[["component"]]    <- NA_character_
  data[["component"]][seq(1, nrow(data), by = 2)] <- gettextf("Component %1$i", seq_len(mixture[["components"]]))

  data$subgroup        <- NA
  data$distribution    <- NA
  data$components      <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$components[1]   <- attr(fit, "components")
  data$model[1]        <- attr(fit, "modelTitle")

  # add footnotes
  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    componentsTable$addFootnote(attr(fit, "label"))
  for (message in .sapConstraintNote(fit))
    componentsTable$addFootnote(message)
  componentsTable$addFootnote(gettext("The mean and the median are those of the fitted component distribution; they are not restricted to the observed follow-up."))
  if (anyNA(data[["est"]]))
    componentsTable$addFootnote(gettext("Some component means or medians are infinite or could not be evaluated numerically and are shown as missing."))
  if (!.sapConstraintActive(fit) && (anyNA(data[["se"]]) || (options[["coefficientsConfidenceInterval"]] && anyNA(data[c("lower", "upper")]))))
    componentsTable$addFootnote(gettext("Some standard errors or confidence intervals could not be evaluated and are shown as missing."))
  componentsTable$addFootnote(gettext("Standard errors and confidence intervals are based on the delta method."))
  if (length(fit[["covpars"]]) > 0)
    componentsTable$addFootnote(gettext("The component means and medians correspond to the reference level of factors and zero value of covariates."))
  for (message in .sapmFitMessages(fit, options))
    componentsTable$addFootnote(message, symbol = gettext("Warning:"))

  componentsTable$setData(data)
  componentsTable$showSpecifiedColumnsOnly <- TRUE

  return(componentsTable)
}
.sapmComponentsTableData        <- function(fit, level) {

  mixture    <- attr(fit, "mixture")
  family     <- .sapmFamily(mixture[["family"]])
  components <- mixture[["components"]]
  estimates  <- fit[["res.t"]][, "est"]

  # quantities of each component: mixing probability, parameters, mean, and median
  quantities <- function(estimates) {
    parameters <- .sapmBaseParameters(estimates, family, components)
    unlist(lapply(seq_len(components), function(k) {
      mean <- try(do.call(family[["mean"]], as.list(parameters[["base"]][[k]])), silent = TRUE)
      c(
        parameters[["probabilities"]][k],
        parameters[["base"]][[k]],
        if (jaspBase::isTryError(mean)) NA else mean,
        .sapmComponentMedian(family, parameters[["base"]][[k]])
      )
    }), use.names = FALSE)
  }

  # delta method with a numerical Jacobian (covariate effects do not affect the baseline quantities)
  estimate      <- quantities(estimates)
  standardError <- rep(NA_real_, length(estimate))
  if (!.sapConstraintActive(fit)) {
    jacobian  <- matrix(0, length(estimate), length(estimates))
    baseIndex <- setdiff(seq_along(estimates), fit[["covpars"]])
    for (i in baseIndex) {
      step          <- 1e-5 * max(abs(estimates[i]), 1)
      upper         <- lower <- estimates
      upper[i]      <- upper[i] + step
      lower[i]      <- lower[i] - step
      jacobian[, i] <- (quantities(upper) - quantities(lower)) / (2 * step)
    }
    standardError <- sqrt(pmax(diag(jacobian %*% fit[["cov"]] %*% t(jacobian)), 0))
  }

  estimate[!is.finite(estimate)]           <- NA
  standardError[!is.finite(standardError)] <- NA

  # confidence intervals are computed on the link scale of each quantity (logit for probabilities, log for positive quantities)
  logParameter <- vapply(family[["transforms"]], function(f) identical(f, log), logical(1))
  links        <- rep(c("logit", ifelse(logParameter, "log", "identity"), "log", "log"), components)
  z            <- stats::qnorm((1 + level) / 2)
  lower        <- upper <- rep(NA_real_, length(estimate))
  logitLink    <- links == "logit"    & !is.na(standardError)
  logLink      <- links == "log"      & !is.na(standardError) & estimate > 0
  identityLink <- links == "identity" & !is.na(standardError)

  lower[logitLink]    <- stats::plogis(stats::qlogis(estimate[logitLink]) - z * standardError[logitLink] / (estimate[logitLink] * (1 - estimate[logitLink])))
  upper[logitLink]    <- stats::plogis(stats::qlogis(estimate[logitLink]) + z * standardError[logitLink] / (estimate[logitLink] * (1 - estimate[logitLink])))
  lower[logLink]      <- exp(log(estimate[logLink]) - z * standardError[logLink] / estimate[logLink])
  upper[logLink]      <- exp(log(estimate[logLink]) + z * standardError[logLink] / estimate[logLink])
  lower[identityLink] <- estimate[identityLink] - z * standardError[identityLink]
  upper[identityLink] <- estimate[identityLink] + z * standardError[identityLink]
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  nQuantities <- length(family[["pars"]]) + 3
  component   <- rep(NA_character_, length(estimate))
  component[seq(1, length(estimate), by = nQuantities)] <- gettextf("Component %1$i", seq_len(components))

  return(data.frame(
    component = component,
    quantity  = rep(c(gettext("Mixing probability"), family[["pars"]], gettext("Mean"), gettext("Median")), components),
    est       = estimate,
    se        = standardError,
    lower     = lower,
    upper     = upper
  ))
}
.sapmClassificationTableFun     <- function(fit, options) {

  # create the table
  classificationTable <- createJaspTable()
  .sapAddColumnSubgroup(     classificationTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( classificationTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   classificationTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        classificationTable, options, output = "coefficientsCovarianceMatrix")
  classificationTable$addColumnInfo(name = "component",     title = gettext("Component"),                   type = "string")
  classificationTable$addColumnInfo(name = "probability",   title = gettext("Mixing probability"),          type = "number")
  classificationTable$addColumnInfo(name = "count",         title = gettext("Count"),                       type = "integer",  overtitle = gettext("Classified"))
  classificationTable$addColumnInfo(name = "proportion",    title = gettext("Proportion"),                  type = "number",   overtitle = gettext("Classified"))
  classificationTable$addColumnInfo(name = "meanPosterior", title = gettext("Mean posterior probability"),  type = "number",   overtitle = gettext("Classified"))

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(classificationTable)

  mixture    <- attr(fit, "mixture")
  components <- mixture[["components"]]
  posterior  <- mixture[["posterior"]]
  dataset    <- attr(fit, "dataset")
  weights    <- if (options[["weights"]] != "") dataset[[options[["weights"]]]] else rep(1, nrow(posterior))

  # observations are classified to the component with the highest posterior probability
  assigned <- max.col(posterior, ties.method = "first")
  data     <- data.frame(
    component     = gettextf("Component %1$i", seq_len(components)),
    probability   = .sapmComponentEstimates(fit, .sapmFamily(mixture[["family"]]), components)[["probabilities"]],
    count         = vapply(seq_len(components), function(k) sum(weights[assigned == k]), numeric(1)),
    meanPosterior = vapply(seq_len(components), function(k) {
      if (!any(assigned == k)) return(NA_real_)
      return(stats::weighted.mean(posterior[assigned == k, k], weights[assigned == k]))
    }, numeric(1))
  )
  data$proportion <- data$count / sum(weights)

  data$subgroup        <- NA
  data$distribution    <- NA
  data$components      <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$components[1]   <- attr(fit, "components")
  data$model[1]        <- attr(fit, "modelTitle")

  # relative entropy (1 = certain posterior classification)
  entropy <- -sum(weights * rowSums(ifelse(posterior > 0, posterior * log(posterior), 0)))
  entropy <- 1 - entropy / (sum(weights) * log(components))

  # add footnotes
  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    classificationTable$addFootnote(attr(fit, "label"))
  for (message in .sapConstraintNote(fit))
    classificationTable$addFootnote(message)
  classificationTable$addFootnote(gettextf("Observations are classified to the component with the highest posterior probability. The relative entropy of the classification is %1$.3f (values close to 1 indicate low uncertainty in the assignments).", entropy))
  for (message in .sapmFitMessages(fit, options))
    classificationTable$addFootnote(message, symbol = gettext("Warning:"))

  classificationTable$setData(data)
  classificationTable$showSpecifiedColumnsOnly <- TRUE

  return(classificationTable)
}
.sapmDiagnosticsTableFun        <- function(fit, options) {

  # create the table
  diagnosticsTable <- createJaspTable()
  if (options[["subgroup"]] != "")
    diagnosticsTable$addColumnInfo(name = "subgroup",     title = gettext("Subgroup"),     type = "string")
  diagnosticsTable$addColumnInfo(name = "distribution",   title = gettext("Distribution"), type = "string")
  diagnosticsTable$addColumnInfo(name = "model",          title = gettext("Model"),        type = "string")
  diagnosticsTable$addColumnInfo(name = "components",     title = gettext("Components"),   type = "integer")
  diagnosticsTable$addColumnInfo(name = "starts",         title = gettext("Starts"),       type = "integer")
  diagnosticsTable$addColumnInfo(name = "replication",    title = gettext("Replications"), type = "integer")
  diagnosticsTable$addColumnInfo(name = "logLik",         title = gettext("Log Lik."),     type = "number")
  diagnosticsTable$addColumnInfo(name = "nextBest",       title = gettext("Next Best Log Lik."), type = "number")
  diagnosticsTable$addColumnInfo(name = "degenerate",     title = gettext("Degenerate Candidates"), type = "integer")
  diagnosticsTable$addColumnInfo(name = "minEss",         title = gettext("Min. Component n (ESS)"), type = "number")
  diagnosticsTable$addColumnInfo(name = "minEvents",      title = gettext("Min. Component Events"),  type = "number")
  diagnosticsTable$addColumnInfo(name = "converged",       title = gettext("Optimizer Converged"), type = "string")
  diagnosticsTable$addColumnInfo(name = "hessian",        title = gettext("Hessian Positive Definite"), type = "string")

  if (!.saSurvivalReady(options) || is.null(fit))
    return(diagnosticsTable)

  data <- .saSafeRbind(lapply(fit, .sapmRowDiagnosticsTable))

  # add footnotes
  diagnosticsTable$addFootnote(gettext("Starts is the number of starting values that produced a solution and Replications the number of them that reached the reported solution (within 0.01 log-likelihood units)."))
  if (options[["mixtureConstrainSpread"]])
    diagnosticsTable$addFootnote(gettext("The converged candidate with the highest likelihood satisfying the spread bound is selected. Component-size and separation diagnostics are warnings and do not change this selection."))
  else
    diagnosticsTable$addFootnote(gettext("A candidate solution is degenerate when a component collapses on a few observations (fewer than 3 effective observations, a vanishing interquartile range, or a diverging parameter), or when optimization does not converge. The best non-degenerate candidate is selected. If all candidates are degenerate, the best of them is reported with a warning."))
  diagnosticsTable$addFootnote(gettext("Convergence and Hessian diagnostics refer to the selected candidate fit. Convergence does not rule out a local optimum or unreliable standard errors."))
  for (message in unique(unlist(lapply(fit, .sapConstraintNote))))
    diagnosticsTable$addFootnote(message)
  for (message in .sapCollectFitErrors(fit, options))
    diagnosticsTable$addFootnote(message, symbol = gettext("Error:"))
  for (message in .sapmSummaryMessages(fit, options)[["warnings"]])
    diagnosticsTable$addFootnote(message, symbol = gettext("Warning:"))

  diagnosticsTable$setData(data)
  diagnosticsTable$showSpecifiedColumnsOnly <- TRUE

  return(diagnosticsTable)
}
.sapmRowDiagnosticsTable        <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  mixture <- attr(fit, "mixture")

  return(data.frame(
    .sapRowModelInformation(fit),
    starts          = mixture[["starts"]],
    replication     = mixture[["replication"]],
    logLik          = fit[["loglik"]],
    nextBest        = mixture[["nextBest"]],
    degenerate      = mixture[["degenerate"]],
    minEss          = mixture[["minEss"]],
    minEvents       = mixture[["minEvents"]],
    converged       = if (mixture[["converged"]]) gettext("yes") else gettext("no"),
    hessian         = if (is.na(mixture[["hessianPositiveDefinite"]])) NA_character_ else if (mixture[["hessianPositiveDefinite"]]) gettext("yes") else gettext("no")
  ))
}
.sapmComponentPlotFun           <- function(fit, options) {

  fit <- fit[[1]]

  estimateTitle <- switch(
    options[["mixtureComponentPlotType"]],
    "survival"           = gettext("Survival Probability"),
    "failureProbability" = gettext("Failure Probability"),
    "density"            = gettext("Density"),
    "hazard"             = gettext("Hazard")
  )

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(createJaspPlot(title = estimateTitle))

  plotData <- try(.sapmComponentPlotData(fit, options))

  if (jaspBase::isTryError(plotData)) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempPlot)
  }

  predictionWarnings <- attr(plotData, "predictionWarnings")
  if (!any(is.finite(plotData[["at"]]) & is.finite(plotData[["estimate"]]))) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(paste(unique(c(gettext("No finite predictions are available for this plot."), predictionWarnings)), collapse = "\n"))
    return(tempPlot)
  }

  hasLevel <- length(unique(plotData[["Level"]])) > 1
  options[["predictionsConfidenceInterval"]] <- options[["predictionsConfidenceInterval"]] && !all(is.na(plotData[["lCi"]]))

  # the mixture is displayed in black and the components follow the color palette
  colors <- c("black", jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(attr(fit, "components")))
  names(colors) <- levels(plotData[["Component"]])

  plot <- ggplot2::ggplot(data = plotData)

  if (options[["mixtureComponentPlotType"]] %in% c("survival", "failureProbability") && options[["mixtureComponentPlotKaplanMeier"]] && options[["censoringType"]] == "right") {
    kmTable <- .sapKaplanMeierStepData(attr(fit, "dataset"), options, failureProbability = options[["mixtureComponentPlotType"]] == "failureProbability")
    plot    <- plot + jaspGraphs::geom_line(mapping = ggplot2::aes(x = at, y = estimate), data = kmTable, color = "grey60")
  }

  if (options[["predictionsConfidenceInterval"]]) {
    aesCall <- list(
      x        = as.name("at"),
      ymin     = as.name("lCi"),
      ymax     = as.name("uCi"),
      group    = if (hasLevel) as.name("Level")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = plotData[plotData[["Component"]] == levels(plotData[["Component"]])[1], ], fill = "grey60", alpha = 0.30)
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
  }

  aesCall <- list(
    x        = as.name("at"),
    y        = as.name("estimate"),
    color    = as.name("Component"),
    linetype = if (hasLevel) as.name("Level")
  )
  geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]))
  plot <- plot + do.call(jaspGraphs::geom_line, geomCall) +
    ggplot2::scale_color_manual(values = colors, name = gettext("Component"))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(plotData[["at"]], na.rm = TRUE))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    plotData[["estimate"]],
    if (options[["predictionsConfidenceInterval"]]) plotData[["lCi"]],
    if (options[["predictionsConfidenceInterval"]]) plotData[["uCi"]]), na.rm = TRUE))

  plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep) +
    jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep) +
    ggplot2::ylab(estimateTitle) + ggplot2::xlab(gettext("Time"))

  # the detailed theme is available only for the survival probability plots
  if (options[["plotTheme"]] == "detailed")
    options[["plotTheme"]] <- "jasp"
  plot <- .sapPredictionPlotAddTheme(plot, options)
  plot <- .sapPredictionPlotAddCaption(plot, predictionWarnings, 550)

  tempPlot <- createJaspPlot(width = 550, height = .sapPredictionPlotCaptionHeight(plot, 320))
  tempPlot$plotObject <- plot

  return(tempPlot)
}
.sapmComponentPlotData          <- function(fit, options) {

  mixture    <- attr(fit, "mixture")
  family     <- .sapmFamily(mixture[["family"]])
  components <- mixture[["components"]]
  type       <- options[["mixtureComponentPlotType"]]
  # the components might change rapidly, the time steps are not rounded for a smooth display
  options[["predictionsLifeTimeRoundSteps"]] <- FALSE
  times      <- .sapOptions2PredictionTime(options, fit, type = "mixtureComponents", plot = TRUE)
  ci         <- options[["predictionsConfidenceInterval"]]
  level      <- options[["predictionsConfidenceIntervalLevel"]]

  # the functions are evaluated with the covariate dependent parameters supplied by flexsurv
  mixtureDensity    <- function(t, start, ...) fit[["dfns"]][["d"]](t, ...)
  componentFunction <- function(k) {
    function(t, start, ...) {
      arguments  <- list(...)
      n          <- max(length(t), lengths(arguments))
      parameters <- lapply(stats::setNames(arguments[paste0(family[["pars"]], k)], family[["pars"]]), rep_len, length.out = n)
      t          <- rep_len(t, n)
      switch(
        type,
        "survival"           = do.call(family[["p"]], c(list(t), parameters, list(lower.tail = FALSE))),
        "failureProbability" = do.call(family[["p"]], c(list(t), parameters)),
        "density"            = .sapmStickBreaking(if (components > 1) do.call(cbind, lapply(arguments[paste0("v", seq_len(components - 1))], rep_len, length.out = n)), n)[, k] * do.call(family[["d"]], c(list(t), parameters)),
        "hazard"             = do.call(family[["h"]], c(list(t), parameters))
      )
    }
  }

  mixtureSummary <- switch(
    type,
    "survival"           = .sapSummaryPredictions(fit, type = "survival", t = times, ci = ci, cl = level),
    "failureProbability" = .sapSummaryPredictions(fit, type = "survival", t = times, ci = ci, cl = level),
    "density"            = .sapSummaryPredictions(fit, fn = mixtureDensity, t = times, ci = ci, cl = level),
    "hazard"             = .sapSummaryPredictions(fit, type = "hazard", t = times, ci = ci, cl = level)
  )
  componentSummaries <- lapply(seq_len(components), function(k) .sapSummaryPredictions(fit, fn = componentFunction(k), t = times, ci = FALSE))
  predictionWarnings <- unique(c(attr(mixtureSummary, "predictionWarnings"), unlist(lapply(componentSummaries, attr, "predictionWarnings"))))

  componentLabels <- c(gettext("Mixture"), gettextf("Component %1$i", seq_len(components)))
  out <- list()
  for (j in seq_along(mixtureSummary)) {

    levelLabel <- if (length(mixtureSummary) > 1) decodeColNames(names(mixtureSummary)[j]) else NA

    mixtureData <- data.frame(
      at        = mixtureSummary[[j]][[1]],
      estimate  = mixtureSummary[[j]][["est"]],
      lCi       = if (ci) mixtureSummary[[j]][["lcl"]] else NA,
      uCi       = if (ci) mixtureSummary[[j]][["ucl"]] else NA,
      Component = componentLabels[1],
      Level     = levelLabel
    )
    if (type == "failureProbability") {
      mixtureData[["estimate"]] <- 1 - mixtureData[["estimate"]]
      mixtureData[c("lCi", "uCi")] <- 1 - mixtureData[c("uCi", "lCi")]
    }

    out[[length(out) + 1]] <- mixtureData
    for (k in seq_len(components)) {
      out[[length(out) + 1]] <- data.frame(
        at        = componentSummaries[[k]][[j]][["time"]],
        estimate  = componentSummaries[[k]][[j]][["est"]],
        lCi       = NA,
        uCi       = NA,
        Component = componentLabels[k + 1],
        Level     = levelLabel
      )
    }
  }

  out <- do.call(rbind, out)
  out[["Component"]] <- factor(out[["Component"]], levels = componentLabels)

  # set any Inf to NA
  out[["estimate"]][is.infinite(out[["estimate"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])]           <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])]           <- NA
  attr(out, "predictionWarnings") <- predictionWarnings

  return(out)
}

# mixture messages
.sapmFitMessages                <- function(fit, options) {

  mixture  <- attr(fit, "mixture")
  messages <- c(.sapConstraintWarning(fit), .sapNativeFitWarnings(fit))

  if (is.null(mixture))
    return(messages)

  if (mixture[["precisionRejected"]] > 0)
    messages <- c(messages, sprintf(ngettext(
      mixture[["precisionRejected"]],
      "%1$i candidate fit was omitted because its likelihood could not be evaluated reliably at the available numerical precision.",
      "%1$i candidate fits were omitted because their likelihoods could not be evaluated reliably at the available numerical precision."
    ), mixture[["precisionRejected"]]))
  for (message in mixture[["warnings"]])
    messages <- c(messages, gettextf("Estimation warning: %1$s", message))

  # the reported solution is a local optimum whenever no other start reached it
  # (with only degenerate candidates the replication is zero and the degeneracy is reported instead)
  if (!mixture[["allDegenerate"]] && mixture[["replication"]] <= 1 && mixture[["starts"]] >= 2)
    messages <- c(messages, gettextf(
      "The reported solution was reached by only %1$i of %2$i starts; the estimates might be a local optimum. Consider more random starts.",
      mixture[["replication"]], mixture[["starts"]]
    ))

  if (mixture[["allDegenerate"]])
    messages <- c(messages, gettext("All candidate solutions were flagged as degenerate or failed to converge; the reported estimates may be unreliable. Consider fewer components or more starting values."))
  else if (!is.null(attr(fit, "constraints")) && mixture[["selectedDegenerate"]])
    messages <- c(messages, gettext("The selected solution triggered component diagnostics despite satisfying the spread bound. Inspect the component sizes and dispersion before interpreting the mixture."))
  if (!mixture[["allDegenerate"]] && is.finite(mixture[["minEvents"]]) && mixture[["minEvents"]] < 5)
    messages <- c(messages, gettextf("The smallest component is supported by %1$.1f effective events; such a component is weakly identified. Consider fewer components.", mixture[["minEvents"]]))

  if (length(mixture[["collapsed"]]) > 0)
    messages <- c(messages, sprintf(ngettext(
      length(mixture[["collapsed"]]),
      "Component %1$s has a negligible weight or diverging parameters; consider fewer components.",
      "Components %1$s have a negligible weight or diverging parameters; consider fewer components."
    ), paste(mixture[["collapsed"]], collapse = ", ")))

  coinciding <- .sapmCoincidingSets(mixture[["duplicated"]], mixture[["components"]])
  if (length(coinciding) == 1 && length(coinciding[[1]]) == mixture[["components"]])
    messages <- c(messages, gettext("All components coincide; the mixture is not identified and its standard errors are unreliable. Consider fewer components."))
  else for (set in coinciding)
    messages <- c(messages, gettextf(
      "Components %1$s coincide; the mixture is not identified and its standard errors are unreliable. Consider fewer components.",
      .sapmListLabel(set)
    ))

  # the coinciding components already explain an unreliable Hessian
  if (mixture[["hessianWarning"]] && nrow(mixture[["duplicated"]]) == 0)
    messages <- c(messages, gettext("The Hessian or parameter covariance could not be used reliably; standard errors and confidence intervals may be unavailable or unreliable."))

  return(messages)
}
.sapmSummaryMessages            <- function(fit, options) {

  messages       <- list(notes = NULL, warnings = NULL)
  successfulFits <- Filter(function(x) !jaspBase::isTryError(x), fit)
  isMixture      <- vapply(successfulFits, function(x) !is.null(attr(x, "mixture")), logical(1))
  mixtures       <- successfulFits[isMixture]

  messages[["notes"]] <- unique(unlist(lapply(successfulFits, .sapConstraintNote)))
  for (model in successfulFits[!isMixture]) {
    message <- paste(.sapmFitMessages(model, options), collapse = " ")
    if (message != "")
      messages[["warnings"]] <- c(messages[["warnings"]], paste0(.sapmCellLabel(model, options), ": ", message))
  }

  if (length(mixtures) == 0)
    return(messages)

  starts <- vapply(mixtures, function(x) attr(x, "mixture")[["starts"]], numeric(1))
  messages[["notes"]] <- c(messages[["notes"]], gettextf(
    "Mixture models were estimated by direct maximization of the likelihood from %1$s starting values per model (%2$s), each refined by %3$i EM iterations.",
    if (min(starts) == max(starts)) as.character(min(starts)) else gettextf("%1$i to %2$i", min(starts), max(starts)),
    .sapmStartLabels(options),
    options[["mixtureEmIterations"]]
  ))
  if (options[["censoringType"]] == "counting")
    messages[["notes"]] <- c(messages[["notes"]], gettext("Left-truncated data: the EM algorithm provides starting values only; the estimates are from the direct maximization of the likelihood."))

  # the messages of each model are reported in a single footnote, models of the same distribution with the same messages are reported together
  fitMessages <- vapply(mixtures, function(x) paste(.sapmFitMessages(x, options), collapse = " "), character(1))
  # A one-component fit is also nested in every mixture of the same family and model.
  fitMessages <- trimws(paste(fitMessages, .sapmLocalOptimumMessages(successfulFits)[isMixture]))
  cells       <- vapply(seq_along(mixtures), function(i) paste(attr(mixtures[[i]], "distribution"), attr(mixtures[[i]], "modelTitle"), attr(mixtures[[i]], "subgroupLabel"), fitMessages[i], sep = "\n"), character(1))
  for (cell in unique(cells[fitMessages != ""])) {
    index      <- which(cells == cell)
    components <- vapply(mixtures[index], function(x) attr(x, "components"), numeric(1))
    messages[["warnings"]] <- c(messages[["warnings"]], paste0(.sapmCellLabel(mixtures[[index[1]]], options, components), ": ", fitMessages[index[1]]))
  }

  return(messages)
}
.sapmLocalOptimumMessages       <- function(mixtures) {

  # a mixture with more components contains the mixture with fewer components, a lower log-likelihood
  # therefore shows that the reported estimates are a local optimum of the likelihood
  messages   <- rep("", length(mixtures))
  cells      <- vapply(mixtures, function(x) paste(attr(x, "family"), attr(x, "modelId"), attr(x, "subgroupLabel"), sep = "\n"), character(1))
  components <- vapply(mixtures, function(x) attr(x, "components"), numeric(1))
  logLik     <- vapply(mixtures, function(x) x[["loglik"]], numeric(1))

  for (cell in unique(cells)) {
    index <- which(cells == cell)
    index <- index[order(components[index])]
    # every model with fewer components is nested, the comparison uses the best fitting one of them
    for (i in seq_along(index)[-1]) {
      best <- index[which.max(logLik[index[seq_len(i - 1)]])]
      if (logLik[index[i]] < logLik[best] - 1e-6)
        messages[index[i]] <- gettextf(
          "The log-likelihood is lower than that of the nested model with %1$s; the reported estimates are a local optimum. Consider more random starts.",
          .sapComponentsLabel(components[best])
        )
    }
  }

  return(messages)
}
.sapmStartLabels                <- function(options) {

  labels <- c(
    if (options[["mixtureStartKmeans"]])    gettext("k-means"),
    if (options[["mixtureStartQuantiles"]]) gettext("quantiles"),
    if (options[["mixtureStartSplit"]])     gettext("splits of the solution with one component fewer"),
    if (options[["mixtureStartRandom"]])    gettextf("%1$i random", options[["mixtureStartRandomCount"]])
  )

  return(paste(labels, collapse = ", "))
}
.sapmCellLabel                  <- function(fit, options, components = attr(fit, "components")) {
  return(gettextf(
    "%1$s model %2$s with %3$s%4$s",
    attr(fit, "distribution"),
    attr(fit, "modelTitle"),
    if (length(components) == 1) .sapComponentsLabel(components) else gettextf("%1$s components", .sapmListLabel(components)),
    if (options[["subgroup"]] != "") paste0(" (", attr(fit, "subgroupLabel"), ")") else ""
  ))
}
.sapmListLabel                  <- function(x) {
  if (length(x) == 1)
    return(as.character(x))
  return(gettextf("%1$s and %2$s", paste(x[-length(x)], collapse = ", "), x[length(x)]))
}
.sapmCoincidingSets             <- function(duplicated, components) {

  # coinciding pairs are merged into sets of mutually coinciding components
  set <- seq_len(components)
  for (i in seq_len(nrow(duplicated)))
    set[set == set[duplicated[i, 2]]] <- set[duplicated[i, 1]]

  return(Filter(function(x) length(x) > 1, unname(split(seq_len(components), set))))
}
.sapmCoefficientsNames          <- function(coeffTable, fit) {

  mixture     <- attr(fit, "mixture")
  family      <- .sapmFamily(mixture[["family"]])
  components  <- mixture[["components"]]
  names       <- rownames(fit[["res"]])

  coeffTable[["mixtureComponent"]] <- NA_integer_

  for (k in seq_len(components)) {

    # component parameters
    for (par in family[["pars"]])
      coeffTable[["coefficient"]][names == paste0(par, k)] <- gettextf("%1$s (component %2$i)", par, k)

    # covariate effects on the location parameter of the component (the effects of the first component are not prefixed)
    index <- fit[["covpars"]][fit[["mx"]][[paste0(family[["location"]], k)]]]
    if (length(index) > 0) {
      coeffTable[["mixtureComponent"]][index] <- k
      if (k > 1) {
        prefix <- paste0(family[["location"]], k, "(")
        coeffTable[["coefficient"]][index] <- substr(names[index], nchar(prefix) + 1, nchar(names[index]) - 1)
      }
    }
  }

  # the stick-breaking weights are reported as the mixing probabilities of all components: the weights are
  # conditional on the preceding components and are easily mistaken for the probabilities themselves
  weightIndex <- which(names %in% paste0("v", seq_len(components - 1)))
  if (length(weightIndex) > 0) {

    probabilities <- .sapmMixingProbabilities(fit, fit[["cl"]])
    replacement   <- coeffTable[rep(weightIndex[1], components), , drop = FALSE]

    replacement[["coefficient"]]             <- gettextf("Mixing probability (component %1$i)", seq_len(components))
    replacement[["est"]]                     <- probabilities[["est"]]
    replacement[["se"]]                      <- probabilities[["se"]]
    replacement[["lower"]]                   <- probabilities[["lower"]]
    replacement[["upper"]]                   <- probabilities[["upper"]]
    replacement[["isRegressionCoefficient"]] <- FALSE
    replacement[["mixtureComponent"]]        <- NA_integer_

    coeffTable           <- rbind(
      coeffTable[seq_len(min(weightIndex) - 1), , drop = FALSE],
      replacement,
      coeffTable[-seq_len(max(weightIndex)), , drop = FALSE]
    )
    rownames(coeffTable) <- NULL
  }

  return(coeffTable)
}
.sapmMixingProbabilities        <- function(fit, level) {

  # the mixing probabilities, their standard errors, and their confidence intervals of all components
  mixture    <- attr(fit, "mixture")
  quantities <- length(.sapmFamily(mixture[["family"]])[["pars"]]) + 3
  data       <- .sapmComponentsTableData(fit, level)

  return(data[seq(1, nrow(data), by = quantities), c("est", "se", "lower", "upper")])
}
