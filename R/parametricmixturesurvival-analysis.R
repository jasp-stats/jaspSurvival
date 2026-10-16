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
# (see .sapRun). Starting values, distribution functions, and plots have separate files.
.sapmDependencies <- c(
  "mixtureComponents", "mixtureMaximumComponents",
  "mixtureStartingValuesKMeans", "mixtureStartingValuesQuantiles", "mixtureStartTails", "mixtureStartingValuesSplitOfThePreviousSolution", "mixtureStartingValuesRandom", "mixtureStartingValuesRandomCount", "mixtureStartMembershipProbabilities",
  "mixtureEmIterations", "setSeed", "seed", "outputFormattingCompareModelsAcrossComponents",
  "mixtureConstrainMinimumSpread", "mixtureConstrainMinimumSpreadType", "mixtureMinimumLogTimeSdRelative", "mixtureMinimumLogTimeSd"
)

.sapmCheckDataset               <- function(dataset, options) {

  hasMixtures <- any(.sapComponents(options) > 1)
  if (!hasMixtures && !options[["mixtureConstrainMinimumSpread"]])
    return()

  if (hasMixtures && !options[["mixtureStartingValuesKMeans"]] && !options[["mixtureStartingValuesQuantiles"]] && !options[["mixtureStartTails"]] && !options[["mixtureStartingValuesSplitOfThePreviousSolution"]] && !options[["mixtureStartingValuesRandom"]])
    .quitAnalysis(gettext("At least one starting value method must be selected."))

  if (hasMixtures)
    .sapmStartMembershipProbabilities(options)

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
.sapmFitMixture                 <- function(dataset, options, distribution, modelTerms, components, previous = NULL) {

  # seeding each number of components makes the starting values independent of the order in which the models are fitted
  jaspBase::.setSeedJASP(options)

  family      <- .sapmFamily(distribution)
  constraint  <- .sapmConstraintSpec(options, distribution, dataset, modelTerms)
  fixed       <- .sapFixedParameters(options, distribution, components)
  .sapCheckFixedConstraint(fixed, constraint)
  formula     <- .sapGetFormula(options, modelTerms, dataset)
  specification <- .sapCompileEquations(dataset, options, family, modelTerms, components)
  nativeFixed <- c(.sapmFixedNativeParameters(fixed), specification[["fixed"]])
  regression  <- lapply(seq_len(components), function(k) .sapRegressionFixed(dataset, modelTerms, formula, k, .sapCaseWeights(dataset, options)))
  survObject  <- .saGetSurvObject(options, dataset)
  caseWeights <- .sapCaseWeights(dataset, options)
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
  termLabels     <- attr(stats::terms(formula), "term.labels")
  emFormula      <- stats::reformulate(if (length(termLabels) > 0) termLabels else "1", response = .sapGetFormula(emOptions, modelTerms, dataset)[[2]])
  covariates     <- stats::model.matrix(emFormula, stats::model.frame(emFormula, dataset))[, -1, drop = FALSE]
  emSpecification <- .sapCompileEquations(dataset, emOptions, family, modelTerms)
  polynomialEquations <- any(vapply(emSpecification[["equations"]], function(equation)
    any(vapply(equation[["equation"]][["transformations"]], function(transform) transform[["transformation"]] == "polynomial", logical(1))), logical(1)))
  fullPolynomialBasis <- polynomialEquations && attr(emSurvObject, "type") %in% c("right", "interval") &&
    any(emSurvObject[, 1] == 0 & emSurvObject[, ncol(emSurvObject)] == 0)
  completeEquations <- any(vapply(emSpecification[["equations"]][setdiff(names(emSpecification[["equations"]]), family[["location"]])],
    function(equation) length(equation[["slopeNames"]]) > 0L, logical(1))) || length(emSpecification[["structuralFixed"]]) > 0L ||
    .sapCheckModelRank(specification, nativeFixed) || fullPolynomialBasis
  mixture        <- .sapmMixtureDistribution(family, components)
  parameterNames <- specification[["parameterNames"]]

  # the solution with one component fewer supplies the split starts
  previousSolution <- NULL
  if (options[["mixtureStartingValuesSplitOfThePreviousSolution"]])
    previousSolution <- .sapmPreviousSolution(dataset, options, distribution, modelTerms, components, previous, family, covariates, survObject)

  starts <- .sapmStarts(options, family, survObject, emSurvObject, components, previousSolution)
  if (length(starts) == 0)
    stop(gettext("No starting values could be constructed for the mixture model."))

  startProgressbar(length(starts), label = gettextf("Fitting %1$s mixture (%2$i components)", .sapOption2DistributionName(distribution), components))

  # every starting value is refined by a few EM iterations and the likelihood is then maximized directly
  # from the state after the first and after the last EM iteration
  candidates <- list()
  fitError   <- NULL
  precisionRejected <- 0L
  failures <- list()
  recordFailure <- function(start, iteration, stage, error) {
    fitError <<- jaspBase::.extractErrorMessage(error)
    failures[[length(failures) + 1L]] <<- data.frame(
      start = start, iteration = iteration, stage = stage, message = fitError)
  }
  for (start in starts) {

    if (jaspBase::isTryError(start[["posterior"]])) {
      recordFailure(start[["name"]], NA_real_, "initialization", start[["posterior"]])
      progressbarTick()
      next
    }

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
      constraint  = constraint,
      fixed       = fixed,
      regression  = regression,
      specification = if (completeEquations) emSpecification else NULL,
      modelTerms = modelTerms,
      options = emOptions
    ), silent = TRUE)

    if (jaspBase::isTryError(states)) {
      recordFailure(start[["name"]], NA_real_, "EM", states)
      progressbarTick()
      next
    }

    for (state in states) {

      # Ordering and conversion belong to this candidate's initialization.
      inits <- try({
        order <- if (length(setdiff(names(nativeFixed), names(specification[["structuralFixed"]]))) > 0L) seq_len(components) else .sapmComponentOrder(family, state[["base"]])
        .sapmInits(mixture, state[["base"]][order], state[["beta"]][order], state[["probabilities"]][order], specification)
      }, silent = TRUE)
      if (jaspBase::isTryError(inits)) {
        recordFailure(start[["name"]], state[["iteration"]], "initialization", inits)
        next
      }
      native <- try(.sapmNativeFit(formula, dataset, options, family, components, mixture, inits, caseWeights, constraint = constraint, fixed = nativeFixed, parameterNames = parameterNames, specification = specification), silent = TRUE)
      error <- if (inherits(native, "try-error")) native else native[["fit"]]
      if (jaspBase::isTryError(error)) {
        recordFailure(start[["name"]], state[["iteration"]], "optimization", error)
        next
      }

      fit       <- native[["fit"]]
      precision <- try(.sapmCheckPrecision(fit, family, components, survObject), silent = TRUE)
      if (jaspBase::isTryError(precision)) {
        precisionRejected <- precisionRejected + 1L
        recordFailure(start[["name"]], state[["iteration"]], "precision", precision)
        next
      }
      diagnostics <- try(.sapmCandidateDiagnostics(fit, family, components, survObject, caseWeights), silent = TRUE)
      if (jaspBase::isTryError(diagnostics)) {
        recordFailure(start[["name"]], state[["iteration"]], "diagnostics", diagnostics)
        next
      }

      orderedInits <- try(.sapmOrderedInits(fit, family, components), silent = TRUE)
      if (jaspBase::isTryError(orderedInits)) {
        recordFailure(start[["name"]], state[["iteration"]], "ordering", orderedInits)
        next
      }

      candidates[[length(candidates) + 1]] <- c(
        list(
          start     = start[["name"]],
          iteration = state[["iteration"]],
          logLik    = fit[["loglik"]],
          converged = fit[["opt"]][["convergence"]] == 0,
          inits     = orderedInits,
          warnings  = native[["warnings"]],
          searchProvenance = attr(fit, "searchProvenance")
        ),
        diagnostics
      )
    }
    progressbarTick()
  }

  failureData <- if (length(failures) > 0L) do.call(rbind, failures) else
    data.frame(start = character(0), iteration = numeric(0), stage = character(0), message = character(0))
  if (length(candidates) == 0) {
    error <- simpleError(gettextf("The mixture model could not be estimated from any starting value: %1$s", if (is.null(fitError)) gettext("the optimizer failed.") else fitError))
    error[["mixture"]] <- list(starts = length(starts), failedStarts = length(starts), failures = failureData)
    stop(error)
  }

  selection <- .sapmSelectCandidate(candidates, constrained = !is.null(constraint))
  best      <- candidates[[selection[["selected"]]]]

  # native covariance/CI construction at the selected estimates, without another optimization
  native <- .sapmNativeFit(formula, dataset, options, family, components, mixture, best[["inits"]], caseWeights, hessian = TRUE, constraint = constraint, fixed = nativeFixed, parameterNames = parameterNames, specification = specification, searchProvenance = best[["searchProvenance"]])
  if (jaspBase::isTryError(native[["fit"]]))
    stop(gettextf("The mixture model could not be finalized: %1$s", jaspBase::.extractErrorMessage(native[["fit"]])))

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
  divergingEffects <- .sapmDivergingEffects(fit, family, components)

  attr(fit, "mixture") <- list(
    family          = family[["family"]],
    components      = components,
    truncated       = truncated,
    candidates      = selection[["candidates"]],
    starts          = length(starts),
    failedStarts    = length(setdiff(vapply(starts, function(start) start[["name"]], character(1)), selection[["candidates"]][["start"]])),
    failures        = failureData,
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
    previous <- .sapFitModel(dataset, options, distribution, modelTerms, components - 1, silent = TRUE)
  }

  if (jaspBase::isTryError(previous))
    return(NULL)

  # the single component fit is not wrapped as a mixture and its parameters have to be assembled
  if (components == 2) {
    base <- stats::setNames(previous[["res"]][family[["pars"]], "est"], family[["pars"]])
    beta <- .sapEquationBetas(previous, family)
    return(list(
      parameters = list(.sapmParameters(family, base, beta, .sapEquationMatrices(previous, family),
        previous[["res.t"]][family[["pars"]], "est"])),
      posterior  = matrix(1, nrow(survObject), 1)
    ))
  }

  return(list(
    parameters = .sapmObservationParameters(previous, family, components - 1),
    posterior  = .sapmPosterior(previous, family, components - 1, survObject)
  ))
}
.sapmComponentParameters        <- function(family, parts, covariates) {

  # natural parameters of every component for every observation (covariates act on the location)
  return(lapply(seq_along(parts[["base"]]), function(k) .sapmParameters(family, parts[["base"]][[k]], parts[["beta"]][[k]],
    covariates, parts[["linkBaseline"]][[k]])))
}
.sapmLogLikelihoodMatrix        <- function(family, survObject, parameters) {
  # Rows have separate component memberships. A longitudinal extension needs a
  # subject ID, class-specific interval histories combined once per subject,
  # subject-level EM memberships and entry conditioning for a persistent class.
  return(matrix(
    vapply(parameters, function(x) .sapmComponentLogLikelihood(family, survObject, x), numeric(nrow(survObject))),
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
  likelihood <- .sapmLogLikelihoodMatrix(family, survObject, parameters)
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
  ess        <- colSums(posterior * caseWeights)

  varyingAncillary <- any(vapply(attr(fit, "equations")[["equations"]], function(equation)
    equation[["parameter"]] != family[["location"]] && .sapEquationVaries(fit, equation), logical(1)))
  if (varyingAncillary) {
    parameters <- .sapmObservationParameters(fit, family, components)
    logIqr <- vapply(parameters, function(parameter) {
      lower <- try(suppressWarnings(do.call(family[["q"]], c(list(0.25), parameter))), silent = TRUE)
      upper <- try(suppressWarnings(do.call(family[["q"]], c(list(0.75), parameter))), silent = TRUE)
      if (jaspBase::isTryError(lower) || jaspBase::isTryError(upper))
        return(rep(NA_real_, nrow(survObject)))
      value <- rep_len(log(upper) - log(lower), nrow(survObject))
      value[!is.finite(value)] <- NA_real_
      return(value)
    }, numeric(nrow(survObject)))
    ratios <- apply(logIqr, 1L, function(row) {
      row <- row[is.finite(row)]
      return(if (length(row) > 1L && max(row) > 0) min(row) / max(row) else NA_real_)
    })
    logIqrRatio <- if (any(is.finite(ratios))) min(ratios[is.finite(ratios)]) else NA_real_
  } else {
    logIqr <- vapply(parts[["base"]], function(base) {
      quantiles <- try(suppressWarnings(do.call(family[["q"]], c(list(c(0.25, 0.75)), as.list(base)))), silent = TRUE)
      if (jaspBase::isTryError(quantiles) || any(!is.finite(quantiles)) || any(quantiles <= 0))
        return(NA_real_)
      return(log(quantiles[2L]) - log(quantiles[1L]))
    }, numeric(1))
    finiteLogIqr <- logIqr[is.finite(logIqr)]
    logIqrRatio <- if (length(finiteLogIqr) > 1L && max(finiteLogIqr) > 0)
      min(finiteLogIqr) / max(finiteLogIqr) else NA_real_
  }
  magnitude <- .sapmConditionalMagnitude(fit, family, components)

  # a cure fraction can make the upper quartile infinite; unavailable quartiles do not establish collapse
  degenerate <- any(!is.finite(theta)) || any(!is.finite(ess)) || min(ess) < 3 ||
    (is.finite(logIqrRatio) && logIqrRatio < 0.01) || any(magnitude) || fit[["opt"]][["convergence"]] != 0

  return(list(degenerate = degenerate))
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
      converged  = converged,
      selected   = seq_along(candidates) == selected
    )
  ))
}
.sapmHessianPositiveDefinite    <- function(fit) {

  hessian <- fit[["opt"]][["hessian"]]

  if (fit[["npars"]] == 0L)
    return(TRUE)

  if (is.null(hessian) || any(!is.finite(hessian)))
    return(NA)

  hessian    <- (hessian + t(hessian)) / 2
  eigenvalue <- try(eigen(hessian, symmetric = TRUE, only.values = TRUE)[["values"]], silent = TRUE)
  if (jaspBase::isTryError(eigenvalue) || any(!is.finite(eigenvalue)))
    return(NA)

  return(min(eigenvalue) > 0)
}
.sapmEm                         <- function(emFormula, dataset, survObject, covariates, family, components, caseWeights, posterior, iterations, constraint = NULL, fixed = rep(list(numeric(0)), components), regression = rep(list(numeric(0)), components), specification = NULL, modelTerms = NULL, options = NULL) {

  probabilities <- colSums(posterior * caseWeights) / sum(caseWeights)
  mSteps        <- NULL
  logLik        <- -Inf
  states        <- list()

  for (iteration in seq_len(iterations)) {

    # M-step: weighted fit of each component
    newMSteps <- try(lapply(seq_len(components), function(k) {
      if (!is.null(specification))
        return(.sapmCompleteMStep(dataset, options, family, modelTerms, k, posterior[, k] * caseWeights,
          mSteps[[k]], constraint, fixed[[k]], specification))
      return(.sapmMStep(
      emFormula  = emFormula,
      dataset    = dataset,
      covariates = covariates,
      family     = family,
      weights    = posterior[, k] * caseWeights,
      previous   = mSteps[[k]],
      constraint = constraint,
      fixed      = c(fixed[[k]], regression[[k]])
      ))
    }), silent = TRUE)

    # E-step: posterior probabilities of component membership
    if (!jaspBase::isTryError(newMSteps)) {
      newLikelihood <- .sapmLogLikelihoodMatrix(family, survObject, lapply(newMSteps, function(mStep) mStep[["parameters"]]))

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
        stop(if (jaspBase::isTryError(newMSteps)) jaspBase::.extractErrorMessage(newMSteps) else gettext("The log-likelihood is not finite."))
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
      linkBaseline  = lapply(mSteps, function(mStep) mStep[["linkBaseline"]]),
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
.sapmInformativeRows <- function(response) {

  # Right censoring at zero contributes no information to a component fit.
  informative <- rep(TRUE, nrow(response))
  if (attr(response, "type") %in% c("right", "interval"))
    informative <- !(response[, 1] == 0 & response[, ncol(response)] == 0)
  if (!any(informative))
    stop(gettext("The mixture model requires events or positive censoring times. Check the survival times and event status."))

  return(informative)
}

.sapmMStep                      <- function(emFormula, dataset, covariates, family, weights, previous, constraint = NULL, fixed = numeric(0)) {

  # Exclude neutral rows from the component fit only; predict all rows.
  response    <- stats::model.response(stats::model.frame(emFormula, dataset))
  informative <- .sapmInformativeRows(response)
  dataset <- dataset[informative, , drop = FALSE]
  weights <- weights[informative]

  # posterior probabilities can underflow to zero which is not allowed as a weight
  weights <- pmax(weights, 1e-10)

  if (length(fixed) > 0L) {

    fit  <- .sapmFixedMStep(emFormula, dataset, covariates, family, weights, previous, constraint, fixed)
    base <- fit[["res"]][family[["pars"]], "est"]
    beta <- fit[["res"]][fit[["covpars"]], "est"]

  } else if (!is.null(constraint) && is.null(family[["survreg"]])) {

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
    fit <- try(suppressWarnings(suppressMessages(.sapmCompressedFit(fitCall))), silent = TRUE)
    if (jaspBase::isTryError(fit)) {
      # This weighted component fit supplies starts only. Native BFGS can backtrack
      # through a nonfinite trial where L-BFGS-B aborts; project its result below.
      fitCall[["method"]]  <- "BFGS"
      fitCall[["lower"]]   <- NULL
      fitCall[["upper"]]   <- NULL
      fitCall[["control"]] <- list(maxit = 1000, reltol = 1e-10, ndeps = rep(1e-6, length(bounds[["lower"]])), fnscale = sum(weights))
      fit <- suppressWarnings(suppressMessages(.sapmCompressedFit(fitCall)))
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
      fit <- suppressWarnings(.sapmCompressedFit(fitCall, survival::survreg))
    } else {
      fit <- try(suppressWarnings(.sapmCompressedFit(fitCall, survival::survreg)), silent = TRUE)
      minimumScale <- if (family[["family"]] == "lnorm") constraint[["naturalBound"]] else 1 / constraint[["naturalBound"]]
      if (jaspBase::isTryError(fit) || any(!is.finite(stats::coef(fit))) || !is.finite(fit[["scale"]]) || fit[["scale"]] < minimumScale) {
        # The public fixed-scale fit supplies feasible location/covariate starts
        # without subtracting extreme component CDFs in flexsurvreg's likelihood.
        fitCall[["scale"]] <- minimumScale
        fit <- suppressWarnings(.sapmCompressedFit(fitCall, survival::survreg))
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

    fit  <- suppressWarnings(suppressMessages(.sapmCompressedFit(fitCall)))
    base <- fit[["res"]][family[["pars"]], "est"]
    beta <- fit[["res"]][fit[["covpars"]], "est"]
  }

  if (any(!is.finite(base)) || any(!is.finite(beta)))
    stop(gettext("A mixture component degenerated during the estimation. Consider fewer components."))

  linkBaseline <- NULL
  if (inherits(fit, "flexsurvreg")) {
    linkBaseline <- fit[["res.t"]][family[["pars"]], "est"]
    for (i in seq_along(base))
      if (!identical(unname(base[i]), unname(fit[["res"]][family[["pars"]][i], "est"])))
        linkBaseline[i] <- family[["transforms"]][[i]](base[i])
  } else {
    location <- stats::coef(fit)[[1L]]
    linkBaseline <- switch(family[["family"]], exp = c(rate = -location),
      lnorm = c(meanlog = location, sdlog = log(fit[["scale"]])),
      weibull = c(shape = -log(fit[["scale"]]), scale = location),
      llogis = c(shape = -log(fit[["scale"]]), scale = location))
  }
  return(list(
    base       = base,
    beta       = beta,
    linkBaseline = linkBaseline,
    parameters = .sapmParameters(family, base, beta, covariates, linkBaseline)
  ))
}
.sapmFixedMStep                 <- function(formula, dataset, covariates, family, weights, previous, constraint, fixed) {

  dlist <- flexsurv::flexsurv.dists[[family[["family"]]]]
  if (!is.null(constraint))
    dlist <- .sapmConstraintDistribution(family[["family"]], constraint, family)
  fitCall <- list(
    formula = formula,
    data    = dataset,
    weights = weights,
    dist    = dlist,
    hessian = FALSE
  )
  if (!is.null(previous))
    fitCall[["inits"]] <- c(previous[["base"]], previous[["beta"]])
  if (!is.null(constraint)) {
    bounds <- .sapmConstraintBounds(constraint, family, 1L, length(family[["pars"]]) + ncol(covariates))
    fitCall[["method"]]  <- "L-BFGS-B"
    fitCall[["lower"]]   <- bounds[["lower"]]
    fitCall[["upper"]]   <- bounds[["upper"]]
    fitCall[["control"]] <- list(maxit = 1000, factr = 1e5, fnscale = sum(weights), pgtol = 1e-6)
  }

  parameterNames <- c(family[["pars"]], colnames(covariates))
  fitCall        <- .sapFixedFitCall(fitCall, fixed, parameterNames)
  return(suppressWarnings(suppressMessages(.sapmCompressedFit(fitCall))))
}

.sapmParameters                 <- function(family, base, beta, covariates, linkBaseline = NULL) {

  # natural parameters of a component for each observation (covariates act on the transformed location parameter)
  parameters <- as.list(base)

  if (is.list(beta)) {
    for (parameter in names(beta)) {
      if (length(beta[[parameter]]) == 0L) next
      index <- match(parameter, family[["pars"]])
      baseline <- if (is.null(linkBaseline)) family[["transforms"]][[index]](base[[parameter]]) else linkBaseline[[parameter]]
      parameters[[parameter]] <- family[["inv.transforms"]][[index]](
        baseline + as.vector(covariates[[parameter]] %*% beta[[parameter]]))
    }
    return(parameters)
  }

  if (length(beta) > 0) {
    location <- which(family[["pars"]] == family[["location"]])
    baseline <- if (is.null(linkBaseline)) family[["transforms"]][[location]](base[[location]]) else linkBaseline[[location]]
    parameters[[location]] <- family[["inv.transforms"]][[location]](
      baseline + as.vector(covariates %*% beta)
    )
  }

  return(parameters)
}
.sapmComponentLogLikelihood     <- function(family, survObject, parameters) {

  subsetParameters <- function(index) lapply(parameters, function(x) if (length(x) > 1) x[index] else x)
  density          <- function(x, index) do.call(family[["d"]], c(list(x), subsetParameters(index), list(log = TRUE)))
  distribution     <- function(x, index, lowerTail = TRUE, logProbability = TRUE) do.call(family[["p"]], c(list(x), subsetParameters(index), list(lower.tail = lowerTail, log.p = logProbability)))

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
          log(probability)
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
.sapmNativeFit                  <- function(formula, dataset, options, family, components, mixture, inits, caseWeights, hessian = FALSE, constraint = NULL, fixed = numeric(0), parameterNames = NULL, specification = NULL, searchProvenance = NULL) {

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
    cl      = options[["coefficientsConfidenceIntervalsLevel"]]
  )
  if (!is.null(specification))
    fitCall[["anc"]] <- specification[["anc"]]
  else if (components > 1 && length(inits) > length(mixture[["dlist"]][["pars"]]))
    fitCall[["anc"]] <- stats::setNames(rep(list(formula[-2]), components - 1), paste0(family[["location"]], 2:components))
  if (options[["weights"]] != "")
    fitCall[["weights"]] <- caseWeights

  schema <- if (!is.null(specification)) .sapNativeSchema(fitCall, specification, fixed)

  if (!is.null(constraint)) {
    point <- .sapmConstraintPoint(inits, constraint, family, components)
    if (!point[["feasible"]])
      return(list(fit = try(stop(gettext("The initial parameters do not satisfy the minimum log-time standard deviation.")), silent = TRUE), hessianWarning = FALSE, warnings = warnings))
    if (hessian) {
      # BFGS with no bounds and maxit=0 evaluates the selected point without moving it.
      # L-BFGS-B may take a step even with maxit=0; its bounds must not reach this call.
      fixedIndex <- match(names(fixed), mixture[["dlist"]][["pars"]])
      fitCall[["hessian"]] <- !any(point[["active"]] & !point[["index"]] %in% fixedIndex)
    } else {
      bounds <- .sapmConstraintBounds(constraint, family, components, length(inits))
      fitCall[["method"]]  <- "L-BFGS-B"
      fitCall[["lower"]]   <- bounds[["lower"]]
      fitCall[["upper"]]   <- bounds[["upper"]]
      fitCall[["control"]] <- list(maxit = 1000, factr = 1e5, ndeps = rep(1e-6, length(inits)), fnscale = sum(caseWeights), pgtol = 1e-6)
    }
  }

  if (!is.null(schema))
    fitCall <- .sapEyringInitials(fitCall, schema, fixed)
  fitCall <- .sapFixedFitCall(fitCall, fixed, parameterNames)
  if (!is.null(schema))
    fitCall <- .sapEyringControls(fitCall, schema)
  covarianceError <- NULL
  covarianceHessian <- NULL
  covarianceFunction <- get(".hess_to_cov", envir = asNamespace("flexsurv"), inherits = FALSE)
  fit <- try(withCallingHandlers(
    suppressMessages(.sapmCompressedFit(fitCall, function(...) {
      .sapEyringNativeFit(list(...), schema, options[["censoringType"]], searchProvenance)
    })),
    warning = function(w) {
      message <- conditionMessage(w)
      if (grepl("hessian|covariance", message, ignore.case = TRUE))
        hessianWarning <<- TRUE
      else
        warnings <<- unique(c(warnings, message))
      invokeRestart("muffleWarning")
    },
    error = function(error) {
      if (!hessian) return()
      frames <- which(vapply(seq_len(sys.nframe()), function(frame)
        identical(sys.function(frame), covarianceFunction), logical(1)))
      if (length(frames) > 0L) {
        covarianceError <<- conditionMessage(error)
        covarianceHessian <<- get("hessian", envir = sys.frame(frames[length(frames)]), inherits = FALSE)
      }
    }
  ), silent = TRUE)
  if (jaspBase::isTryError(fit) && !is.null(covarianceError)) {
    # A singular covariance does not invalidate the selected point estimates.
    # Re-evaluate the same point with maxit=0, without requesting its covariance.
    fitCall[["hessian"]] <- FALSE
    fit <- try(suppressWarnings(suppressMessages(.sapmCompressedFit(fitCall, function(...) {
      .sapEyringNativeFit(list(...), schema, options[["censoringType"]], searchProvenance)
    }))), silent = TRUE)
    if (!jaspBase::isTryError(fit)) {
      fit <- .sapEyringUnavailableInference(fit,
        gettextf("Native covariance finalization failed: %1$s. Confidence intervals are unavailable.", covarianceError))
      fit[["opt"]][["hessian"]] <- covarianceHessian
      provenance <- attr(fit, "informationProvenance", exact = TRUE)
      provenance[["nativeH"]] <- covarianceHessian
      provenance[["nativeAttempt"]] <- TRUE
      provenance[["nativeCondition"]] <- covarianceError
      attr(fit, "informationProvenance") <- provenance
      hessianWarning <- TRUE
    }
  }
  fit <- .sapCompleteFixedFit(fit, fixed, options[["coefficientsConfidenceIntervalsLevel"]],
    if (is.null(specification)) numeric(0) else specification[["structuralFixed"]])
  if (!jaspBase::isTryError(fit)) {
    attr(fit, "nativeControl") <- fitCall[["control"]]
    attr(fit, "structuralInitialAdjustment") <- attr(fitCall, "structuralInitialAdjustment")
    attr(fit, "fullParameterScales") <- attr(fitCall, "fullParameterScales")
  }
  if (!jaspBase::isTryError(fit) && !is.null(specification))
    attr(fit, "equations") <- .sapVerifyNativeSchema(fit, schema[["specification"]])
  if (!jaspBase::isTryError(fit) && !is.null(attr(fit, "inferenceUnavailable")))
    hessianWarning <- TRUE
  if (!jaspBase::isTryError(fit) && identical(attr(fit, "informationOutcome", exact = TRUE), "analyticAvailable"))
    hessianWarning <- FALSE

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
