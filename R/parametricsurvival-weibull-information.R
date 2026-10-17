# Exact right-censored Weibull information in the native equation coordinates.
.sapWeibullTarget <- function(fit, specification, censoringType) {

  if (specification[["family"]] != "weibull" || censoringType != "right")
    return(NULL)
  response <- fit[["data"]][["m"]][[1L]]
  if (!inherits(response, "Surv") || !identical(attr(response, "type"), "right") ||
    any(!is.finite(response[, "time"]) | response[, "time"] < 0) ||
    any(response[, "time"] == 0 & response[, "status"] == 1) ||
    any(c("(bhazard)", "(rtrunc)") %in% names(fit[["data"]][["m"]])))
    return(NULL)

  specification <- .sapVerifyNativeSchema(fit, specification)
  theta         <- fit[["res.t"]][, "est"]
  components    <- specification[["components"]]
  free          <- setdiff(seq_along(theta), fit[["fixedpars"]])
  weights       <- fit[["data"]][["m"]][["(weights)"]]
  fixedIndex    <- match(names(specification[["fixed"]]), names(theta))
  if (length(weights) != nrow(response) || any(!is.finite(weights) | weights < 0) ||
    any(!is.finite(theta)) || !identical(as.integer(free), as.integer(fit[["optpars"]])) ||
    anyNA(fixedIndex) || any(!fixedIndex %in% fit[["fixedpars"]]) ||
    !identical(unname(theta[fixedIndex]), as.numeric(specification[["fixed"]])))
    stop(gettext("Native coefficients or weights differ from the Weibull information target."))

  equations <- lapply(seq_len(components), function(k) {
    lapply(c("shape", "scale"), function(parameter) {
      native   <- if (components == 1L) parameter else paste0(parameter, k)
      baseline <- match(native, names(theta))
      equation <- specification[["equations"]][[native]]
      slopes   <- equation[["slopeIndices"]]
      design   <- fit[["data"]][["mml"]][[native]]
      if (length(slopes) == 0L)
        design <- matrix(1, nrow(response), 1L)
      if (is.na(baseline) || !is.matrix(design) || nrow(design) != nrow(response) ||
        ncol(design) != length(slopes) + 1L || any(!is.finite(design)) || any(design[, 1L] != 1))
        stop(gettext("Native design differs from the Weibull information target."))
      return(list(index = c(baseline, slopes), design = design))
    })
  })
  weightIndex <- if (components > 1L) match(paste0("v", seq_len(components - 1L)), names(theta)) else integer(0)
  if (anyNA(weightIndex))
    stop(gettext("Native mixing weights differ from the Weibull information target."))
  return(list(theta = theta, free = free, components = components, equations = equations,
    weightIndex = weightIndex, times = response[, "time"], events = response[, "status"], weights = weights))
}

.sapWeibullDerivatives <- function(target, theta = target[["theta"]]) {

  free       <- target[["free"]]
  size       <- length(free)
  components <- target[["components"]]
  weightIndex <- target[["weightIndex"]]
  stick       <- stats::plogis(theta[weightIndex])
  logStick    <- -pmax(-theta[weightIndex], 0) - log1p(exp(-abs(theta[weightIndex])))
  logRemain   <- -pmax(theta[weightIndex], 0) - log1p(exp(-abs(theta[weightIndex])))
  logWeights  <- vapply(seq_len(components), function(k)
    sum(logRemain[seq_len(k - 1L)]) + if (k < components) logStick[k] else 0, numeric(1))
  H <- correction <- matrix(0, size, size)
  score <- scoreCorrection <- numeric(size)
  contributions <- numeric(length(target[["times"]]))
  for (row in seq_along(contributions)) {
    time <- target[["times"]][row]
    if (time == 0 || target[["weights"]][row] == 0)
      next
    event <- target[["events"]][row]
    logs  <- rep(-Inf, components)
    gradients <- matrix(0, components, size)
    hessians  <- vector("list", components)
    for (k in seq_len(components)) {
      equations <- target[["equations"]][[k]]
      eta <- vapply(equations, function(equation)
        sum(theta[equation[["index"]]] * equation[["design"]][row, ]), numeric(1))
      shape <- exp(eta[1L])
      u     <- shape * (log(time) - eta[2L])
      z     <- exp(u)
      hessians[[k]] <- matrix(0, size, size)
      if (!is.finite(z) || !is.finite(u))
        next
      r       <- event - z
      logs[k] <- logWeights[k] + event * (eta[1L] - log(time) + u) - z
      J       <- matrix(0, 2L, length(theta))
      for (j in 1:2)
        J[j, equations[[j]][["index"]]] <- equations[[j]][["design"]][row, ]
      J <- J[, free, drop = FALSE]
      localScore <- c(event + r * u, -r * shape)
      localH <- matrix(c(-z * u^2 + r * u, shape * (z * u - r),
        shape * (z * u - r), -z * shape^2), 2L, 2L)
      gradients[k, ] <- as.vector(localScore %*% J)
      hessians[[k]]  <- crossprod(J, localH %*% J)
      if (length(weightIndex)) {
        # Stick j is passed by later components and selected by component j.
        active <- seq_len(min(k, components - 1L))
        index  <- match(weightIndex[active], free)
        keep   <- which(!is.na(index))
        index  <- index[keep]
        active <- active[keep]
        gradients[k, index] <- as.numeric(active == k) - stick[active]
        hessians[[k]][cbind(index, index)] <- -stick[active] * (1 - stick[active])
      }
    }
    maximum <- max(logs)
    if (!is.finite(maximum))
      return(list(logLik = -Inf, score = rep(NA_real_, size), H = matrix(NA_real_, size, size)))
    posterior <- exp(logs - maximum)
    normalizer <- sum(posterior)
    posterior  <- posterior / normalizer
    gradient   <- colSums(posterior * gradients)
    hessian    <- -tcrossprod(gradient)
    for (k in seq_len(components))
      hessian <- hessian + posterior[k] * (hessians[[k]] + tcrossprod(gradients[k, ]))
    weight <- target[["weights"]][row]
    contributions[row] <- weight * (maximum + log(normalizer))
    information <- -weight * hessian
    updated <- H + information
    correction <- correction + ifelse(abs(H) >= abs(information),
      (H - updated) + information, (information - updated) + H)
    H <- updated
    contribution <- weight * gradient
    updated <- score + contribution
    scoreCorrection <- scoreCorrection + ifelse(abs(score) >= abs(contribution),
      (score - updated) + contribution, (contribution - updated) + score)
    score <- updated
  }
  dimnames(H) <- list(names(theta)[free], names(theta)[free])
  return(list(logLik = sum(contributions), score = score + scoreCorrection, H = H + correction))
}

# Polish an already converged, unconstrained native search; finalization stays at its point.
.sapWeibullRefine <- function(fit, fitCall, specification, censoringType) {

  if ((!is.null(fitCall[["control"]][["maxit"]]) && fitCall[["control"]][["maxit"]] == 0) || !is.null(fitCall[["lower"]]) ||
    !is.null(fitCall[["upper"]]) || fit[["npars"]] == 0L || fit[["opt"]][["convergence"]] != 0)
    return(fit)
  target <- .sapWeibullTarget(fit, specification, censoringType)
  if (is.null(target))
    return(fit)
  theta <- target[["theta"]]
  free  <- target[["free"]]
  scale <- fitCall[["control"]][["parscale"]]
  if (is.null(scale))
    scale <- rep(1, length(free))
  initial <- .sapWeibullDerivatives(target, theta)
  if (!is.finite(initial[["logLik"]]) || any(!is.finite(initial[["score"]])) ||
    abs(initial[["logLik"]] - fit[["loglik"]]) > 1e-8 * max(1, abs(fit[["loglik"]])) ||
    max(abs(initial[["score"]] * scale)) > 0.01)
    return(fit)
  for (iteration in seq_len(8L)) {
    value <- .sapWeibullDerivatives(target, theta)
    H     <- value[["H"]] * outer(scale, scale)
    decomposition <- try(chol(H), silent = TRUE)
    if (inherits(decomposition, "try-error"))
      break
    step <- as.vector(chol2inv(decomposition) %*% (value[["score"]] * scale))
    if (any(!is.finite(step)) || max(abs(step)) < 1e-10)
      break
    accepted <- FALSE
    for (fraction in 2^-(0:20)) {
      proposal <- theta
      proposal[free] <- theta[free] + fraction * step * scale
      proposed <- .sapWeibullDerivatives(target, proposal)
      if (is.finite(proposed[["logLik"]]) && proposed[["logLik"]] >= value[["logLik"]]) {
        theta <- proposal
        accepted <- TRUE
        break
      }
    }
    if (!accepted)
      break
  }
  if (identical(theta, target[["theta"]]))
    return(fit)
  polishCall <- fitCall
  polishCall[["inits"]] <- as.numeric(theta)
  for (j in seq_along(fit[["dlist"]][["pars"]]))
    polishCall[["inits"]][j] <- fit[["dlist"]][["inv.transforms"]][[j]](theta[j])
  if (any(!is.finite(polishCall[["inits"]])))
    return(fit)
  polishCall[["hessian"]] <- FALSE
  polishCall[["control"]] <- list(maxit = 0)
  refined <- try(do.call(flexsurv::flexsurvreg, polishCall), silent = TRUE)
  if (inherits(refined, "try-error"))
    return(fit)
  .sapVerifyNativeSchema(refined, specification)
  if (!is.finite(refined[["loglik"]]) || refined[["loglik"]] < fit[["loglik"]] - 1e-9)
    return(fit)
  for (field in c("convergence", "counts", "message"))
    refined[["opt"]][[field]] <- fit[["opt"]][[field]]
  attr(refined, "analyticRefinement") <- list(iterations = iteration, initialLogLik = fit[["loglik"]],
    logLik = refined[["loglik"]], scaledScore = max(abs(.sapWeibullDerivatives(target, theta)[["score"]] * scale)))
  return(refined)
}
