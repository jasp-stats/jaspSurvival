# Native flexsurv 2.3.2 converts information to covariance with a relative nearPD
# threshold. In raw units that can change a valid inverse dramatically. Scaling
# optim steps alone cannot fix that conversion: fit the realized design columns
# themselves, then map the native point and complete covariance exactly once.
# Gompertz also needs a well-scaled time unit for its identity-linked shape.
.sapNativeBounds <- function(fitCall, free, name) {

  value <- fitCall[[name]]
  if (length(value) == 0L)
    return(rep(if (name == "lower") -Inf else Inf, length(free)))
  if (length(value) == 1L)
    return(rep(value, length(free)))
  if (length(value) != length(free))
    stop("Native optimization bounds do not match the free coefficient order.")
  return(value)
}

.sapNativeStandardization <- function(fitCall, schema, carrier) {

  gompertz <- identical(carrier[["dlist"]][["name"]], "gompertz")
  if (is.null(schema) || carrier[["npars"]] == 0L || (length(carrier[["covpars"]]) == 0L && !gompertz))
    return(NULL)
  parameterNames <- rownames(carrier[["res.t"]])
  n             <- length(parameterNames)
  free          <- setdiff(seq_len(n), fitCall[["fixedpars"]])
  lower         <- .sapNativeBounds(fitCall, free, "lower")
  upper         <- .sapNativeBounds(fitCall, free, "upper")
  J             <- G <- diag(n)
  dimnames(J)   <- dimnames(G) <- list(parameterNames, parameterNames)
  centers       <- stats::setNames(rep(0, n), parameterNames)
  scales        <- stats::setNames(rep(1, n), parameterNames)
  offset        <- stats::setNames(rep(0, n), parameterNames)
  original      <- schema[["fit"]]
  dataset       <- fitCall[["data"]]
  weights       <- original[["data"]][["m"]][["(weights)"]]
  if (nrow(original[["data"]][["m"]]) != nrow(dataset) ||
    !identical(rownames(original[["data"]][["m"]]), rownames(dataset)) ||
    length(weights) != nrow(dataset))
    stop("Native standardization cohort differs from the prepared equation cohort.")
  if (any(!is.finite(weights) | weights < 0) || !any(weights > 0))
    stop(gettext("Native design standardization requires finite nonnegative fitting weights with a positive total."))
  weights <- weights / max(weights)
  weights <- weights / sum(weights)
  scaledCall <- fitCall
  formulas   <- list()
  columns    <- character(0)
  for (native in names(schema[["specification"]][["equations"]])) {
    equation <- schema[["specification"]][["equations"]][[native]]
    matrix   <- original[["data"]][["mml"]][[native]]
    indices  <- equation[["slopeIndices"]]
    baseline <- equation[["baselineIndex"]]
    if (length(indices) == 0L)
      next
    if (nrow(matrix) != nrow(dataset) || ncol(matrix) != length(indices) + 1L ||
      !identical(rownames(matrix), rownames(dataset)) || any(!is.finite(matrix)))
      stop("Native standardization design does not match the prepared coefficient positions.")
    # A fixed or finitely bounded intercept must keep a rectangular feasible set.
    # Otherwise center BOTH fixed and free columns: fixed slopes contribute a
    # known intercept offset, which is essential for compensating large columns.
    baselineFree <- match(baseline, free)
    center <- !is.na(baselineFree) && !is.finite(lower[baselineFree]) && !is.finite(upper[baselineFree])
    synthetic <- character(length(indices))
    for (j in seq_along(indices)) {
      index   <- indices[j]
      column  <- matrix[, j + 1L]
      maximum <- max(abs(column))
      mean    <- if (!center) 0 else if (all(column[weights > 0] == column[which(weights > 0)[1L]]))
        column[which(weights > 0)[1L]] else if (maximum > 0) sum(weights * (column / maximum)) * maximum else 0
      shifted <- column - mean
      if (!is.finite(mean) || any(!is.finite(shifted)))
        stop(gettext("The standardized design exceeds the supported numerical range. Choose another predictor transformation or scale."))
      maximum <- max(abs(shifted))
      scale   <- if (maximum > 0) maximum * sqrt(sum(weights * (shifted / maximum)^2)) else 1
      if (is.finite(scale) && scale == 0)
        scale <- 1
      if (!is.finite(mean) || !is.finite(scale) || scale <= 0 || any(!is.finite(shifted / scale)))
        stop(gettext("The standardized design exceeds the supported numerical range. Choose another predictor transformation or scale."))
      centers[index] <- mean
      scales[index]  <- scale
      J[index, index]    <- 1 / scale
      J[baseline, index] <- -mean / scale
      G[index, index]    <- scale
      G[baseline, index] <- mean
      name <- paste0(".jaspNativeColumn", index)
      while (name %in% c(names(dataset), columns))
        name <- paste0(name, "_")
      synthetic[j] <- name
      columns      <- c(columns, name)
      dataset[[name]] <- shifted / scale
    }
    # Transform AFTER model.matrix: factor contrasts, interactions, polynomial
    # bases, spline knots and temperature transformations retain their cohort.
    formula <- equation[["formula"]]
    formula[[3L]] <- stats::reformulate(synthetic)[[2L]]
    formulas[[native]] <- formula
  }
  timeScale <- 1
  if (gompertz) {
    response <- stats::model.response(original[["data"]][["m"]])
    timeColumns <- seq_len(ncol(response) - 1L)
    times <- as.numeric(response[, timeColumns, drop = FALSE])
    timeScale <- max(abs(times[is.finite(times)]))
    if (timeScale == 0)
      timeScale <- 1
    response[, timeColumns] <- response[, timeColumns, drop = FALSE] / timeScale
    responseName <- ".jaspNativeTimeResponse"
    while (responseName %in% names(dataset))
      responseName <- paste0(responseName, "_")
    dataset[[responseName]] <- response
    shape <- c(match("shape", parameterNames), schema[["specification"]][["equations"]][["shape"]][["slopeIndices"]])
    # t* = t/c implies shape* = c*shape and log(rate*) = log(rate)+log(c).
    # Shape regression coefficients use the identity link and scale as well.
    J[, shape] <- J[, shape, drop = FALSE] / timeScale
    G[shape, ] <- G[shape, , drop = FALSE] * timeScale
    scales[shape] <- scales[shape] * timeScale
    offset[match("rate", parameterNames)] <- -log(timeScale)
  }
  if (all(centers == 0 & scales == 1) && timeScale == 1)
    return(NULL)
  location <- carrier[["dlist"]][["location"]]
  scaledCall[["formula"]] <- formulas[[location]]
  if (is.null(scaledCall[["formula"]]))
    scaledCall[["formula"]] <- fitCall[["formula"]]
  if (gompertz)
    scaledCall[["formula"]][[2L]] <- as.name(responseName)
  scaledCall[["anc"]] <- lapply(formulas[setdiff(names(formulas), location)], function(formula) formula[-2L])
  scaledCall[["data"]] <- dataset
  beta  <- carrier[["res.t"]][, "est"]
  for (name in c("lower", "upper"))
    if (length(fitCall[[name]]) > 0L)
      scaledCall[[name]] <- (.sapNativeBounds(fitCall, free, name) - offset[free]) * scales[free]
  scaledCall[["control"]][["parscale"]] <- rep(1, length(free))
  return(list(call = scaledCall, J = J, G = G, centers = centers, scales = scales, offset = offset, timeScale = timeScale,
    free = free, fixed = fitCall[["fixedpars"]], fixedValues = beta[fitCall[["fixedpars"]]]))
}

.sapNativeInitials <- function(linkPoint, metadata, G = NULL, offset = NULL) {

  if (!is.numeric(linkPoint) || length(linkPoint) != nrow(metadata[["res.t"]]) || any(!is.finite(linkPoint)))
    stop(gettext("The selected native link point is unavailable or does not match the fitted coefficient layout."))
  gamma <- if (is.null(offset)) linkPoint else linkPoint - offset
  gamma <- if (is.null(G)) gamma else as.vector(G %*% gamma)
  inits <- gamma
  for (i in metadata[["basepars"]])
    inits[i] <- metadata[["dlist"]][["inv.transforms"]][[i]](gamma[i])
  if (any(!is.finite(gamma)) || any(!is.finite(inits)) || any(!vapply(metadata[["basepars"]], function(i)
    is.finite(metadata[["dlist"]][["transforms"]][[i]](inits[i])), logical(1))))
    stop(gettext("The standardized initial baseline cannot be represented on the native parameter scale. Choose another predictor transformation or scale."))
  return(unname(inits))
}

# A numerical consistency check, not a test of Hessian accuracy or model
# identifiability. The product allowance admits rounding in a valid ill-
# conditioned inverse, while detecting native relative-eigenvalue projection.
.sapNativeCovarianceIssue <- function(H, V, p) {

  if (!is.matrix(H) || !is.matrix(V) || !identical(dim(H), c(p, p)) || !identical(dim(V), c(p, p)) ||
    any(!is.finite(H)) || any(!is.finite(V)))
    return(gettext("The native information or covariance matrix is unavailable or nonfinite."))
  if (p == 0L)
    return(NULL)
  if (!isSymmetric(H, tol = sqrt(.Machine$double.eps), check.attributes = FALSE) ||
    !isSymmetric(V, tol = sqrt(.Machine$double.eps), check.attributes = FALSE) ||
    any(diag(H) <= 0) || any(diag(V) <= 0))
    return(gettext("The native information or covariance matrix is not symmetric positive definite."))
  curvature <- sqrt(diag(H))
  R <- H / outer(curvature, curvature)
  S <- V * outer(curvature, curvature)
  if (any(!is.finite(R)) || any(!is.finite(S)))
    return(gettext("The normalized native information or covariance matrix is nonfinite."))
  eigenR <- tryCatch(eigen(R, symmetric = TRUE, only.values = TRUE)[["values"]], error = function(error) NULL)
  eigenV <- tryCatch(eigen(V, symmetric = TRUE, only.values = TRUE)[["values"]], error = function(error) NULL)
  if (is.null(eigenR) || is.null(eigenV) || any(!is.finite(eigenR)) || any(!is.finite(eigenV)) ||
    min(eigenR) <= 0 || min(eigenV) <= 0)
    return(gettext("The native information or covariance matrix is not positive definite."))
  residual  <- norm(R %*% S - diag(p), "I")
  allowance <- max(p * sqrt(.Machine$double.eps),
    10 * p * .Machine$double.eps * norm(abs(R) %*% abs(S), "I"))
  if (!is.finite(residual) || !is.finite(allowance) || residual > allowance)
    return(gettext("The native covariance matrix is inconsistent with the observed information. Standard errors and confidence intervals are unavailable."))
  return(NULL)
}

.sapNativeUnavailableInference <- function(fit, reason) {

  names <- rownames(fit[["res.t"]])
  free  <- setdiff(seq_along(names), fit[["fixedpars"]])
  fit[["cov"]] <- matrix(NA_real_, length(free), length(free), dimnames = list(names[free], names[free]))
  for (field in c("res", "res.t"))
    fit[[field]][, colnames(fit[[field]]) != "est"] <- NA_real_
  attr(fit, "inferenceUnavailable") <- reason
  return(fit)
}

.sapNativeDiagnosticHessian <- function(fit) {

  provenance <- attr(fit, "informationProvenance", exact = TRUE)
  if (identical(provenance[["coordinates"]], "standardizedDesign"))
    return(provenance[["nativeH"]])
  return(fit[["opt"]][["hessian"]])
}

.sapNativeRun <- function(fitCall) {

  requestedHessian <- fitCall[["hessian"]]
  searchCall <- fitCall
  searchCall[["hessian"]] <- FALSE
  point <- do.call(flexsurv::flexsurvreg, searchCall)
  fit   <- point
  provenance <- list(nativeH = NULL, nativeAttempt = FALSE, nativeCondition = NULL, nativeWarnings = character(0))
  finalControl <- NULL
  if (requestedHessian && point[["npars"]] > 0L) {
    provenance[["nativeAttempt"]] <- TRUE
    finalCall <- fitCall
    finalCall[["inits"]] <- unname(point[["res"]][, "est"])
    finalCall[["method"]] <- "BFGS"
    finalCall[["lower"]] <- finalCall[["upper"]] <- NULL
    # Keep search and native same-point Hessian stages separate. In particular,
    # Eyring's 1e-5 search differences must not replace native default 1e-3
    # Hessian steps, where nested numerical differences otherwise cancel.
    finalCall[["control"]] <- list(maxit = 0)
    if (!is.null(fitCall[["control"]][["parscale"]]))
      finalCall[["control"]][["parscale"]] <- fitCall[["control"]][["parscale"]]
    finalControl <- finalCall[["control"]]
    covarianceFunction <- get(".hess_to_cov", envir = asNamespace("flexsurv"), inherits = FALSE)
    fit <- tryCatch(withCallingHandlers(do.call(flexsurv::flexsurvreg, finalCall),
      warning = function(warning) {
        provenance[["nativeWarnings"]] <<- unique(c(provenance[["nativeWarnings"]], conditionMessage(warning)))
        invokeRestart("muffleWarning")
      }, error = function(error) {
        frames <- which(vapply(seq_len(sys.nframe()), function(frame)
          identical(sys.function(frame), covarianceFunction), logical(1)))
        if (length(frames) > 0L)
          provenance[["nativeH"]] <<- get("hessian", envir = sys.frame(frames[length(frames)]), inherits = FALSE)
      }), error = function(error) {
        provenance[["nativeCondition"]] <<- conditionMessage(error)
        failed <- .sapNativeUnavailableInference(point,
          gettextf("Native covariance calculation failed: %1$s. Standard errors and confidence intervals are unavailable.", conditionMessage(error)))
        failed[["opt"]]["hessian"] <- list(provenance[["nativeH"]])
        return(failed)
      })
    .sapmCheckFinalPoint(fit, unname(point[["res"]][, "est"]), point[["loglik"]])
    # Native analytic Hessians can simplify to a scalar with one free parameter.
    if (fit[["npars"]] == 1L && is.numeric(fit[["opt"]][["hessian"]]) && length(fit[["opt"]][["hessian"]]) == 1L)
      fit[["opt"]][["hessian"]] <- matrix(fit[["opt"]][["hessian"]], 1L, 1L)
    if (is.null(provenance[["nativeCondition"]]))
      provenance["nativeH"] <- list(fit[["opt"]][["hessian"]])
    if (is.null(attr(fit, "inferenceUnavailable", exact = TRUE))) {
      issue <- .sapNativeCovarianceIssue(provenance[["nativeH"]], fit[["cov"]], as.integer(fit[["npars"]]))
      if (!is.null(issue))
        fit <- .sapNativeUnavailableInference(fit, issue)
    }
  }
  # Retain the real search outcome, not maxit=0 finalization diagnostics.
  for (field in c("convergence", "counts", "message", "value"))
    fit[["opt"]][[field]] <- point[["opt"]][[field]]
  for (field in c("loglik", "AIC", "BIC"))
    fit[[field]] <- point[[field]]
  return(list(fit = fit, provenance = provenance, finalControl = finalControl))
}

.sapNativeOriginalFit <- function(carrier, selected, standardization) {

  free  <- standardization[["free"]]
  J     <- standardization[["J"]][free, free, drop = FALSE]
  G     <- standardization[["G"]][free, free, drop = FALSE]
  names <- rownames(carrier[["res.t"]])
  # The full point map includes the known fixed-coordinate offset. Covariance
  # uses Jff alone, because fixed coordinates carry no uncertainty; G is built
  # explicitly from means/scales rather than solving an ill-conditioned J.
  beta <- as.vector(standardization[["J"]] %*% selected[["res.t"]][, "est"]) + standardization[["offset"]]
  beta[standardization[["fixed"]]] <- standardization[["fixedValues"]]
  carrier[["res.t"]][, ] <- NA_real_
  carrier[["res.t"]][, "est"] <- beta
  H <- selected[["opt"]][["hessian"]]
  carrier[["opt"]] <- selected[["opt"]]
  carrier[["opt"]][["par"]] <- stats::setNames(beta[free], names[free])
  carrier[["opt"]]["hessian"] <- list(if (is.matrix(H) && identical(dim(H), c(length(free), length(free)))) t(G) %*% H %*% G else NULL)
  carrier[["cov"]] <- matrix(NA_real_, length(free), length(free), dimnames = list(names[free], names[free]))
  unavailable <- attr(selected, "inferenceUnavailable", exact = TRUE)
  if (is.null(unavailable) && is.matrix(selected[["cov"]]) && all(is.finite(selected[["cov"]]))) {
    V <- J %*% selected[["cov"]] %*% t(J)
    # Symmetrize multiplication roundoff only. Raw native normboot clips negative
    # eigenvalues, so withhold inference if mapping loses positive definiteness;
    # do not project, floor eigenvalues, or call nearPD again in original units.
    if (all(is.finite(V)) && isSymmetric(V, tol = sqrt(.Machine$double.eps), check.attributes = FALSE)) {
      V <- (V + t(V)) / 2
      eigenvalues <- tryCatch(eigen(V, symmetric = TRUE, only.values = TRUE)[["values"]], error = function(error) NULL)
      if (all(diag(V) > 0) && !is.null(eigenvalues) && all(is.finite(eigenvalues)) && min(eigenvalues) > 0) {
        dimnames(V) <- list(names[free], names[free])
        carrier[["cov"]] <- V
        se <- sqrt(diag(V))
        z  <- stats::qnorm(1 - (1 - selected[["cl"]]) / 2)
        carrier[["res.t"]][free, "se"] <- se
        carrier[["res.t"]][free, 2L] <- beta[free] - z * se
        carrier[["res.t"]][free, 3L] <- beta[free] + z * se
      } else
        unavailable <- gettext("The covariance matrix cannot be represented as positive definite in the original coefficient units. Standard errors and confidence intervals are unavailable.")
    } else
      unavailable <- gettext("The covariance matrix cannot be represented in the original coefficient units. Standard errors and confidence intervals are unavailable.")
  }
  carrier[["res"]] <- carrier[["res.t"]]
  for (i in carrier[["basepars"]]) {
    carrier[["res"]][i, 1:3] <- carrier[["dlist"]][["inv.transforms"]][[i]](carrier[["res.t"]][i, 1:3])
    if (identical(body(carrier[["dlist"]][["transforms"]][[i]]), body(log)))
      carrier[["res"]][i, "se"] <- exp(beta[i]) * carrier[["res.t"]][i, "se"]
    else if (!identical(carrier[["dlist"]][["transforms"]][[i]], identity))
      carrier[["res"]][i, "se"] <- NA_real_
  }
  # Preserve the supplied natural fixed values exactly, including rounding of
  # log/inverse-log pairs. Fixed inference stays missing on both reporting scales.
  carrier[["res"]][standardization[["fixed"]], "est"] <- attr(standardization, "originalFixedNatural")
  carrier[["coefficients"]] <- stats::setNames(beta, names)
  if (standardization[["timeScale"]] != 1) {
    events <- carrier[["data"]][["Y"]][, "status"] == 1
    weights <- carrier[["data"]][["m"]][["(weights)"]]
    # Only exact failures contribute a time-density Jacobian; censoring and
    # truncation terms are probabilities, including counting-process entry.
    correction <- sum(weights * events) * log(standardization[["timeScale"]])
    selected[["loglik"]] <- selected[["loglik"]] - correction
    selected[["logliki"]] <- selected[["logliki"]] - events * log(standardization[["timeScale"]])
    selected[["AIC"]] <- selected[["AIC"]] + 2 * correction
    selected[["BIC"]] <- selected[["BIC"]] + 2 * correction
    carrier[["opt"]][["value"]] <- selected[["opt"]][["value"]] + correction
  }
  for (field in c("loglik", "logliki", "AIC", "BIC", "cl"))
    carrier[[field]] <- selected[[field]]
  if (!is.null(unavailable))
    carrier <- .sapNativeUnavailableInference(carrier, unavailable)
  return(carrier)
}

.sapNativeFit <- function(fitCall, schema, censoringType, searchProvenance = NULL, selectedPoint = NULL) {

  requestedHessian <- fitCall[["hessian"]]
  # The carrier describes original formula/data/subset/weights at a VALID INITIAL
  # point. A selected raw natural baseline may underflow/overflow despite finite
  # conditional predictions; never feed that 0/Inf back into native initialization.
  # NEW raw natural warm starts retain native's representability limit. Repeated
  # finalization instead keeps this valid seed and a separate full selected LINK
  # point; only its standardized native baseline needs to be representable.
  standardization <- NULL
  carrier <- NULL
  hasDesign <- !is.null(schema) && length(schema[["fit"]][["covpars"]]) > 0L &&
    length(fitCall[["fixedpars"]]) < nrow(schema[["fit"]][["res.t"]])
  hasTimeScale <- !is.null(schema) && identical(schema[["fit"]][["dlist"]][["name"]], "gompertz")
  if (hasDesign || hasTimeScale || (!is.null(selectedPoint) && is.null(schema))) {
    carrierCall <- fitCall
    carrierCall[["method"]] <- "BFGS"
    carrierCall[["lower"]] <- carrierCall[["upper"]] <- NULL
    carrierCall[["control"]] <- list(maxit = 0)
    carrierCall[["hessian"]] <- FALSE
    carrier <- do.call(flexsurv::flexsurvreg, carrierCall)
    if (!is.null(schema))
      .sapVerifyNativeSchema(carrier, schema[["specification"]])
    standardization <- .sapNativeStandardization(fitCall, schema, carrier)
  }
  nativeCall <- if (is.null(standardization)) fitCall else standardization[["call"]]
  if (!is.null(standardization) || !is.null(selectedPoint)) {
    metadata <- if (!is.null(carrier)) carrier else schema[["fit"]]
    # Transform the actual selected link point directly. In particular, never
    # log an underflowed/overflowed natural report or require the metadata seed's
    # transformed point to initialize this separate finalization.
    linkPoint <- if (is.null(selectedPoint)) metadata[["res.t"]][, "est"] else selectedPoint
    nativeCall[["inits"]] <- .sapNativeInitials(linkPoint, metadata,
      if (is.null(standardization)) NULL else standardization[["G"]],
      if (is.null(standardization)) NULL else standardization[["offset"]])
  }
  if (is.null(nativeCall[["method"]]))
    nativeCall[["method"]] <- "BFGS"
  native <- .sapNativeRun(nativeCall)
  fit    <- native[["fit"]]
  provenance <- native[["provenance"]]
  provenance[["searchMethod"]] <- nativeCall[["method"]]
  provenance[["finalMethod"]] <- if (!is.null(native[["finalControl"]])) "BFGS" else NULL
  if (!is.null(standardization)) {
    if (nrow(fit[["res.t"]]) != nrow(carrier[["res.t"]]) ||
      !identical(fit[["mx"]], carrier[["mx"]]) || !identical(fit[["covpars"]], carrier[["covpars"]]))
      stop("Standardized native coefficient positions differ from the original equations.")
    fixed <- standardization[["fixed"]]
    originalFixed <- carrier[["res"]][fixed, "est"]
    suppliedFixed <- attr(fitCall, "nativeFixedValues", exact = TRUE)
    if (length(suppliedFixed) > 0L)
      originalFixed[match(names(suppliedFixed), rownames(carrier[["res"]])[fixed])] <- suppliedFixed
    attr(standardization, "originalFixedNatural") <- originalFixed
    provenance[["standardizedPoint"]] <- fit[["res.t"]][, "est"]
    provenance[["J"]] <- standardization[["J"]]
    provenance[["G"]] <- standardization[["G"]]
    provenance[["centers"]] <- standardization[["centers"]]
    provenance[["scales"]] <- standardization[["scales"]]
    provenance[["offset"]] <- standardization[["offset"]]
    provenance[["timeScale"]] <- standardization[["timeScale"]]
    provenance[["fixed"]] <- standardization[["fixed"]]
    provenance[["originalFixedValues"]] <- standardization[["fixedValues"]]
    provenance[["coordinates"]] <- "standardizedDesign"
    fit <- .sapNativeOriginalFit(carrier, fit, standardization)
  } else
    provenance[["coordinates"]] <- "originalDesign"
  if (!is.null(schema))
    .sapVerifyNativeSchema(fit, schema[["specification"]])
  if (is.null(searchProvenance))
    searchProvenance <- list(opt = fit[["opt"]], method = nativeCall[["method"]], control = nativeCall[["control"]],
      loglik = fit[["loglik"]], AIC = fit[["AIC"]], BIC = fit[["BIC"]])
  else {
    .sapmCheckFinalPoint(fit, if (is.null(selectedPoint)) fitCall[["inits"]] else selectedPoint,
      searchProvenance[["loglik"]], link = !is.null(selectedPoint))
    for (field in c("convergence", "counts", "message", "value"))
      fit[["opt"]][[field]] <- searchProvenance[["opt"]][[field]]
    for (field in c("loglik", "AIC", "BIC"))
      fit[[field]] <- searchProvenance[[field]]
  }
  attr(fit, "nativeHessianWarning") <- length(provenance[["nativeWarnings"]]) > 0L ||
    !is.null(attr(fit, "inferenceUnavailable", exact = TRUE))
  outcome <- if (fit[["npars"]] == 0L) "fullyFixed" else if (!requestedHessian) "nativeNoH" else
    if (!is.null(attr(fit, "inferenceUnavailable", exact = TRUE))) "nativeUnavailable" else "nativeAvailable"
  fit <- .sapInformationRecord(fit, "native", outcome, provenance)
  attr(fit, "searchProvenance") <- searchProvenance
  attr(fit, "searchControl") <- searchProvenance[["control"]]
  attr(fit, "nativeControl") <- nativeCall[["control"]]
  attr(fit, "finalControl") <- native[["finalControl"]]
  attr(fit, "requestedHessian") <- requestedHessian
  return(fit)
}
