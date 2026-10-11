# Same-point exact-density information for canonical Eyring GenF.orig fits.
.sapInformationPolicyVersion <- "eyring-genf-orig-density-1"
.sapGenfOrigBernoulli <- c(1/12, -1/120, 1/252, -1/240, 1/132, -691/32760,
  1/12, -3617/8160, 43867/14364, -174611/6600, 854513/3036, -236364091/65520)

.sapGenfOrigSoftplus <- function(value) {
  return(max(value, 0) + log1p(exp(-abs(value))))
}
.sapGenfOrigLogisticPair <- function(value) {
  exponential <- exp(-abs(value))
  return(if (value >= 0) c(1, exponential) / (1 + exponential) else c(exponential, 1) / (1 + exponential))
}
.sapGenfOrigAsymptoticSK <- function(shape, other) {
  logarithm <- log1p(other / shape)
  h <- other / (shape + other)
  g <- shape / (shape + other)
  termsS <- termsK <- numeric(12L)
  for (k in seq_len(12L)) {
    r <- 2 * k
    difference <- -expm1(-r * logarithm)
    exponential <- exp(-r * logarithm)
    common <- .sapGenfOrigBernoulli[k] * shape^(1 - r)
    termsS[k] <- common * difference
    termsK[k] <- common * ((1 - r) * difference - r * h * exponential)
  }
  return(c(S = sum(c(.5 * h, termsS)), K = sum(c(-.5 * g * h, termsK))))
}
.sapGenfOrigStableSK <- function(shape, other) {
  # Shift small shapes into the asymptotic range, then undo the shift by recurrence.
  if (shape >= 8) return(.sapGenfOrigAsymptoticSK(shape, other))
  m <- ceiling(8 - shape)
  n <- shape + m
  total <- shape + other
  sk <- .sapGenfOrigAsymptoticSK(n, other)
  h <- other / total
  g <- shape / total
  v <- m / n
  difference <- if (h * v < .5) log1p(-h * v) else log(g + h * shape / n)
  F <- (shape / n) * sk[1L]
  G <- shape * difference
  termsS <- termsK <- numeric(m)
  for (j in 0:(m - 1L)) {
    term <- (shape / (shape + j)) * (other / (total + j))
    termsS[j + 1L] <- term
    termsK[j + 1L] <- term * (j / (shape + j) - shape / (total + j))
  }
  return(c(S = sum(c(F, G, termsS)),
    K = sum(c(F, (shape/n)^2 * (sk[2L] - sk[1L]), G, (shape/n) * h * m * (1 + shape/(total + m)), termsK))))
}
.sapGenfOrigStableE <- function(shape, other, proportion, complement) {
  total <- shape + other
  if (total >= 8) {
    bracket <- sum(c(.5, vapply(seq_len(12L), function(k)
      2 * k * .sapGenfOrigBernoulli[k] * total^(1 - 2*k), numeric(1))))
    return(proportion * complement * bracket)
  }
  m <- ceiling(8 - total)
  n <- total + m
  value <- sum(c(total * total * trigamma(n), vapply(0:(m - 1L), function(j) (total/(total + j))^2, numeric(1))))
  return(proportion * complement * (value - total))
}
.sapGenfOrigLog1pmxBracket <- function(value) {
  y <- value / (2 + value)
  summation <- sum(vapply(0:19, function(j) y^(2*j)/(2*j + 3), numeric(1)))
  return(-1/(2 + value) + 2*y/(2 + value)^2 * summation)
}
.sapGenfOrigCoreValue <- function(shape, value, signedResidual, residual, b, c, z, parameter) {
  if (abs(value) <= .5)
    return((signedResidual * value) * .sapGenfOrigLog1pmxBracket(value))
  return(if (parameter == "b") residual + shape * (.sapGenfOrigSoftplus(c - b) - .sapGenfOrigSoftplus(-z)) else
    -residual + shape * (.sapGenfOrigSoftplus(b - c) - .sapGenfOrigSoftplus(z)))
}
.sapGenfOrigScaledLog1pmx <- function(shape, otherShape, logShape, logValue) {
  if (logValue <= log(.5)) {
    value <- exp(logValue)
    return(exp(logShape + 2*logValue) * .sapGenfOrigLog1pmxBracket(value))
  }
  return(shape * .sapGenfOrigSoftplus(logValue) - otherShape)
}
.sapGenfOrigScaledD <- function(shape, logShape, logValue) {
  if (logValue <= log(.5)) {
    value <- exp(logValue)
    return(exp(logShape + 2*logValue) * sum(c(.sapGenfOrigLog1pmxBracket(value), 1/(1 + value))))
  }
  pair <- .sapGenfOrigLogisticPair(logValue)
  return(shape * (.sapGenfOrigSoftplus(logValue) - pair[1L]))
}
.sapGenfOrigUnitCoefficients <- function(shape) {
  x <- shape + 1
  if (shape < 8) {
    values <- vapply(seq_len(13L), function(n) psigamma(x, deriv = n), numeric(1))
    return(list(g = c(shape * values[1L] - 1, shape * values[2:12]),
      t = vapply(seq_len(12L), function(n) sum(c(shape * values[n], shape * shape * values[n + 1L])), numeric(1))))
  }
  u <- shape / x
  v <- 1 / x
  bernoulli <- 2 * seq_len(12L) * .sapGenfOrigBernoulli
  g <- t <- numeric(12L)
  g[1L] <- sum(c((u/2 - 1)*v, vapply(seq_len(12L), function(k) bernoulli[k]*u*v^(2*k), numeric(1))))
  t[1L] <- sum(c(u*v, u*(.5 - u)*v, vapply(seq_len(12L), function(k) bernoulli[k]*u*(1 - (2*k + 1)*u)*v^(2*k), numeric(1))))
  for (n in 2:12) {
    factor <- (-1)^(n - 1) * u * v^(n - 1)
    terms <- vapply(seq_len(12L), function(k) bernoulli[k]*(factorial(2*k + n - 1)/factorial(2*k))*v^(2*k), numeric(1))
    g[n] <- factor * sum(c(factorial(n - 1), factorial(n)*(v/2), terms))
    t[n] <- factor * sum(c(factorial(n - 1)*(1 - n*u), factorial(n)*(v/2)*(1 - (n + 1)*u), terms*(1 - (2*seq_len(12L) + n)*u)))
  }
  return(list(g = g, t = t))
}
.sapGenfOrigStableGT <- function(shape, other, b, c, S, K) {
  delta <- expm1(c)
  if (abs(delta) <= .125) {
    if (delta == 0) return(c(G = 0, T = 0))
    coefficients <- .sapGenfOrigUnitCoefficients(shape)
    powers <- vapply(seq_len(12L), function(n) delta^n/factorial(n), numeric(1))
    return(c(G = sum(powers * coefficients[["g"]]), T = sum(powers * coefficients[["t"]])))
  }
  return(c(G = sum(c(S, .sapGenfOrigScaledLog1pmx(shape, other, b, c - b))),
    T = sum(c(K, .sapGenfOrigScaledD(shape, b, c - b)))))
}
.sapGenfOrigChooseSum <- function(original, alternative) {
  # Use the equivalent expression with smaller terms to reduce cancellation.
  originalTotal <- sum(abs(original))
  alternativeTotal <- sum(abs(alternative))
  if (is.na(originalTotal) || is.na(alternativeTotal)) return(NA_real_)
  return(sum(if (alternativeTotal < originalTotal) alternative else original))
}
.sapGenfOrigTailProduct <- function(residual, logProbability) {
  return(if (residual == 0) 0 else sign(residual) * exp(log(abs(residual)) + logProbability))
}
.sapGenfOrigDensityDerivatives <- function(logTime, point) {
  # Differentiate in the optimizer's coordinates: mu, log(sigma), log(s1), log(s2).
  mu <- point[1L]
  a <- point[2L]
  b <- point[3L]
  c <- point[4L]
  sigma <- exp(a)
  A <- exp(b)
  B <- exp(c)
  C <- A + B
  q <- (logTime - mu) / sigma
  if (any(!is.finite(c(sigma, A, B, C, q))) || any(c(sigma, A, B) == 0))
    return(list(score = rep(NA_real_, 4L), HessianLogDensity = matrix(NA_real_, 4L, 4L)))
  logC <- max(b, c) + log1p(exp(-abs(b - c)))
  z <- b - c + q
  shapePair <- .sapGenfOrigLogisticPair(b - c)
  lp <- -.sapGenfOrigSoftplus(-z)
  lpc <- -.sapGenfOrigSoftplus(z)
  residual <- if (q < 0) exp(b + lpc) * (-expm1(q)) else exp(c + lp) * expm1(-q)
  D <- -exp(logC + lp + lpc)
  sk <- .sapGenfOrigStableSK(A, B)
  skb <- .sapGenfOrigStableSK(B, A)
  E <- .sapGenfOrigStableE(A, B, shapePair[1L], shapePair[2L])
  coreB <- .sapGenfOrigCoreValue(A, -residual/A, -residual, residual, b, c, z, "b")
  coreC <- .sapGenfOrigCoreValue(B, residual/B, residual, residual, b, c, z, "c")
  V <- residual * (residual/C)
  gt <- .sapGenfOrigStableGT(A, B, b, c, sk[1L], sk[2L])
  gtb <- .sapGenfOrigStableGT(B, A, c, b, skb[1L], skb[2L])
  Ad <- .sapGenfOrigScaledD(A, b, -z)
  Bd <- .sapGenfOrigScaledD(B, c, z)
  Bpc <- exp(c + lpc)
  Ap <- exp(b + lp)
  pcR <- .sapGenfOrigTailProduct(residual, lpc)
  pR <- .sapGenfOrigTailProduct(residual, lp)
  score <- c(-residual/sigma, -1 - residual*q,
    .sapGenfOrigChooseSum(c(coreB, sk[1L]), c(Bpc, -Ad, gt[1L])),
    .sapGenfOrigChooseSum(c(coreC, skb[1L]), c(Ap, -Bd, gtb[1L])))
  H <- matrix(0, 4L, 4L)
  H[1L, 1L] <- D/sigma/sigma
  H[2L, 2L] <- sum(c(D*q*q, residual*q))
  H[1L, 2L] <- H[2L, 1L] <- sum(c(D*q, residual))/sigma
  H[1L, 3L] <- H[3L, 1L] <- -pcR/sigma
  H[1L, 4L] <- H[4L, 1L] <- -pR/sigma
  H[2L, 3L] <- H[3L, 2L] <- -q*pcR
  H[2L, 4L] <- H[4L, 2L] <- -q*pR
  H[3L, 3L] <- .sapGenfOrigChooseSum(c(coreB, V, sk[2L]), c(pcR, -Ad, gt[2L]))
  H[4L, 4L] <- .sapGenfOrigChooseSum(c(coreC, V, skb[2L]), c(-pR, -Bd, gtb[2L]))
  H[3L, 4L] <- H[4L, 3L] <- E - V
  return(list(score = score, HessianLogDensity = H))
}

.sapGenfOrigCanonical <- function(fit) {
  canonical <- flexsurv::flexsurv.dists[["genf.orig"]]
  fields <- setdiff(names(canonical), "inits")
  if (!identical(names(fit[["dlist"]]), names(canonical)) ||
      !identical(fit[["dlist"]][fields], canonical[fields])) return(FALSE)
  functions <- c(d = "dgenf.orig", p = "pgenf.orig", h = "hgenf.orig", H = "Hgenf.orig", r = "rgenf.orig",
    q = "qgenf.orig", rmst = "rmst_genf.orig", mean = "mean_genf.orig")
  if (!all(vapply(names(functions), function(name)
      identical(fit[["dfns"]][[name]], getExportedValue("flexsurv", functions[[name]])), logical(1)))) return(FALSE)
  if (!all(vapply(c("DLd", "DLS", "D2Ld", "D2LS"), function(name)
      name %in% names(fit[["dfns"]]) && is.null(fit[["dfns"]][[name]]), logical(1)))) return(FALSE)
  override <- fit[["call"]][["dfns"]]
  return(is.null(override) || identical(override, quote(NULL)))
}
.sapGenfOrigTarget <- function(fit, specification, censoringType) {
  response <- fit[["data"]][["m"]][[1L]]
  if (censoringType != "right" || !inherits(response, "Surv") || !identical(attr(response, "type"), "right") ||
      any(response[, "status"] != 1) || any(!is.finite(response[, "time"]) | response[, "time"] <= 0) ||
      !.sapGenfOrigCanonical(fit) || any(c("(bhazard)", "(rtrunc)") %in% names(fit[["data"]][["m"]]))) return(NULL)
  .sapVerifyNativeSchema(fit, specification)
  coefficientNames <- specification[["parameterNames"]]
  if (!identical(coefficientNames[seq_along(fit[["dlist"]][["pars"]])], fit[["dlist"]][["pars"]]))
    stop("Native baseline order differs from the information target.")
  theta <- fit[["res.t"]][, "est"]
  fixed <- fit[["fixedpars"]]
  if (anyNA(fixed) || anyDuplicated(fixed) || any(!fixed %in% seq_along(theta)) ||
      !identical(rownames(fit[["res"]]), coefficientNames) || any(!is.finite(theta)))
    stop("Native fitted coefficients or fixes differ from the information target.")
  compiledFixed <- specification[["fixed"]]
  compiledIndices <- match(names(compiledFixed), coefficientNames)
  if (anyNA(compiledIndices) || anyDuplicated(compiledIndices) || any(!compiledIndices %in% fixed) ||
      !identical(unname(theta[compiledIndices]), as.numeric(compiledFixed)))
    stop("Native fixed equation coefficients differ from the information target.")
  recordedFixed <- attr(fit, "restrictParametersValues", exact = TRUE)
  recordedIndices <- match(names(recordedFixed), coefficientNames)
  if (anyNA(recordedIndices) || anyDuplicated(recordedIndices) || any(!recordedIndices %in% fixed))
    stop("Native fixed parameter values differ from the information target.")
  recordedLink <- as.numeric(recordedFixed)
  for (i in seq_along(recordedIndices))
    if (recordedIndices[i] <= length(fit[["dlist"]][["pars"]]))
      recordedLink[i] <- fit[["dlist"]][["transforms"]][[recordedIndices[i]]](recordedFixed[i])
  if (!identical(unname(theta[recordedIndices]), recordedLink))
    stop("Native fixed parameter values differ from the information target.")
  free <- setdiff(seq_along(theta), fixed)
  if (!identical(as.integer(fit[["optpars"]]), as.integer(free)) || fit[["npars"]] != length(free))
    stop("Native free coefficients differ from the information target.")
  m <- fit[["data"]][["m"]]
  Y <- fit[["data"]][["Y"]]
  weights <- m[["(weights)"]]
  if (!is.numeric(weights) || length(weights) != nrow(response) || any(!is.finite(weights) | weights < 0) ||
      !any(weights > 0) || nrow(m) != nrow(response) || nrow(Y) != nrow(response) ||
      (!is.null(rownames(response)) && !identical(rownames(m), rownames(response))) ||
      !identical(rownames(Y), rownames(m)) || !identical(unname(Y[, "time"]), unname(response[, "time"])) ||
      !identical(unname(Y[, "status"]), unname(response[, "status"])))
    stop("Native response rows or fitting weights differ from the information target.")
  equations <- lapply(c("mu", "sigma", "s1", "s2"), function(parameter) {
    equation <- specification[["equations"]][[parameter]]
    slopes <- fit[["covpars"]][fit[["mx"]][[parameter]]]
    native <- fit[["data"]][["mml"]][[parameter]]
    if (is.null(equation) || is.null(native)) {
      if (length(slopes) || !is.null(native) ||
          (!is.null(equation) && (length(equation[["slopeNames"]]) || length(equation[["slopeIndices"]]))))
        stop("A native parameter equation is missing from the information target.")
      if (!is.null(equation)) {
        prepared <- equation[["matrix"]]
        if (!is.matrix(prepared) || ncol(prepared) != 1L || nrow(prepared) != nrow(m) ||
            !identical(rownames(prepared), rownames(m)) || any(!is.finite(prepared)) || any(prepared[, 1L] != 1))
          stop("A constant native equation differs from the information target.")
      }
      design <- matrix(numeric(0), nrow(response), 0L)
    } else {
      prepared <- equation[["matrix"]]
      if (is.null(native) || !identical(dim(native), dim(prepared)) ||
          !identical(dimnames(native), dimnames(prepared)) || !identical(as.vector(native), as.vector(prepared)) ||
          !identical(rownames(native), rownames(m)) || any(!is.finite(native)) || any(native[, 1L] != 1) ||
          ncol(native) != length(slopes) + 1L ||
          !identical(as.integer(slopes), as.integer(equation[["slopeIndices"]])))
        stop("Native equation rows or coefficient maps differ from the information target.")
      design <- native[, -1L, drop = FALSE]
    }
    return(list(baseline = match(parameter, coefficientNames), slopes = slopes, design = design))
  })
  return(list(theta = theta, names = coefficientNames, free = free, fixed = fixed,
    times = response[, "time"], logTimes = log(response[, "time"]), weights = weights, equations = equations))
}
.sapGenfOrigInformation <- function(target) {
  theta <- target[["theta"]]
  free <- target[["free"]]
  size <- length(free)
  rows <- length(target[["weights"]])
  information <- array(0, c(rows, size, size))
  for (row in seq_len(rows)) {
    eta <- numeric(4L)
    J <- matrix(0, 4L, size)
    for (k in seq_len(4L)) {
      equation <- target[["equations"]][[k]]
      eta[k] <- sum(c(theta[equation[["baseline"]]], equation[["design"]][row, ] * theta[equation[["slopes"]]]))
      columns <- numeric(length(theta))
      columns[equation[["baseline"]]] <- 1
      columns[equation[["slopes"]]] <- equation[["design"]][row, ]
      J[k, ] <- columns[free]
    }
    value <- .sapGenfOrigDensityDerivatives(target[["logTimes"]][row], eta)
    # Map the four distribution parameters back to the free regression coefficients.
    for (i in seq_len(size)) for (j in seq_len(size))
      information[row, i, j] <- -target[["weights"]][row] * sum(outer(J[, i], J[, j]) * value[["HessianLogDensity"]])
  }
  H <- matrix(0, size, size)
  for (i in seq_len(size)) for (j in seq_len(size)) H[i, j] <- sum(information[, i, j])
  dimnames(H) <- list(target[["names"]][free], target[["names"]][free])
  return(H)
}
.sapInformationRecord <- function(fit, method, outcome, provenance) {
  attr(fit, "informationPolicyVersion") <- .sapInformationPolicyVersion
  attr(fit, "informationMethod") <- method
  attr(fit, "informationOutcome") <- outcome
  attr(fit, "informationProvenance") <- provenance
  return(fit)
}
.sapInformationNote <- function(fit) {
  if (identical(attr(fit, "informationOutcome", exact = TRUE), "analyticAvailable"))
    return(gettext("Standard errors and confidence intervals use analytic exact-event information at the fitted Generalized F (original) Eyring estimates."))
  return(NULL)
}
.sapInformationSignature <- function(fit) {
  if (inherits(fit, "try-error")) return(as.character(fit))
  return(list(cov = fit[["cov"]], res = fit[["res"]], resT = fit[["res.t"]],
    unavailable = attr(fit, "inferenceUnavailable", exact = TRUE),
    methodNote = .sapInformationNote(fit),
    nativeWarning = attr(fit, "nativeHessianWarning", exact = TRUE), mixture = attr(fit, "mixture", exact = TRUE)["hessianWarning"]))
}
.sapAssessInformation <- function(fit, censoringType) {
  if (identical(attr(fit, "informationPolicyVersion", exact = TRUE), .sapInformationPolicyVersion)) return(fit)
  provenance <- list(nativeH = if (!inherits(fit, "try-error")) fit[["opt"]][["hessian"]] else NULL,
    nativeAttempt = if (!inherits(fit, "try-error")) isTRUE(attr(fit, "requestedHessian", exact = TRUE)) else FALSE,
    nativeCondition = NULL, nativeWarnings = character(0), analyticReason = NULL)
  if (inherits(fit, "try-error")) return(.sapInformationRecord(fit, "native", "retainedError", provenance))
  if (fit[["npars"]] == 0L) {
    provenance["nativeH"] <- list(NULL)
    provenance[["nativeAttempt"]] <- FALSE
    provenance[["sourceSyntheticH"]] <- is.matrix(fit[["opt"]][["hessian"]]) &&
      identical(dim(fit[["opt"]][["hessian"]]), c(0L, 0L))
    return(.sapInformationRecord(fit, "native", "fullyFixed", provenance))
  }
  specification <- attr(fit, "equations", exact = TRUE)
  eyring <- length(specification[["structuralFixed"]]) > 0L
  if (is.null(specification)) eyring <- .sapEquationHasEyring(attr(fit, "modelTerms", exact = TRUE))
  if (!eyring || !identical(fit[["dlist"]][["name"]], "genf.orig") ||
      (!is.null(specification) && specification[["components"]] != 1L))
    return(.sapInformationRecord(fit, "native", "nativeIneligible", provenance))
  requested <- attr(fit, "requestedHessian", exact = TRUE)
  if (identical(requested, FALSE)) return(.sapInformationRecord(fit, "native", "nativeNoH", provenance))
  required <- !is.null(specification) && !is.null(requested) && !is.null(fit[["dfns"]]) &&
    !is.null(fit[["data"]][["m"]]) && !is.null(fit[["data"]][["Y"]]) && !is.null(fit[["call"]]) &&
    !is.null(fit[["optpars"]]) && !is.null(fit[["fixedpars"]]) && !is.null(fit[["data"]][["m"]][["(weights)"]])
  if (!required) {
    reason <- gettext("The retained fit lacks the native response or coefficient metadata needed to refresh its information matrix. Rerun the model to obtain confidence intervals.")
    provenance[["analyticReason"]] <- reason
    return(.sapInformationRecord(.sapEyringUnavailableInference(fit, reason), "analyticGenfOrigDensity", "refreshMetadataUnavailable", provenance))
  }
  return(.sapEyringInformation(fit, specification, requested, censoringType, provenance))
}
.sapAssessInformationStore <- function(store, censoringType) {
  changed <- FALSE
  if (is.null(store) || identical(attr(store, "informationPolicyVersion", exact = TRUE), .sapInformationPolicyVersion))
    return(list(store = store, changed = changed))
  for (group in names(store)) for (family in names(store[[group]])) for (component in names(store[[group]][[family]]))
    for (model in seq_along(store[[group]][[family]][[component]])) {
      fit <- store[[group]][[family]][[component]][[model]]
      signature <- .sapInformationSignature(fit)
      fit <- .sapAssessInformation(fit, censoringType)
      changed <- changed || !identical(signature, .sapInformationSignature(fit))
      store[[group]][[family]][[component]][[model]] <- fit
    }
  attr(store, "informationPolicyVersion") <- .sapInformationPolicyVersion
  return(list(store = store, changed = changed))
}
