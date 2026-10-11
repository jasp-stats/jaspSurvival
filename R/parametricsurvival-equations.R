# The slot contract is asserted against the native distribution registry.
.sapParameterSlots <- function(distribution) {

  expected <- list(exp = character(0), gamma = "shape", genf = c("sigma", "Q", "P"),
    gengamma = c("sigma", "Q"), gompertz = "shape", llogis = "shape", lnorm = "sdlog",
    weibull = "shape", gengamma.orig = c("shape", "k"), genf.orig = c("sigma", "s1", "s2"))
  native <- flexsurv::flexsurv.dists[[distribution]]
  slots  <- setdiff(native[["pars"]], native[["location"]])
  if (!identical(slots, expected[[distribution]]))
    stop("Native ancillary parameter order differs from the analysis contract.")
  identityParameters <- switch(distribution, genf = c("mu", "Q"), gengamma = c("mu", "Q"),
    gompertz = "shape", lnorm = "meanlog", genf.orig = "mu", character(0))
  logParameters <- native[["pars"]][vapply(native[["transforms"]], function(link) identical(link, log), logical(1))]
  if (!identical(logParameters, setdiff(native[["pars"]], identityParameters)))
    stop("Native parameter links differ from the analysis contract.")
  return(slots)
}
.sapModelVariables <- function(modelTerms) {

  return(unique(c(unlist(modelTerms[["components"]], use.names = FALSE),
    unlist(lapply(modelTerms[["ancillaryEquations"]], .sapModelVariables), use.names = FALSE))))
}
.sapOptionsVariables <- function(options) {
  models <- options[["modelTerms"]]
  variables <- unlist(lapply(models, function(model) model[["components"]]), use.names = FALSE)
  for (slot in as.character(1:3))
    variables <- c(variables, unlist(lapply(models, function(model)
      model[["ancillaryEquations"]][[slot]][["components"]]), use.names = FALSE))
  return(unique(variables))
}
.sapModelTransformations <- function(modelTerms) {

  return(c(list(modelTerms[["transformations"]]),
    unlist(lapply(modelTerms[["ancillaryEquations"]], .sapModelTransformations), recursive = FALSE)))
}
.sapFamilyEquations <- function(modelTerms, family) {

  equations <- stats::setNames(list(modelTerms), family[["location"]])
  equations[[1L]][["ancillaryEquations"]] <- NULL
  slots <- .sapParameterSlots(family[["family"]])
  for (i in seq_along(slots)) {
    equation <- modelTerms[["ancillaryEquations"]][[as.character(i)]]
    if (!is.null(equation))
      equations[[slots[i]]] <- equation
  }
  return(equations)
}
.sapModelTermsForFamily <- function(modelTerms, distribution) {

  slots <- seq_along(.sapParameterSlots(distribution))
  modelTerms[["ancillaryEquations"]] <- modelTerms[["ancillaryEquations"]][intersect(as.character(slots), names(modelTerms[["ancillaryEquations"]]))]
  return(modelTerms)
}
.sapNativeParameterIdentity <- function(family, components, native) {
  parameters <- family[["pars"]]
  catalogue <- data.frame(parameter = rep(parameters, components),
    component = rep(seq_len(components), each = length(parameters)),
    native = if (components == 1L) parameters else as.vector(outer(parameters, seq_len(components), paste0)))
  row <- which(catalogue[["native"]] == native)
  if (length(row) != 1L)
    stop("Native component parameter name is not uniquely defined.")
  return(list(parameter = catalogue[["parameter"]][row], component = catalogue[["component"]][row]))
}
.sapCompileEquations <- function(dataset, options, family, modelTerms, components = 1L) {

  equations <- .sapFamilyEquations(modelTerms, family)
  formulas  <- lapply(equations, function(equation) .sapGetFormula(options, equation, dataset))
  matrices  <- lapply(formulas, function(formula)
    stats::model.matrix(formula, stats::model.frame(formula, dataset)))
  nativeLocation <- if (components == 1L) family[["location"]] else paste0(family[["location"]], 1L)
  nativePars <- if (components == 1L) family[["pars"]] else .sapmParameterNames(family, components)[["componentPars"]]
  nativeEquations <- list()
  for (native in nativePars) {
    identity <- .sapNativeParameterIdentity(family, components, native)
    k   <- identity[["component"]]
    par <- identity[["parameter"]]
    if (is.null(equations[[par]]))
      next
    columns <- colnames(matrices[[par]])[-1L]
    names   <- if (length(columns) == 0L) character(0) else if (native == nativeLocation) columns else paste0(native, "(", columns, ")")
    fixed   <- .sapRegressionFixed(dataset, equations[[par]], formulas[[par]], k, .sapCaseWeights(dataset, options))
    structural <- .sapEyringFixed(dataset, equations[[par]], formulas[[par]], family, par)
    fixed <- c(fixed, structural)
    nativeEquations[[native]] <- list(parameter = par, component = k, equation = equations[[par]],
      formula = formulas[[par]], matrix = matrices[[par]], slopeNames = names,
      link = if (identical(family[["transforms"]][[match(par, family[["pars"]])]], log)) "log" else "identity",
      transform = family[["transforms"]][[match(par, family[["pars"]])]],
      inverseLink = family[["inv.transforms"]][[match(par, family[["pars"]])]],
      slopeTerms = attr(matrices[[par]], "assign")[-1L],
      structuralFixed = stats::setNames(as.numeric(structural), names[match(names(structural), columns)]),
      fixed = stats::setNames(as.numeric(fixed), names[match(names(fixed), columns)]))
  }
  order <- c(nativeLocation, setdiff(nativePars, nativeLocation))
  nativeEquations <- nativeEquations[intersect(order, names(nativeEquations))]
  anc <- lapply(nativeEquations[setdiff(names(nativeEquations), nativeLocation)], function(equation) equation[["formula"]][-2L])
  # Empty ancillary equations are constants and need no native formula.
  anc <- anc[vapply(anc, function(formula) length(attr(stats::terms(formula), "term.labels")) > 0L, logical(1))]
  baseline <- if (components == 1L) family[["pars"]] else c(nativePars, .sapmParameterNames(family, components)[["weightPars"]])
  names <- c(baseline, unlist(lapply(nativeEquations, function(equation) equation[["slopeNames"]]), use.names = FALSE))
  fixed <- do.call(c, unname(lapply(nativeEquations, function(equation) equation[["fixed"]])))
  structural <- do.call(c, unname(lapply(nativeEquations, function(equation) equation[["structuralFixed"]])))
  if (anyNA(names) || anyDuplicated(names) || anyNA(match(names(fixed), names)) || anyDuplicated(names(fixed)))
    stop("Native coefficient names do not form a unique equation mapping.")
  return(list(formula = formulas[[family[["location"]]]], anc = anc, equations = nativeEquations,
    parameterNames = names, fixed = fixed, structuralFixed = structural, components = components, family = family[["family"]]))
}
.sapVerifyNativeSchema <- function(fit, specification) {

  expected <- specification[["parameterNames"]]
  if (!identical(rownames(fit[["res.t"]]), expected))
    stop("Native coefficient order differs from the prepared equation specification.")
  for (parameter in names(specification[["equations"]])) {
    equation <- specification[["equations"]][[parameter]]
    slope <- fit[["covpars"]][fit[["mx"]][[parameter]]]
    if (!identical(unname(expected[slope]), unname(equation[["slopeNames"]])))
      stop("Native coefficient indices differ from the prepared equation specification.")
    matrix <- fit[["data"]][["mml"]][[parameter]]
    if (length(slope) > 0L && !identical(colnames(matrix), colnames(equation[["matrix"]])))
      stop("Native design columns differ from the prepared equation specification.")
    equation[["baselineIndex"]] <- match(parameter, expected)
    equation[["slopeIndices"]]  <- slope
    equation[["terms"]]         <- fit[["data"]][["mml"]][[parameter]]
    specification[["equations"]][[parameter]] <- equation
  }
  return(specification)
}
.sapCheckModelRank <- function(specification, fixed) {

  fullDeficient <- FALSE
  for (native in names(specification[["equations"]])) {
    equation <- specification[["equations"]][[native]]
    design <- equation[["matrix"]]
    columns <- c(native, equation[["slopeNames"]])
    norm <- sqrt(colSums(design^2))
    norm[norm == 0] <- 1
    design <- sweep(design, 2, norm, "/")
    fullDeficient <- fullDeficient || qr(design)[["rank"]] < ncol(design)
    free <- which(!columns %in% names(fixed))
    if (length(free) == 0L)
      next
    decomposition <- qr(design[, free, drop = FALSE])
    if (decomposition[["rank"]] < length(free)) {
      redundant <- free[decomposition[["pivot"]][seq.int(decomposition[["rank"]] + 1L, length(free))]]
      stop(gettextf("The equation for %1$s in component %2$i contains dependent predictors (%3$s). Remove redundant predictors or fix their coefficients.",
        equation[["parameter"]], equation[["component"]], paste(jaspBase::decodeColNames(columns[redundant]), collapse = ", ")))
    }
  }
  return(fullDeficient)
}
.sapNativeSchema <- function(fitCall, specification, fixed) {

  pilot <- fitCall
  pilot[["fixedpars"]] <- NULL
  pilot[["lower"]] <- pilot[["upper"]] <- NULL
  pilot[["method"]]  <- "BFGS"
  pilot[["control"]] <- list(maxit = 0)
  pilot[["hessian"]] <- FALSE
  restrictedSeed <- is.null(fitCall[["inits"]]) && .sapCheckModelRank(specification, fixed)
  if (is.null(fitCall[["inits"]]) && (length(specification[["structuralFixed"]]) > 0L || restrictedSeed)) {
    seed <- pilot
    seed[["formula"]][[3L]] <- 1L
    seed[["anc"]] <- NULL
    seedFit <- suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, seed)))
    slopes <- unlist(lapply(specification[["equations"]], function(equation) equation[["slopeNames"]]), use.names = FALSE)
    baselines <- setdiff(specification[["parameterNames"]], slopes)
    if (!identical(rownames(seedFit[["res"]]), baselines) || anyDuplicated(baselines) ||
        any(!is.finite(seedFit[["res"]][, "est"])))
      stop(gettext("The native seed does not match the prepared baseline parameters."))
    inits <- stats::setNames(rep(0, length(specification[["parameterNames"]])), specification[["parameterNames"]])
    inits[baselines] <- seedFit[["res"]][baselines, "est"]
    pilot[["inits"]] <- unname(inits)
  }
  fit <- suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, pilot)))
  return(list(fit = fit, specification = .sapVerifyNativeSchema(fit, specification),
    restrictedInitials = if (restrictedSeed) pilot[["inits"]] else NULL))
}
.sapEquationMatrices <- function(fit, family, component = 1L, components = 1L) {

  return(stats::setNames(lapply(family[["pars"]], function(parameter) {
    native <- if (components == 1L) parameter else paste0(parameter, component)
    matrix <- fit[["data"]][["mml"]][[native]]
    if (is.null(matrix))
      return(matrix(numeric(0), nrow(fit[["data"]][["m"]]), 0L))
    return(matrix[, -1L, drop = FALSE])
  }), family[["pars"]]))
}
.sapEquationBetas <- function(fit, family, component = 1L, components = 1L) {

  return(stats::setNames(lapply(family[["pars"]], function(parameter) {
    native <- if (components == 1L) parameter else paste0(parameter, component)
    index  <- fit[["covpars"]][fit[["mx"]][[native]]]
    return(fit[["res.t"]][index, "est"])
  }), family[["pars"]]))
}
.sapCoefficientLabels <- function(fit, covariance = FALSE) {

  labels <- rownames(fit[["res"]])
  transformed <- rep(FALSE, length(labels))
  for (native in names(attr(fit, "equations")[["equations"]])) {
    equation <- attr(fit, "equations")[["equations"]][[native]]
    index <- equation[["slopeIndices"]]
    terms <- colnames(equation[["matrix"]])[-1L]
    labels[index] <- .sapTransformationNames(terms, fit, equation[["equation"]])
    transformed[index] <- labels[index] != terms
    if (covariance || equation[["parameter"]] != .sapmFamily(attr(fit, "family"))[["location"]])
      labels[index] <- paste0(if (covariance) native else equation[["parameter"]], "(", labels[index], ")")
  }
  attr(labels, "transformed") <- transformed
  return(labels)
}

.sapModelEquations <- function(jaspResults, options) {

  if (!is.null(jaspResults[["modelEquations"]]))
    return()
  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = .sapExtractFit(jaspResults, options, type = "selected"),
    outputFunction = .sapModelEquationsTableFun,
    name           = "modelEquations",
    title          = gettext("Fitted Model Equations"),
    dependencies   = c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
      "fittedModelEquation", "fittedModelEquationDecimals"),
    position       = 2.2
  )
  return()
}

.sapModelEquationsTableFun <- function(fit, options) {

  table <- createJaspTable()
  .sapAddModelColumns(table, options, output = "coefficients", includeModel = FALSE)
  table$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
  if (options[["analysisType"]] == "mixture")
    table$addColumnInfo(name = "mixtureComponent", title = gettext("Component"), type = "integer")
  families <- if (length(fit) > 0L) unique(vapply(fit, function(model) attr(model, "family"), character(1))) else .sapGetDistributions(options)
  familyParameters <- lapply(families, function(family) .sapmFamily(family)[["pars"]])
  numbered <- length(families) > 1L
  parameters <- if (numbered) paste0("parameter", seq_len(max(lengths(familyParameters)))) else
    unique(unlist(familyParameters, use.names = FALSE))
  for (i in seq_along(parameters))
    table$addColumnInfo(name = parameters[i], title = if (numbered) gettextf("Parameter %i", i) else parameters[i], type = "string")
  table$showSpecifiedColumnsOnly <- TRUE
  if (!.saSurvivalReady(options))
    return(table)

  data <- .saSafeRbind(lapply(fit, function(model) {
    if (jaspBase::isTryError(model))
      return(.sapRowModelInformation(model))
    details <- .sapEquationDetails(model)
    modelParameters <- .sapmFamily(attr(model, "family"))[["pars"]]
    rows <- lapply(seq_len(attr(model, "components")), function(component) {
      row <- .sapRowModelInformation(model)
      if (options[["analysisType"]] == "mixture")
        row[["mixtureComponent"]] <- component
      for (native in names(details)) {
        detail <- details[[native]]
        if (detail[["component"]] == component) {
          column <- if (numbered) paste0("parameter", match(detail[["parameter"]], modelParameters)) else detail[["parameter"]]
          row[[column]] <- .sapFittedParameterEquation(model, native, detail, options[["fittedModelEquationDecimals"]])
        }
      }
      return(row)
    })
    return(.saSafeRbind(rows))
  }))
  table$setData(data)
  messages <- .sapCollectFitErrors(fit, options)
  messages <- c(messages, unique(unlist(lapply(fit, .sapFactorCodingNotes))))
  for (message in messages)
    table$addFootnote(message)
  return(table)
}

.sapEquationDetails <- function(fit) {

  family <- .sapmFamily(attr(fit, "family"))
  components <- attr(fit, "components")
  native <- if (components == 1L) family[["pars"]] else .sapmParameterNames(family, components)[["componentPars"]]
  specification <- attr(fit, "equations")[["equations"]]
  return(stats::setNames(lapply(native, function(parameter) {
    equation <- specification[[parameter]]
    index <- match(parameter, rownames(fit[["res.t"]]))
    identity <- .sapNativeParameterIdentity(family, components, parameter)
    par <- identity[["parameter"]]
    slopes <- if (is.null(equation)) integer(0) else equation[["slopeIndices"]]
    list(parameter = par, component = identity[["component"]],
      link = if (identical(family[["transforms"]][[match(par, family[["pars"]])]], log)) "log" else "identity",
      baselineIndex = index, slopeIndices = slopes,
      coefficients = fit[["res.t"]][c(index, slopes), "est"],
      designColumns = if (is.null(equation)) character(0) else colnames(equation[["matrix"]])[-1L],
      equation = equation,
      structural = c(index, slopes) %in% match(names(attr(fit, "structuralParameters")), rownames(fit[["res.t"]])),
      fixed = c(index, slopes) %in% fit[["fixedpars"]])
  }), native))
}

.sapEquationNumber <- function(value, decimals) {

  if (is.finite(value) && value != 0 && round(value, decimals) == 0)
    return(formatC(value, format = "e", digits = max(1L, decimals)))
  return(formatC(value, format = "f", digits = decimals))
}

.sapEquationEscape <- function(text) {

  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  return(gsub("'", "&#39;", text, fixed = TRUE))
}

.sapFittedParameterEquation <- function(fit, native, detail, decimals) {

  parameter <- detail[["parameter"]]
  if (length(detail[["slopeIndices"]]) == 0L)
    return(paste(parameter, "=", .sapEquationNumber(fit[["res"]][native, "est"], decimals)))

  labels <- .sapTransformationNames(detail[["designColumns"]], fit, detail[["equation"]][["equation"]])
  ordinary <- labels == detail[["designColumns"]]
  variables <- .sapModelVariables(detail[["equation"]][["equation"]])
  labels[ordinary] <- vapply(labels[ordinary], .saTermNames, character(1), variables = variables)
  labels <- .sapEquationEscape(jaspBase::decodeColNames(labels))
  coefficients <- detail[["coefficients"]]
  rhs <- .sapEquationNumber(coefficients[1L], decimals)
  for (i in seq_along(labels)) {
    value <- coefficients[i + 1L]
    if (value == 0) next
    rhs <- paste(rhs, if (value < 0) "\u2212" else "+", .sapEquationNumber(abs(value), decimals), labels[i])
  }
  lhs <- if (detail[["link"]] == "log") paste0("log(", parameter, ")") else parameter
  return(paste(lhs, "=", rhs))
}
.sapEquationVaries <- function(fit, equation) {

  slopes <- equation[["slopeIndices"]]
  return(any(!slopes %in% fit[["fixedpars"]] | fit[["res.t"]][slopes, "est"] != 0))
}

.sapmCompleteMStep <- function(dataset, options, family, modelTerms, component, weights, previous, constraint, fixed, specification) {

  selectComponent <- function(equation) {
    equation[["parameterRestrictions"]] <- equation[["parameterRestrictions"]][component]
    equation[["ancillaryEquations"]] <- lapply(equation[["ancillaryEquations"]], selectComponent)
    return(equation)
  }
  current <- .sapCompileEquations(dataset, options, family, selectComponent(modelTerms))
  fixed <- c(fixed, current[["fixed"]])
  dlist <- flexsurv::flexsurv.dists[[family[["family"]]]]
  if (!is.null(constraint))
    dlist <- .sapmConstraintDistribution(family[["family"]], constraint, family)
  response <- stats::model.response(stats::model.frame(current[["formula"]], dataset))
  informative <- .sapmInformativeRows(response)
  polynomial <- any(vapply(current[["equations"]], function(equation)
    any(vapply(equation[["equation"]][["transformations"]], function(transform)
      transform[["transformation"]] == "polynomial", logical(1))), logical(1)))
  fullPolynomial <- polynomial && any(!informative)
  # Native polynomial formulas must retain the original fitting cohort/basis.
  nativeRows <- if (fullPolynomial) rep(TRUE, nrow(dataset)) else informative
  nativeWeights <- pmax(weights[nativeRows], 1e-10)
  fitCall <- list(formula = current[["formula"]], anc = current[["anc"]], data = dataset[nativeRows, , drop = FALSE],
    weights = nativeWeights, dist = dlist, hessian = FALSE)
  if (!is.null(previous))
    fitCall[["inits"]] <- c(previous[["base"]], unlist(previous[["beta"]], use.names = FALSE))
  if (fullPolynomial && is.null(fitCall[["inits"]])) {
    seed <- fitCall
    seed[["formula"]][[3L]] <- 1L
    seed[["anc"]] <- NULL
    seed[["data"]] <- dataset[informative, , drop = FALSE]
    seed[["weights"]] <- pmax(weights[informative], 1e-10)
    seed[["method"]] <- "BFGS"
    seed[["control"]] <- list(maxit = 0)
    seedFit <- suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, seed)))
    slopes <- unlist(lapply(current[["equations"]], function(equation) equation[["slopeNames"]]), use.names = FALSE)
    baselines <- setdiff(current[["parameterNames"]], slopes)
    if (!identical(rownames(seedFit[["res"]]), baselines) || anyDuplicated(baselines) ||
        any(!is.finite(seedFit[["res"]][, "est"])))
      stop(gettext("The native seed does not match the prepared baseline parameters."))
    inits <- stats::setNames(rep(0, length(current[["parameterNames"]])), current[["parameterNames"]])
    inits[baselines] <- seedFit[["res"]][baselines, "est"]
    fitCall[["inits"]] <- unname(inits)
  }
  schema <- .sapNativeSchema(fitCall, current, fixed)
  if (!fullPolynomial && (is.null(fitCall[["inits"]]) || length(current[["structuralFixed"]]) == 0L))
    fitCall[["inits"]] <- unname(schema[["fit"]][["res"]][, "est"])
  if (!is.null(constraint)) {
    bounds <- .sapmConstraintBounds(constraint, family, 1L, length(current[["parameterNames"]]))
    fitCall[["method"]] <- "L-BFGS-B"
    fitCall[["lower"]] <- bounds[["lower"]]
    fitCall[["upper"]] <- bounds[["upper"]]
    fitCall[["control"]] <- list(maxit = 1000, factr = 1e5, fnscale = sum(weights[informative]), pgtol = 1e-6)
    fitCall[["inits"]] <- .sapmFeasibleInits(fitCall[["inits"]], constraint, family, 1L)
  }
  fitCall <- .sapEyringInitials(fitCall, schema, fixed)
  fitCall <- .sapFixedFitCall(fitCall, fixed, current[["parameterNames"]])
  fitCall <- .sapEyringControls(fitCall, schema)
  fit <- suppressWarnings(suppressMessages(.sapEyringNativeFit(fitCall, schema, options[["censoringType"]])))
  .sapVerifyNativeSchema(fit, current)
  base <- fit[["res"]][family[["pars"]], "est"]
  beta <- .sapEquationBetas(fit, family)
  # Keep native location-first order for the following component warm start.
  beta <- beta[c(family[["location"]], setdiff(family[["pars"]], family[["location"]]))]
  matrices <- stats::setNames(lapply(family[["pars"]], function(parameter) {
    native <- specification[["equations"]][[parameter]]
    if (is.null(native)) return(matrix(numeric(0), nrow(dataset), 0L))
    return(native[["matrix"]][, -1L, drop = FALSE])
  }), family[["pars"]])
  linkBaseline <- fit[["res.t"]][family[["pars"]], "est"]
  return(list(base = base, beta = beta, linkBaseline = linkBaseline, nativeControl = fitCall[["control"]],
    structuralInitialAdjustment = attr(fitCall, "structuralInitialAdjustment"), fullParameterScales = attr(fitCall, "fullParameterScales"),
    parameters = .sapmParameters(family, base, beta, matrices, linkBaseline)))
}
.sapmConditionalMagnitude <- function(fit, family, components) {

  parameters <- .sapmObservationParametersWithoutDiagnostics(fit, family, components)
  selected <- family[["pars"]][vapply(family[["transforms"]], function(link) identical(link, log), logical(1)) &
    family[["pars"]] != family[["location"]]]
  return(vapply(parameters, function(parameter) any(vapply(parameter[selected], function(value)
    any(!is.finite(log(value)) | abs(log(value)) > 15), logical(1))), logical(1)))
}
.sapmObservationParametersWithoutDiagnostics <- function(fit, family, components) {

  base <- .sapmBaseParameters(fit[["res.t"]][, "est"], family, components)[["base"]]
  return(lapply(seq_len(components), function(k) {
    baseline <- stats::setNames(fit[["res.t"]][paste0(family[["pars"]], k), "est"], family[["pars"]])
    return(.sapmParameters(family, base[[k]], .sapEquationBetas(fit, family, k, components),
      .sapEquationMatrices(fit, family, k, components), baseline))
  }))
}
.sapmDivergingEffects <- function(fit, family, components) {

  selected <- family[["pars"]][family[["pars"]] == family[["location"]] |
    vapply(family[["transforms"]], function(link) identical(link, log), logical(1))]
  return(vapply(seq_len(components), function(k) {
    beta <- .sapEquationBetas(fit, family, k, components)
    matrices <- .sapEquationMatrices(fit, family, k, components)
    any(vapply(selected, function(parameter) {
      if (length(beta[[parameter]]) == 0L) return(FALSE)
      value <- as.vector(matrices[[parameter]] %*% beta[[parameter]])
      return(any(!is.finite(value)) || max(abs(value - mean(value))) > 15)
    }, logical(1)))
  }, logical(1)))
}
