# The slot contract is asserted against the native distribution registry.
.sapParameterSlots <- function(distribution) {

  expected <- list(exp = character(0), gamma = "shape", genf = c("sigma", "Q", "P"),
    gengamma = c("sigma", "Q"), gompertz = "shape", llogis = "shape", lnorm = "sdlog",
    weibull  = "shape", gengamma.orig = c("shape", "k"), genf.orig = c("sigma", "s1", "s2"))
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

  models    <- options[["modelTerms"]]
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
  catalogue  <- data.frame(parameter = rep(parameters, components),
    component = rep(seq_len(components), each = length(parameters)),
    native    = if (components == 1L) parameters else as.vector(outer(parameters, seq_len(components), paste0)))
  row <- which(catalogue[["native"]] == native)
  if (length(row) != 1L)
    stop("Native component parameter name is not uniquely defined.")
  return(list(parameter = catalogue[["parameter"]][row], component = catalogue[["component"]][row]))
}

# Compile parameter equations into native coefficient names, designs and fixed values.
.sapCompileEquations <- function(dataset, options, family, modelTerms, components = 1L) {

  equations <- .sapFamilyEquations(modelTerms, family)
  formulas  <- lapply(equations, function(equation) .sapGetFormula(options, equation, dataset))
  matrices  <- lapply(formulas, function(formula)
    stats::model.matrix(formula, stats::model.frame(formula, dataset)))

  # Expand each parameter equation over mixture components in native naming order.
  nativeLocation  <- if (components == 1L) family[["location"]] else paste0(family[["location"]], 1L)
  nativePars      <- if (components == 1L) family[["pars"]] else .sapmParameterNames(family, components)[["componentPars"]]
  nativeEquations <- list()
  for (native in nativePars) {
    identity <- .sapNativeParameterIdentity(family, components, native)
    k        <- identity[["component"]]
    par      <- identity[["parameter"]]
    if (is.null(equations[[par]]))
      next
    columns    <- colnames(matrices[[par]])[-1L]
    names      <- if (length(columns) == 0L) character(0) else if (native == nativeLocation) columns else paste0(native, "(", columns, ")")
    fixed      <- .sapRegressionFixed(dataset, equations[[par]], formulas[[par]], k, .sapCaseWeights(dataset, options))
    structural <- .sapEyringFixed(dataset, equations[[par]], formulas[[par]], family, par)
    fixed      <- c(fixed, structural)
    nativeEquations[[native]] <- list(parameter = par, component = k, equation = equations[[par]],
      formula         = formulas[[par]], matrix = matrices[[par]], slopeNames = names,
      link            = if (identical(family[["transforms"]][[match(par, family[["pars"]])]], log)) "log" else "identity",
      transform       = family[["transforms"]][[match(par, family[["pars"]])]],
      inverseLink     = family[["inv.transforms"]][[match(par, family[["pars"]])]],
      slopeTerms      = attr(matrices[[par]], "assign")[-1L],
      structuralFixed = stats::setNames(as.numeric(structural), names[match(names(structural), columns)]),
      fixed           = stats::setNames(as.numeric(fixed), names[match(names(fixed), columns)]))
  }

  # Keep the location equation first and reconcile fixed coefficients with that order.
  order           <- c(nativeLocation, setdiff(nativePars, nativeLocation))
  nativeEquations <- nativeEquations[intersect(order, names(nativeEquations))]
  anc             <- lapply(nativeEquations[setdiff(names(nativeEquations), nativeLocation)], function(equation) equation[["formula"]][-2L])
  # Empty ancillary equations are constants and need no native formula.
  anc        <- anc[vapply(anc, function(formula) length(attr(stats::terms(formula), "term.labels")) > 0L, logical(1))]
  baseline   <- if (components == 1L) family[["pars"]] else c(nativePars, .sapmParameterNames(family, components)[["weightPars"]])
  names      <- c(baseline, unlist(lapply(nativeEquations, function(equation) equation[["slopeNames"]]), use.names = FALSE))
  fixed      <- do.call(c, unname(lapply(nativeEquations, function(equation) equation[["fixed"]])))
  structural <- do.call(c, unname(lapply(nativeEquations, function(equation) equation[["structuralFixed"]])))
  if (anyNA(names) || anyDuplicated(names) || anyNA(match(names(fixed), names)) || anyDuplicated(names(fixed)))
    stop("Native coefficient names do not form a unique equation mapping.")
  return(list(formula = formulas[[family[["location"]]]], anc = anc, equations = nativeEquations,
    parameterNames = names, fixed = fixed, structuralFixed = structural, components = components, family = family[["family"]]))
}

# Bind prepared equations to the coefficient indices actually returned by flexsurv.
.sapVerifyNativeSchema <- function(fit, specification) {

  expected <- specification[["parameterNames"]]
  if (!identical(rownames(fit[["res.t"]]), expected))
    stop("Native coefficient order differs from the prepared equation specification.")
  for (parameter in names(specification[["equations"]])) {
    equation <- specification[["equations"]][[parameter]]
    slope    <- fit[["covpars"]][fit[["mx"]][[parameter]]]
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

# Check identifiability of free coefficients after accounting for fixed coefficients.
.sapCheckModelRank <- function(specification, fixed) {

  fullDeficient <- FALSE
  for (native in names(specification[["equations"]])) {
    equation <- specification[["equations"]][[native]]
    design   <- equation[["matrix"]]
    columns  <- c(native, equation[["slopeNames"]])
    norm     <- sqrt(colSums(design^2))
    norm[norm == 0] <- 1
    design        <- sweep(design, 2, norm, "/")
    fullDeficient <- fullDeficient || qr(design)[["rank"]] < ncol(design)

    # Fixed columns may make the full design deficient without making free effects unidentified.
    free          <- which(!columns %in% names(fixed))
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

# Use a zero-iteration native fit to establish coefficient layout before optimization.
.sapNativeSchema <- function(fitCall, specification, fixed) {

  pilot <- fitCall
  pilot[["fixedpars"]] <- NULL
  pilot[["lower"]] <- pilot[["upper"]] <- NULL
  pilot[["method"]]  <- "BFGS"
  pilot[["control"]] <- list(maxit = 0)
  pilot[["hessian"]] <- FALSE

  # A baseline seed avoids initializing a deficient design before fixes are applied.
  restrictedSeed <- is.null(fitCall[["inits"]]) && .sapCheckModelRank(specification, fixed)
  if (is.null(fitCall[["inits"]]) && (length(specification[["structuralFixed"]]) > 0L || restrictedSeed)) {
    seed <- pilot
    seed[["formula"]][[3L]] <- 1L
    seed[["anc"]] <- NULL
    pilot[["inits"]] <- .sapBaselineInitials(seed, specification)
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

  labels      <- rownames(fit[["res"]])
  transformed <- rep(FALSE, length(labels))
  for (native in names(attr(fit, "equations")[["equations"]])) {
    equation <- attr(fit, "equations")[["equations"]][[native]]
    index    <- equation[["slopeIndices"]]
    terms    <- colnames(equation[["matrix"]])[-1L]
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
  families         <- if (length(fit) > 0L) unique(vapply(fit, function(model) attr(model, "family"), character(1))) else .sapGetDistributions(options)
  familyParameters <- lapply(families, function(family) .sapmFamily(family)[["pars"]])
  numbered         <- length(families) > 1L
  parameters       <- if (numbered) paste0("parameter", seq_len(max(lengths(familyParameters)))) else
    unique(unlist(familyParameters, use.names = FALSE))
  for (i in seq_along(parameters))
    table$addColumnInfo(
      name  = parameters[i],
      title = if (numbered) gettextf("Parameter %i", i) else parameters[i],
      type  = "string"
    )
  table$showSpecifiedColumnsOnly <- TRUE
  if (!.saSurvivalReady(options))
    return(table)

  data <- .saSafeRbind(lapply(fit, function(model) {
    if (jaspBase::isTryError(model))
      return(.sapRowModelInformation(model))
    details         <- .sapEquationDetails(model)
    modelParameters <- .sapmFamily(attr(model, "family"))[["pars"]]
    rows            <- lapply(seq_len(attr(model, "components")), function(component) {
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
  for (message in messages)
    table$addFootnote(message)
  return(table)
}

.sapEquationDetails <- function(fit) {

  family        <- .sapmFamily(attr(fit, "family"))
  components    <- attr(fit, "components")
  native        <- if (components == 1L) family[["pars"]] else .sapmParameterNames(family, components)[["componentPars"]]
  specification <- attr(fit, "equations")[["equations"]]
  return(stats::setNames(lapply(native, function(parameter) {
    equation <- specification[[parameter]]
    index    <- match(parameter, rownames(fit[["res.t"]]))
    identity <- .sapNativeParameterIdentity(family, components, parameter)
    par      <- identity[["parameter"]]
    slopes   <- if (is.null(equation)) integer(0) else equation[["slopeIndices"]]
    list(parameter = par, component = identity[["component"]],
      link          = if (identical(family[["transforms"]][[match(par, family[["pars"]])]], log)) "log" else "identity",
      baselineIndex = index, slopeIndices = slopes,
      coefficients  = fit[["res.t"]][c(index, slopes), "est"],
      designColumns = if (is.null(equation)) character(0) else colnames(equation[["matrix"]])[-1L],
      equation      = equation,
      structural    = c(index, slopes) %in% match(names(attr(fit, "structuralParameters")), rownames(fit[["res.t"]])),
      fixed         = c(index, slopes) %in% fit[["fixedpars"]])
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

.sapEquationConstant <- function(value) {

  return(vapply(value, function(number) {
    for (digits in seq_len(17L)) {
      text <- format(number, digits = digits, trim = TRUE)
      if (identical(as.numeric(text), as.numeric(number)))
        return(text)
    }
    return(format(number, digits = 17L, trim = TRUE))
  }, character(1)))
}

# Format language objects, keeping operator precedence and fitted constants.
.sapEquationExpression <- function(expression, html = FALSE) {

  if (is.numeric(expression)) {
    if (!is.null(attr(expression, "boltzmannConstant")))
      return(if (html) "k<sub>B</sub>" else "k_B")
    return(.sapEquationConstant(expression))
  }
  if (is.logical(expression))
    return(as.character(expression))
  if (is.name(expression)) {
    label <- jaspBase::decodeColNames(as.character(expression))
    if (make.names(label) != label)
      label <- paste0("`", gsub("`", "\\`", gsub("\\", "\\\\", label, fixed = TRUE), fixed = TRUE), "`")
    return(if (html) .sapEquationEscape(label) else label)
  }

  operator  <- as.character(expression[[1L]])
  arguments <- as.list(expression)[-1L]
  if (length(operator) == 1L && operator %in% c("I", "as.numeric", "as.character", "("))
    return(.sapEquationExpression(arguments[[1L]], html))
  if (length(operator) == 1L && operator == "log" && length(arguments) == 2L)
    return(paste0("log(", .sapEquationExpression(arguments[[1L]], html), ", base = ",
      .sapEquationExpression(arguments[[2L]], html), ")"))
  values <- vapply(arguments, .sapEquationExpression, character(1), html = html)
  if (length(operator) == 1L && operator %in% c("+", "-", "*", "/", "^"))
    return(paste0("(", if (length(values) == 1L) paste0(operator, values) else
      paste(values, collapse = paste0(" ", operator, " ")), ")"))
  label <- paste(deparse(expression[[1L]]), collapse = "")
  return(paste0(if (html) .sapEquationEscape(label) else label, "(", paste(values, collapse = ", "), ")"))
}

.sapEquationArrheniusExpression <- function(expression, variable, specification, fit) {

  # Mark only the multiplier in the prepared I(1/(constant * T_K)) AST.
  quoted   <- paste(deparse(as.name(variable), backtick = TRUE), collapse = "")
  expected <- str2lang(.sapVariableExpression(quoted, specification,
    attr(fit, "dataset"), fit[["data"]][["m"]][["(weights)"]]))
  if (!identical(expression, expected))
    stop(gettext("The fitted Arrhenius expression does not match its transformation constant."))
  # Use the actual fitted literal, including the generator's deparse precision.
  attr(expression[[2L]][[3L]][[2L]][[2L]], "boltzmannConstant") <- TRUE
  return(expression)
}

.sapEquationRootExpression <- function(expression, degree) {

  if (!is.call(expression))
    return(expression)
  if (identical(expression[[1L]], as.name("^")))
    expression[[3L]] <- call("/", 1, degree)
  else
    for (index in seq.int(2L, length(expression)))
      expression[[index]] <- .sapEquationRootExpression(expression[[index]], degree)
  return(expression)
}

.sapEquationAtoms <- function(fit, native, expression, coding, equation, index, html = FALSE) {

  value         <- fit[["data"]][["m"]][[expression]]
  parsed        <- str2lang(expression)
  variable      <- all.vars(parsed)[1L]
  specification <- equation[["transformations"]][[variable]]
  if (!is.null(specification) && specification[["transformation"]] == "arrhenius")
    parsed <- .sapEquationArrheniusExpression(parsed, variable, specification, fit)
  label <- .sapEquationExpression(parsed, html)
  if (is.factor(value)) {
    if (coding == 2L)
      columns <- levels(value)
    else {
      contrast <- fit[["all.contrasts"]][[native]][[expression]]
      if (!is.null(contrast))
        stats::contrasts(value) <- contrast
      contrast <- stats::contrasts(value)
      columns  <- colnames(contrast)
      if (is.null(columns))
        columns <- as.character(seq_len(ncol(contrast)))
    }
    # Full indicator columns use their actual levels, not contrast tokens.
    tokens  <- if (coding == 2L) character(0) else .sapFactorCoefficientTokens(fit, equation)
    names   <- paste0(expression, columns)
    labels  <- unname(tokens[match(names, names(tokens))])
    missing <- is.na(labels)
    labels[missing] <- paste0(jaspBase::decodeColNames(variable), " (", columns[missing], ")")
    if (html)
      labels <- .sapEquationEscape(labels)
    return(list(names = names, labels = paste0("(", labels, ")")))
  }
  if (is.matrix(value)) {
    count   <- ncol(value)
    columns <- colnames(value)
    if (is.null(columns))
      columns <- as.character(seq_len(count))
    symbol   <- paste0(if (specification[["transformation"]] == "polynomial") "P" else "B", "[", native, ", ", index, ", ")
    argument <- .sapEquationExpression(as.name(variable))
    labels   <- paste0(symbol, seq_len(count), "](", if (html) .sapEquationEscape(argument) else argument, ")")
    return(list(names = paste0(expression, if (count == 1L) "" else columns), labels = labels))
  }
  if (!is.null(specification) && specification[["transformation"]] == "root")
    label <- .sapEquationExpression(.sapEquationRootExpression(parsed, specification[["rootDegree"]]), html)
  return(list(names = expression, labels = label))
}

# Match whole native column names using fitted term membership. Colons inside
# namespaces, escaped names or factor levels are never interaction separators.
.sapEquationTerms <- function(fit, native, detail, html = FALSE) {

  equation <- detail[["equation"]]
  if (length(detail[["designColumns"]]) == 0L)
    return(character(0))

  factors <- attr(stats::terms(equation[["formula"]]), "factors")
  labels  <- character(length(detail[["designColumns"]]))
  for (term in unique(equation[["slopeTerms"]])) {
    selected <- which(factors[, term] > 0L)
    atoms    <- lapply(selected, function(index) .sapEquationAtoms(fit, native, rownames(factors)[index],
      factors[index, term], equation[["equation"]], index, html))
    grid  <- expand.grid(lapply(atoms, function(atom) seq_along(atom[["names"]])))
    names <- apply(grid, 1L, function(row) paste(vapply(seq_along(atoms), function(index)
      atoms[[index]][["names"]][row[index]], character(1)), collapse = ":"))
    display <- apply(grid, 1L, function(row) paste(vapply(seq_along(atoms), function(index)
      atoms[[index]][["labels"]][row[index]], character(1)), collapse = " * "))
    columns <- which(equation[["slopeTerms"]] == term)
    mapping <- match(detail[["designColumns"]][columns], names)
    if (anyNA(mapping))
      stop(gettext("The fitted equation terms do not match the native design columns."))
    labels[columns] <- display[mapping]
  }
  return(labels)
}

.sapFittedParameterEquation <- function(fit, native, detail, decimals) {

  parameter <- detail[["parameter"]]
  if (length(detail[["slopeIndices"]]) == 0L)
    return(paste(parameter, "=", .sapEquationNumber(fit[["res"]][native, "est"], decimals)))

  labels       <- .sapEquationTerms(fit, native, detail, html = TRUE)
  coefficients <- detail[["coefficients"]]
  rhs          <- .sapEquationNumber(coefficients[1L], decimals)
  for (i in seq_along(labels)) {
    value <- coefficients[i + 1L]
    if (value == 0)
      next
    rhs <- paste(rhs, if (value < 0) "\u2212" else "+", .sapEquationNumber(abs(value), decimals), "*", labels[i])
  }
  lhs <- if (detail[["link"]] == "log") paste0("log(", parameter, ")") else parameter
  return(paste(lhs, "=", rhs))
}

.sapEquationVaries <- function(fit, equation) {

  slopes <- equation[["slopeIndices"]]
  return(any(!slopes %in% fit[["fixedpars"]] | fit[["res.t"]][slopes, "est"] != 0))
}

# Fit one weighted component while preserving its complete parameter equations.
.sapmCompleteMStep <- function(dataset, options, family, modelTerms, component, weights, previous, constraint, fixed, specification) {

  selectComponent <- function(equation) {
    equation[["parameterRestrictions"]] <- equation[["parameterRestrictions"]][component]
    equation[["ancillaryEquations"]] <- lapply(equation[["ancillaryEquations"]], selectComponent)
    return(equation)
  }
  current <- .sapCompileEquations(dataset, options, family, selectComponent(modelTerms))
  fixed   <- c(fixed, current[["fixed"]])
  dlist   <- flexsurv::flexsurv.dists[[family[["family"]]]]
  if (!is.null(constraint))
    dlist <- .sapmConstraintDistribution(family[["family"]], constraint, family)

  # Exclude likelihood-neutral rows unless polynomial bases need the full cohort.
  response    <- stats::model.response(stats::model.frame(current[["formula"]], dataset))
  informative <- .sapmInformativeRows(response)
  polynomial  <- any(vapply(current[["equations"]], function(equation)
    any(vapply(equation[["equation"]][["transformations"]], function(transform)
      transform[["transformation"]] == "polynomial", logical(1))), logical(1)))
  fullPolynomial <- polynomial && any(!informative)
  # Native polynomial formulas must retain the original fitting cohort/basis.
  nativeRows    <- if (fullPolynomial) rep(TRUE, nrow(dataset)) else informative
  nativeWeights <- pmax(weights[nativeRows], 1e-10)
  fitCall       <- list(formula = current[["formula"]], anc = current[["anc"]], data = dataset[nativeRows, , drop = FALSE],
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
    fitCall[["inits"]] <- .sapBaselineInitials(seed, current)
  }

  # Apply native coefficient maps, bounds and structural fixes before the weighted fit.
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
  fit     <- suppressWarnings(suppressMessages(.sapmCompressedFit(fitCall, function(...) {
    nativeCall <- list(...)
    attr(nativeCall, "nativeFixedValues") <- attr(fitCall, "nativeFixedValues")
    .sapNativeFit(nativeCall, schema, options[["censoringType"]])
  })))
  .sapVerifyNativeSchema(fit, current)

  # Return the fitted component on both natural and link scales for the next EM step.
  base <- fit[["res"]][family[["pars"]], "est"]
  beta <- .sapEquationBetas(fit, family)
  # Keep native location-first order for the following component warm start.
  beta     <- beta[c(family[["location"]], setdiff(family[["pars"]], family[["location"]]))]
  matrices <- stats::setNames(lapply(family[["pars"]], function(parameter) {
    native <- specification[["equations"]][[parameter]]
    if (is.null(native))
      return(matrix(numeric(0), nrow(dataset), 0L))
    return(native[["matrix"]][, -1L, drop = FALSE])
  }), family[["pars"]])
  linkBaseline <- fit[["res.t"]][family[["pars"]], "est"]
  return(list(base = base, beta = beta, linkBaseline = linkBaseline, nativeControl = attr(fit, "nativeControl"),
    structuralInitialAdjustment = attr(fitCall, "structuralInitialAdjustment"),
    parameters                  = .sapmParameters(family, base, beta, matrices, linkBaseline)))
}

# Map a baseline-only native seed to the complete equation coefficient order.
.sapBaselineInitials <- function(seed, specification) {

  seedFit   <- suppressWarnings(suppressMessages(do.call(flexsurv::flexsurvreg, seed)))
  slopes    <- unlist(lapply(specification[["equations"]], function(equation) equation[["slopeNames"]]), use.names = FALSE)
  baselines <- setdiff(specification[["parameterNames"]], slopes)
  if (!identical(rownames(seedFit[["res"]]), baselines) || anyDuplicated(baselines) ||
    any(!is.finite(seedFit[["res"]][, "est"])))
    stop(gettext("The native seed does not match the prepared baseline parameters."))
  inits <- stats::setNames(rep(0, length(specification[["parameterNames"]])), specification[["parameterNames"]])
  inits[baselines] <- seedFit[["res"]][baselines, "est"]
  return(unname(inits))
}
