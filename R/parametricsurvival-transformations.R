# A variable has one transformation per equation; interactions share that choice.
.sapPrepareModelTerms <- function(options) {

  components <- max(.sapComponents(options))
  mixture    <- options[["analysisType"]] == "mixture"
  ancillary  <- options[["modelAncillaryParameterRegression"]]
  options[["modelTerms"]] <- lapply(options[["modelTerms"]], .sapPrepareEquation,
    advanced = options[["modelAdvancedSpecification"]], modifiers = options[["modelAdvancedSpecificationTerms"]], components = components, mixture = mixture, factors = options[["factors"]])
  families   <- .sapGetDistributions(options)
  slots      <- max(vapply(families, function(family) length(.sapParameterSlots(family)), integer(1)), 0L)
  locationKeys <- vapply(options[["modelTerms"]], function(model) model[["name"]], character(1))
  if (!identical(locationKeys, paste0("model", seq_along(locationKeys))))
    stop("Location equation model keys differ from the native model order.")
  if (ancillary) for (slot in seq_len(slots)) {
    prefix <- paste0("modelAncillaryParameter", slot)
    keys <- vapply(options[[paste0(prefix, "ModelTerms")]], function(model) model[["name"]], character(1))
    if (!identical(keys, paste0("ancillary", slot, locationKeys)))
      stop("Ancillary equation model keys do not match the location model order.")
  }
  for (i in seq_along(options[["modelTerms"]])) {
    model <- options[["modelTerms"]][[i]]
    model[["ancillaryEquations"]] <- list()
    if (ancillary) for (slot in seq_len(slots)) {
      prefix <- paste0("modelAncillaryParameter", slot)
      rows   <- options[[paste0(prefix, "ModelTerms")]]
      keys   <- vapply(rows, function(row) row[["name"]], character(1))
      expected <- paste0("ancillary", slot, model[["name"]])
      if (anyDuplicated(keys) || sum(keys == expected) != 1L)
        stop("Ancillary equation model keys do not match the location models.")
      equation <- .sapPrepareEquation(rows[[match(expected, keys)]], options[[paste0(prefix, "AdvancedSpecification")]],
        options[[paste0(prefix, "AdvancedSpecificationTerms")]], components, mixture, options[["factors"]])
      equation[["name"]] <- model[["name"]]
      model[["ancillaryEquations"]][[as.character(slot)]] <- equation
    }
    options[["modelTerms"]][[i]] <- model
  }
  .sapCheckEyringModels(options[["modelTerms"]])

  return(options)
}
.sapPrepareEquation <- function(model, advanced, modifiers, components, mixture, factors) {

  transformations <- list()
  factorContrasts <- list()
  restrictions    <- rep(list(list()), components)
  if (advanced) {
    modifierIndex <- which(vapply(modifiers, function(modifier)
      modifier[["model"]] == model[["name"]], logical(1)))
    if (length(modifierIndex) != 1L)
      stop("Equation modifier keys do not match the model keys.")
    rows <- unlist(lapply(modifiers[modifierIndex], function(modifier)
      modifier[["predictors"]]), recursive = FALSE, use.names = FALSE)
    for (row in rows) {
      variable <- row[["variable"]][[1L]]
      values <- .sapModelTermRestrictions(row, variable, model[["title"]], components, mixture)
      for (k in seq_along(values))
        if (length(values[[k]]) > 0L)
          restrictions[[k]][[variable]] <- values[[k]]

      if (variable %in% factors)
        factorContrasts[[variable]] <- .sapFactorContrastSpecification(row)
      else {
        specification <- .sapTransformationSpecification(row)
        if (!is.null(specification)) transformations[[variable]] <- specification
      }
    }
  }
  model[["transformations"]]       <- transformations
  model[["factorContrasts"]]       <- factorContrasts
  model[["factorVariables"]]       <- intersect(unique(unlist(model[["components"]], use.names = FALSE)), factors)
  model[["parameterRestrictions"]] <- restrictions
  model[["advancedSpecification"]] <- advanced

  return(model)
}

.sapTransformationSpecification <- function(row) {

  transformation <- row[["predictorTransformation"]]
  if (transformation == "none")
    return(NULL)

  specification <- list(transformation = transformation, temperatureUnit = row[["predictorTransformationFrom"]])
  if (transformation == "arrhenius")
    specification[["boltzmannConstant"]] <- .sapBoltzmannConstantEv
  if (transformation == "customPower") {
    base <- .sapNumericExpression(row[["predictorTransformationBase"]])
    if (!.sapFiniteNumeric(base, expectedLength = 1L))
      .quitAnalysis(gettext("The custom logarithm base must be one finite number or expression."))
    if (base <= 0)
      .quitAnalysis(gettext("The custom logarithm base must be positive."))
    if (base == 1)
      .quitAnalysis(gettext("The custom logarithm base must be different from 1."))
    specification[["logBase"]] <- base
    specification[["logBaseExpression"]] <- as.character(row[["predictorTransformationBase"]])
    multiplier <- .sapNumericExpression(row[["predictorTransformationMultiplier"]])
    if (!.sapFiniteNumeric(multiplier, expectedLength = 1L) || multiplier == 0)
      .quitAnalysis(gettext("The logarithm multiplier must be one finite, nonzero number or expression."))
    specification[["logMultiplier"]] <- multiplier
    specification[["logMultiplierExpression"]] <- as.character(row[["predictorTransformationMultiplier"]])
  }
  if (transformation %in% c("customExponent", "root")) {
    field <- if (transformation == "root") "rootDegree" else "powerExponent"
    input <- row[[if (transformation == "root") "predictorTransformationRootDegree" else "predictorTransformationExponent"]]
    exponent <- .sapNumericExpression(input)
    if (!.sapFiniteNumeric(exponent, expectedLength = 1L))
      .quitAnalysis(gettext("The exponent or root degree must be one finite number or expression."))
    if (exponent == 0)
      .quitAnalysis(gettext("The exponent or root degree must be different from 0."))
    specification[[field]] <- exponent
    specification[[paste0(field, "Expression")]] <- as.character(input)
    specification[["powerExponent"]] <- if (transformation == "root") 1 / exponent else exponent
    if (!is.finite(specification[["powerExponent"]]))
      .quitAnalysis(gettext("The root degree is too close to zero to evaluate its reciprocal."))
  }
  if (transformation == "polynomial")
    specification[["polynomialDegree"]] <- row[["predictorTransformationDegree"]]
  if (transformation == "naturalSpline")
    specification[["splineDf"]] <- row[["predictorTransformationDegreesOfFreedom"]]

  return(specification)
}

.sapVariableExpression <- function(variable, transformation, dataset, weights) {

  if (is.null(transformation))
    return(variable)

  numericExpression <- paste0("as.numeric(as.character(", variable, "))")
  number <- function(value, scientific = FALSE)
    format(value, digits = 17, scientific = scientific, trim = TRUE)
  expression <- switch(transformation[["transformation"]],
    # Embed fitting-data constants, so predictions do not recompute scaling.
    scale        = paste0("I((", variable, " - ", number(.sapWeightedMean(dataset[[variable]], weights), scientific = TRUE),
                          ") / ", number(.sapWeightedSd(dataset[[variable]], weights), scientific = TRUE), ")"),
    power        = paste0("log(", numericExpression, ")"),
    customPower  = paste0("I(", number(transformation[["logMultiplier"]]), " * log(", numericExpression,
      ", base = ", number(transformation[["logBase"]]), "))"),
    arrhenius    = paste0("I(1/(", number(transformation[["boltzmannConstant"]], scientific = TRUE), " * ",
      .sapTemperatureExpression(variable, transformation[["temperatureUnit"]]), "))"),
    eyring       = paste0("I(1/(", .sapTemperatureExpression(variable, transformation[["temperatureUnit"]]), "))"),
    reciprocal   = paste0("I(1/", numericExpression, ")"),
    customExponent = paste0("I(", numericExpression, "^", number(transformation[["powerExponent"]]), ")"),
    root = {
      signed <- .sapOddRoot(transformation)
      value <- if (signed) paste0("abs(", numericExpression, ")") else numericExpression
      paste0("I(", if (signed) paste0("sign(", numericExpression, ") * "), value,
        "^", number(transformation[["powerExponent"]]), ")")
    },
    polynomial   = paste0("stats::poly(", numericExpression, ", degree = ", transformation[["polynomialDegree"]], ", raw = FALSE)"),
    naturalSpline = {
      # Embed the fitting-data knots, including for predictions on a new cohort.
      basis <- splines::ns(dataset[[variable]], df = transformation[["splineDf"]])
      vector <- function(values) paste0("c(", paste(number(values, scientific = TRUE), collapse = ", "), ")")
      paste0("splines::ns(", variable, ", knots = ", vector(attr(basis, "knots")),
        ", Boundary.knots = ", vector(attr(basis, "Boundary.knots")), ", intercept = FALSE)")
    }
  )

  return(paste(deparse(str2lang(expression), width.cutoff = 500L), collapse = ""))
}

.sapOddRoot <- function(specification) {
  degree <- specification[["rootDegree"]]
  return(degree == round(degree) && abs(degree) %% 2 == 1)
}

# Per-row domain checks are shared by fitting validation and custom predictions.
# Sample-level checks (variance and basis size) belong to fitting only.
.sapTransformationInvalid <- function(value, specification) {

  domain <- switch(specification[["transformation"]],
    power =, customPower = value <= 0,
    arrhenius =, eyring = {
      temperature <- .sapKelvin(value, specification[["temperatureUnit"]])
      !is.finite(temperature) | temperature <= 0
    },
    reciprocal = value == 0,
    customExponent = !is.finite(suppressWarnings(value^specification[["powerExponent"]])),
    root = !is.finite(suppressWarnings((if (.sapOddRoot(specification)) abs(value) else value)^specification[["powerExponent"]])),
    polynomial = !is.finite(suppressWarnings(value^specification[["polynomialDegree"]])),
    rep(FALSE, length(value))
  )
  invalid <- !is.finite(value) | domain
  invalid[is.na(invalid)] <- TRUE

  return(invalid)
}

.sapCheckTransformationData <- function(dataset, modelTerms) {

  .sapCheckFactorData(dataset, modelTerms)
  for (equation in modelTerms[["ancillaryEquations"]])
    .sapCheckTransformationData(dataset, equation)
  for (variable in names(modelTerms[["transformations"]])) {
    specification  <- modelTerms[["transformations"]][[variable]]
    transformation <- specification[["transformation"]]
    value          <- suppressWarnings(as.numeric(as.character(dataset[[variable]])))
    label          <- jaspBase::decodeColNames(variable)
    invalid        <- .sapTransformationInvalid(value, specification)
    if (any(!is.finite(value)))
      .quitAnalysis(gettextf("The transformation of '%1$s' requires finite numeric values.", label))
    if (transformation == "scale") {
      deviation <- stats::sd(dataset[[variable]])
      if (!is.finite(deviation) || deviation == 0)
        .quitAnalysis(gettextf("Scaling '%1$s' requires a finite, nonzero standard deviation.", label))
    }
    if (transformation %in% c("power", "customPower") && any(invalid))
      .quitAnalysis(gettextf("The logarithmic transformation of '%1$s' requires strictly positive stress values.", label))
    if (transformation %in% c("arrhenius", "eyring")) {
      if (any(invalid))
        .quitAnalysis(gettextf("The temperature transformation of '%1$s' requires finite absolute temperatures above zero. Check the selected temperature units.", label))
    }
    if (transformation == "reciprocal" && any(invalid))
      .quitAnalysis(gettextf("The reciprocal transformation of '%1$s' requires nonzero stress values.", label))
    if (transformation %in% c("customExponent", "root")) {
      exponent <- specification[["powerExponent"]]
      if (exponent < 0 && any(value == 0))
        .quitAnalysis(gettextf("A negative exponent for '%1$s' requires nonzero values.", label))
      if (exponent != round(exponent) && any(value < 0) && !(transformation == "root" && .sapOddRoot(specification)))
        .quitAnalysis(gettextf("A fractional exponent for '%1$s' requires nonnegative values.", label))
      if (any(invalid))
        .quitAnalysis(gettextf("The power-transformed values of '%1$s' exceed the numeric range.", label))
    }
    if (transformation == "naturalSpline") {
      if (!is.numeric(dataset[[variable]]))
        .quitAnalysis(gettextf("Natural cubic splines require a continuous numeric predictor: '%s'.", label))
      if (length(unique(value)) <= specification[["splineDf"]])
        .quitAnalysis(gettextf("The natural cubic spline of '%1$s' requires at least %2$s distinct values.", label, specification[["splineDf"]] + 1))
    }
    if (transformation == "polynomial") {
      degree <- specification[["polynomialDegree"]]
      if (length(unique(value)) <= degree)
        .quitAnalysis(gettextf("The polynomial transformation of '%1$s' requires at least %2$s distinct values.", label, degree + 1))
      if (any(invalid))
        .quitAnalysis(gettextf("The polynomial transformation of '%1$s' exceeds the numeric range.", label))
    }
  }

  return()
}

.sapTransformationNames <- function(coefficients, fit, equation = attr(fit, "modelTerms")) {

  transformations <- equation[["transformations"]]
  variables       <- unique(unlist(equation[["components"]], use.names = FALSE))
  factorTokens    <- .sapFactorCoefficientTokens(fit, equation)
  for (i in seq_along(coefficients)) {
    originalCoefficient <- coefficients[i]
    placeholders <- if (length(factorTokens)) paste0(".factorContrast", seq_along(factorTokens), ".") else character(0)
    for (index in order(nchar(names(factorTokens)), decreasing = TRUE))
      coefficients[i] <- gsub(names(factorTokens)[index], placeholders[index], coefficients[i], fixed = TRUE)
    # Keep namespace operators intact; only split interaction separators.
    originalTerms <- strsplit(coefficients[i], "(?<!:):(?!:)", perl = TRUE)[[1]]
    displayTerms  <- originalTerms
    for (variable in names(transformations)) {
      specification <- transformations[[variable]]
      expression    <- .sapVariableExpression(variable, specification, attr(fit, "dataset"), fit[["data"]][["m"]][["(weights)"]])
      variableLabel <- jaspBase::decodeColNames(variable)
      if (specification[["transformation"]] %in% c("polynomial", "naturalSpline")) {
        count <- if (specification[["transformation"]] == "polynomial") specification[["polynomialDegree"]] else specification[["splineDf"]]
        label <- if (specification[["transformation"]] == "polynomial") gettext("Polynomial") else gettext("Natural cubic spline")
        if (count == 1)
          displayTerms <- gsub(expression, gettextf("%1$s(%2$s, %3$s)", label, variableLabel, 1), displayTerms, fixed = TRUE)
        else
          for (degree in rev(seq_len(count)))
            displayTerms <- gsub(paste0(expression, degree), gettextf("%1$s(%2$s, %3$s)", label, variableLabel, degree), displayTerms, fixed = TRUE)
      } else {
        label <- .sapTransformationLabel(variableLabel, specification)
        displayTerms <- gsub(expression, label, displayTerms, fixed = TRUE)
        if (specification[["transformation"]] == "eyring")
          displayTerms <- gsub(.sapEyringLogExpression(variable, specification[["temperatureUnit"]]),
            gettextf("Eyring log temperature(%1$s, %2$s)", variableLabel, .sapTemperatureUnitLabel(specification[["temperatureUnit"]])), displayTerms, fixed = TRUE)
      }
    }
    ordinaryTerms <- displayTerms == originalTerms & !vapply(displayTerms, function(term) any(vapply(placeholders, grepl, logical(1), x = term, fixed = TRUE)), logical(1))
    if (!all(ordinaryTerms)) {
      displayTerms[ordinaryTerms] <- vapply(originalTerms[ordinaryTerms], .saTermNames, character(1), variables = variables)
      for (index in seq_along(placeholders)) displayTerms <- gsub(placeholders[index], factorTokens[index], displayTerms, fixed = TRUE)
      coefficients[i] <- paste(jaspBase::decodeColNames(displayTerms), collapse = jaspBase::interactionSymbol)
    } else coefficients[i] <- originalCoefficient
  }

  return(coefficients)
}

.sapTransformationLabel <- function(variable, specification) {

  transformation <- specification[["transformation"]]
  label <- switch(transformation,
    scale              = gettext("Scale"),
    power              = "log",
    customPower        = "log",
    customExponent     = gettext("Power"),
    root               = gettext("Root"),
    naturalSpline      = gettext("Natural cubic spline"),
    arrhenius          = gettext("Arrhenius"),
    eyring             = gettext("Eyring"),
    reciprocal         = gettext("Reciprocal")
  )
  if (transformation == "customPower") {
    logarithm <- paste0("log(", variable, ", base = ", .sapEquationConstant(specification[["logBase"]]), ")")
    multiplier <- specification[["logMultiplier"]]
    if (multiplier == 1) return(logarithm)
    number <- .sapEquationConstant(multiplier)
    if (multiplier < 0) number <- paste0("(", number, ")")
    return(paste(number, "*", logarithm))
  }
  if (transformation == "customExponent")
    return(gettextf("%1$s(%2$s, %3$s)", label, variable, specification[["powerExponentExpression"]]))
  if (transformation == "root")
    return(gettextf("%1$s(%2$s, %3$s)", label, variable, specification[["rootDegreeExpression"]]))
  if (transformation == "arrhenius")
    return(gettextf("%1$s(%2$s) [eV]", label, variable))
  if (transformation == "eyring")
    return(gettextf("%1$s(%2$s, %3$s)", label, variable, .sapTemperatureUnitLabel(specification[["temperatureUnit"]])))

  return(paste0(label, "(", variable, ")"))
}
