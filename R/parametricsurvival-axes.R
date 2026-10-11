# Display coordinates reuse the fitted term; predictions stay in raw units.
.sapPredictionAxisScale <- function(fit, variable, scale, timeAxis = FALSE, options = NULL) {
  if (scale == "none") return(NULL)
  if (scale == "log") return(list(transform = scales::transform_log10(), labels = NULL, title = NULL, logarithmic = TRUE))
  if (timeAxis) return(NULL)
  mappings <- unlist(lapply(fit, function(model) {
    terms <- attr(model, "modelTerms")
    equations <- c(list(terms), terms[["ancillaryEquations"]])
    equations <- Filter(function(equation) variable %in% unlist(equation[["components"]], use.names = FALSE), equations)
    plotRange <- if (is.null(options)) range(attr(model, "dataset")[[variable]]) else .sapPredictionAxisRange(model, options, variable)
    lapply(equations, function(equation) .sapPredictionTermScale(attr(model, "dataset")[[variable]],
      variable, equation[["transformations"]][[variable]], plotRange, model[["data"]][["m"]][["(weights)"]]))
  }), recursive = FALSE)
  if (!length(mappings)) stop(gettext("The selected predictor has no fitted model term. Choose Linear or Log."))
  if (!all(vapply(mappings, function(mapping) identical(mapping[["specification"]], mappings[[1L]][["specification"]]), logical(1))))
    stop(gettext("The displayed models use different transformations for this predictor. Choose Linear or Log, or display the models separately."))
  return(mappings[[1L]])
}
.sapPredictionTermScale <- function(values, variable, specification, plotRange = range(values), weights) {
  label <- jaspBase::decodeColNames(variable)
  if (is.null(specification)) return(list(transform = scales::transform_identity(), labels = NULL, title = label, specification = NULL))
  kind <- specification[["transformation"]]
  if (kind != "arrhenius") specification[["temperatureUnit"]] <- NULL
  if (kind %in% c("polynomial", "naturalSpline", "eyring"))
    stop(gettext("Polynomial, spline and Eyring specifications contain multiple model terms. Choose Linear or Log for this axis."))
  exponent <- switch(kind, reciprocal = -1, customExponent =, root = specification[["powerExponent"]])
  if (!is.null(exponent)) {
    signedPower <- (kind == "root" && .sapOddRoot(specification)) ||
      (kind == "customExponent" && abs(exponent) %% 2 == 1)
    if ((!signedPower || exponent < 0) && min(plotRange) < 0 && max(plotRange) > 0)
      stop(gettext("This model-term transformation is not invertible across the displayed predictor range. Choose Linear or Log."))
    negative <- max(plotRange) <= 0
    if (!signedPower) specification[["negativeBranch"]] <- negative
    transform <- function(x) if (signedPower) sign(x) * abs(x)^exponent else x^exponent
    inverse <- function(x) if (signedPower) sign(x) * abs(x)^(1 / exponent) else if (negative) -abs(x)^(1 / exponent) else x^(1 / exponent)
    title <- if (kind == "reciprocal") gettextf("1 / %s", label) else .sapTransformationLabel(label, specification)
  } else if (kind %in% c("power", "customPower")) {
    base <- if (kind == "customPower") specification[["logBase"]] else exp(1)
    multiplier <- if (kind == "customPower") specification[["logMultiplier"]] else 1
    transform <- function(x) multiplier * log(x, base = base)
    inverse <- function(x) base^(x / multiplier)
    title <- .sapTransformationLabel(label, specification)
  } else if (kind == "arrhenius") {
    unit <- specification[["temperatureUnit"]]
    boltzmann <- specification[["boltzmannConstant"]]
    transform <- function(x) 1 / (boltzmann * .sapKelvin(x, unit))
    inverse <- function(x) {
      kelvin <- 1 / (boltzmann * x)
      return(switch(unit, kelvin = kelvin, celsius = kelvin - 273.15, fahrenheit = (kelvin - 273.15) * 9 / 5 + 32))
    }
    title <- gettextf("1 / (k_B %1$s) (eV\u207b\u00b9; temperature in Kelvin)", label)
  } else if (kind == "scale") {
    center <- .sapWeightedMean(values, weights)
    deviation <- .sapWeightedSd(values, weights)
    specification[["center"]] <- center
    specification[["deviation"]] <- deviation
    transform <- function(x) (x - center) / deviation
    inverse <- function(x) x * deviation + center
    title <- gettextf("Standardized %s", label)
  }
  breaks <- function(limits) inverse(jaspGraphs::getPrettyAxisBreaks(sort(transform(limits))))
  trans <- scales::new_transform("modelTerm", transform, inverse, breaks = breaks)
  # Compare evaluated transformations, not alternative spellings such as 3/2 and 1.5.
  specification[intersect(names(specification), c("logBaseExpression", "logMultiplierExpression",
    "powerExponentExpression", "rootDegreeExpression"))] <- NULL
  return(list(transform = trans, labels = function(x) format(signif(transform(x), 4L), trim = TRUE), title = title, specification = specification))
}
.sapPredictionPositionScale <- function(axis, position) {
  build <- if (position == "x") ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
  return(build(transform = axis[["transform"]], labels = if (is.null(axis[["labels"]])) ggplot2::waiver() else axis[["labels"]],
    name = if (is.null(axis[["title"]])) ggplot2::waiver() else axis[["title"]]))
}
.sapPredictionAxisGrid <- function(values, number, axis) {
  if (is.null(axis)) return(seq(min(values), max(values), length.out = number))
  if (identical(axis[["logarithmic"]], TRUE)) values <- values[values > 0]
  if (!length(values)) stop(gettext("A logarithmic axis requires positive predictor values."))
  transformed <- axis[["transform"]]$transform(range(values))
  return(axis[["transform"]]$inverse(seq(transformed[1L], transformed[2L], length.out = number)))
}
