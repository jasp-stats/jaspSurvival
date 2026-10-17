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

# Tick endpoints cover the rendered geometry, including bounds and overlays.
.sapPredictionPrettyAxes <- function(plot, quantileAxis = FALSE, probabilityAxis = FALSE) {
  built <- ggplot2::ggplot_build(plot)
  estimates <- which(vapply(plot$layers, function(layer)
    (inherits(layer$geom, "GeomLine") || inherits(layer$geom, "GeomPoint")) && inherits(layer$data, "waiver"), logical(1)))
  if (length(estimates) && !any(vapply(built$data[estimates], function(layer)
      any(is.finite(layer$x) & is.finite(layer$y)), logical(1))))
    stop(gettext("No finite predictions remain after the selected axis transformations. Choose a different scale or evaluation range."))
  plot$scales <- plot$scales$clone()
  for (position in c("x", "y")) {
    scale <- built$layout[[paste0("panel_scales_", position)]][[1L]]
    if (scale$is_discrete()) next
    columns <- paste0(position, c("", "min", "max", "end", "intercept"))
    values <- unlist(lapply(seq_along(built$data), function(i) {
      # A one-dimensional rug has no meaningful coordinate on the other axis.
      if (inherits(plot$layers[[i]]$geom, "GeomRug") &&
          !position %in% names(plot$layers[[i]]$mapping)) return(NULL)
      layer <- built$data[[i]]
      unlist(layer[intersect(columns, names(layer))], use.names = FALSE)
    }), use.names = FALSE)
    if (is.numeric(scale$limits)) values <- c(values, scale$limits)
    values <- values[is.finite(values)]
    if (!length(values)) next
    transformation <- scale$get_transformation()
    bounds <- range(values)
    intervals <- if (position == "x" && !quantileAxis) 3L else 5L
    tolerance <- 64 * .Machine$double.eps * max(abs(bounds))
    validTicks <- function(ticks) {
      raw <- suppressWarnings(transformation$inverse(ticks))
      mapped <- suppressWarnings(transformation$transform(raw))
      valid <- is.finite(raw) & is.finite(mapped)
      if ((position == "x" && quantileAxis) || (position == "y" && probabilityAxis))
        valid <- valid & raw >= 0 & raw <= 1
      return(mapped[valid])
    }
    if (position == "y" && probabilityAxis && transformation$name == "logmlogmp") {
      ticks <- .sapPredictionProbabilityTicks(bounds, transformation, intervals)
    } else if (transformation$name == "log-10") {
      # Labels remain in original units on logarithmic axes.
      rawBounds <- sort(transformation$inverse(bounds))
      rawTicks <- if (diff(bounds) < 1) jaspGraphs::getPrettyAxisBreaks(rawBounds, n = intervals) else
        scales::breaks_log(n = intervals)(rawBounds)
      ticks <- validTicks(suppressWarnings(transformation$transform(rawTicks)))
      anchors <- .sapPredictionTickAnchors(rawBounds)
      anchors <- validTicks(suppressWarnings(transformation$transform(anchors[anchors > 0])))
    } else {
      ticks <- validTicks(jaspGraphs::getPrettyAxisBreaks(bounds, n = intervals))
      anchors <- validTicks(.sapPredictionTickAnchors(bounds))
    }
    if (!(position == "y" && probabilityAxis && transformation$name == "logmlogmp")) {
      lower <- anchors[anchors <= bounds[1L] + tolerance]
      upper <- anchors[anchors >= bounds[2L] - tolerance]
      if ((!length(ticks) || min(ticks) > bounds[1L] + tolerance) && length(lower)) ticks <- c(max(lower), ticks)
      if ((!length(ticks) || max(ticks) < bounds[2L] - tolerance) && length(upper)) ticks <- c(ticks, min(upper))
    }
    # Cover any boundary omitted by the selected break algorithm.
    if (!length(ticks) || min(ticks) > bounds[1L] + tolerance) ticks <- c(bounds[1L], ticks)
    if (!length(ticks) || max(ticks) < bounds[2L] - tolerance) ticks <- c(ticks, bounds[2L])
    ticks <- sort(unique(ticks))
    rawTicks <- suppressWarnings(transformation$inverse(ticks))
    roundTrip <- suppressWarnings(transformation$transform(rawTicks))
    valid <- is.finite(rawTicks) & is.finite(roundTrip)
    ticks <- roundTrip[valid]
    rawTicks <- rawTicks[valid]
    if (!length(ticks)) next
    replacement <- scale$clone()
    replacement$breaks <- rawTicks
    if (position == "y" && probabilityAxis && transformation$name != "identity" && inherits(replacement$labels, "waiver"))
      replacement$labels <- .sapPredictionProbabilityLabels
    # ggplot stores continuous limits in transformed coordinates.
    replacement$limits <- range(c(ticks, bounds))
    replacement$oob <- scales::oob_keep
    plot$scales$scales <- Filter(function(other) !position %in% other$aesthetics, plot$scales$scales)
    plot$scales$add(replacement)
  }
  return(plot)
}

# Round finite-domain endpoints without making the entire tick grid denser.
.sapPredictionTickAnchors <- function(bounds) {
  magnitude <- abs(bounds[bounds != 0])
  if (!length(magnitude)) return(0)
  return(sort(unique(c(0, as.vector(outer(c(-5, -2, -1, 1, 2, 5),
    10^seq(floor(log10(min(magnitude))) - 1, ceiling(log10(max(magnitude))) + 1), `*`))))))
}

# Choose round probabilities, with additional resolution near either boundary.
.sapPredictionProbabilityTicks <- function(bounds, transformation, intervals) {
  rawBounds <- sort(transformation$inverse(bounds))
  probabilities <- sort(unique(c(
    jaspGraphs::getPrettyAxisBreaks(rawBounds, n = intervals),
    seq(.1, .9, .1), as.vector(outer(c(1, 2, 5), 10^seq(-323, -1), `*`)),
    1 - as.vector(outer(c(1, 2, 5), 10^seq(-16, -1), `*`)))))
  probabilities <- unique(signif(probabilities, 15L))
  probabilities <- probabilities[is.finite(probabilities) & probabilities > 0 & probabilities < 1]
  ticks <- transformation$transform(probabilities)
  lower <- which(ticks <= bounds[1L])
  upper <- which(ticks >= bounds[2L])
  ticks <- ticks[seq.int(if (length(lower)) max(lower) else 1L,
    if (length(upper)) min(upper) else length(ticks))]
  targets <- seq(min(ticks), max(ticks), length.out = intervals + 1L)
  return(unique(ticks[vapply(targets, function(target) which.min(abs(ticks - target)), integer(1))]))
}

.sapPredictionProbabilityLabels <- function(probabilities) {
  return(vapply(probabilities, function(probability) {
    label <- format(probability, digits = 15L, trim = TRUE)
    if (probability < 1 && label == "1")
      label <- paste0("1 \u2212 ", format(1 - probability, digits = 2L, trim = TRUE))
    return(label)
  }, character(1)))
}

.sapPredictionAxisSettingOptions <- c("predictionsXAxisTransformation", "predictionsCovariateXAxisSettings",
  paste0("predictionsCovariateXAxisSettings", rep(c("XAxis", "YAxis"), each = 3L), rep(c("Transformation", "From", "To"), 2L)))
.sapPredictionAxisSettings <- function(options, variable) {
  if (options[["predictionsOutputPlotType"]] == "contour") {
    position <- if (variable == unlist(options[["predictionsContourXAxis"]], use.names = FALSE)) "X" else "Y"
    prefix <- paste0("predictionsCovariateXAxisSettings", position, "Axis")
    return(list(scale = options[[paste0(prefix, "Transformation")]], from = options[[paste0(prefix, "From")]], to = options[[paste0(prefix, "To")]]))
  }
  if (variable == options[["predictionsLifetimeTimeId"]])
    return(list(scale = options[["predictionsXAxisTransformation"]], from = "", to = ""))
  rows <- options[["predictionsCovariateXAxisSettings"]]
  variables <- vapply(rows, function(row) row[["variable"]][[1L]], character(1))
  row <- rows[[match(variable, variables)]]
  return(list(scale = row[["xAxisTransformation"]], from = row[["from"]], to = row[["to"]]))
}
.sapPredictionAxisRange <- function(fit, options, variable) {
  settings <- .sapPredictionAxisSettings(options, variable)
  values <- attr(fit, "dataset")[[variable]]
  if (settings[["scale"]] == "log") values <- values[values > 0]
  if (!length(values)) stop(gettext("A logarithmic plot range requires positive predictor values."))
  bounds <- range(values)
  for (i in seq_along(bounds)) {
    text <- trimws(settings[[if (i == 1L) "from" else "to"]])
    if (nzchar(text)) {
      value <- .sapNumericExpression(text)
      if (!.sapFiniteNumeric(value, expectedLength = 1L))
        stop(gettextf("The plot range for %s requires finite scalar values or expressions.", jaspBase::decodeColNames(variable)))
      bounds[i] <- value
    }
  }
  if (bounds[1L] >= bounds[2L])
    stop(gettextf("From must be smaller than To for the plot range of %s.", jaspBase::decodeColNames(variable)))
  if (settings[["scale"]] == "log" && any(bounds <= 0))
    stop(gettextf("The logarithmic plot range for %s must be positive.", jaspBase::decodeColNames(variable)))
  specifications <- .sapModelTransformations(attr(fit, "modelTerms"))
  for (equation in specifications) if (!is.null(equation[[variable]]) && any(.sapTransformationInvalid(bounds, equation[[variable]])))
    stop(gettextf("The plot range for %s is outside its fitted transformation's domain.", jaspBase::decodeColNames(variable)))
  return(bounds)
}
.sapPredictionAxisPlotGrid <- function(fit, options, variable, number) {
  settings <- .sapPredictionAxisSettings(options, variable)
  bounds <- .sapPredictionAxisRange(fit, options, variable)
  axis <- .sapPredictionAxisScale(list(fit), variable, settings[["scale"]], options = options)
  return(.sapPredictionAxisGrid(bounds, number, axis))
}
