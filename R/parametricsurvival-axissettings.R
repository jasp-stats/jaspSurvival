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
