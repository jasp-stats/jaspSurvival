# Contour levels are outcome values; axes and evaluation slices keep their units.
.sapPredictionContourLevelOptions <- c("predictionsContoursLevels", "predictionsContoursLevelsNumber",
  "predictionsContoursCommonAcrossFigures", paste0("predictionsContoursLevels", c("SurvivalTime", "RelativeSurvivalTime",
    "SurvivalProbability", "FailureProbability", "Hazard", "CumulativeHazard", "RestrictedMeanSurvivalTime")))
.sapPredictionContourCustomLevels <- function(options, measure) {
  suffix <- paste0(toupper(substring(measure, 1L, 1L)), substring(measure, 2L))
  text <- trimws(options[[paste0("predictionsContoursLevels", suffix)]])
  if (!nzchar(text)) return(numeric(0))
  values <- .sapNumericExpression(text, multiple = TRUE)
  if (!.sapFiniteNumeric(values))
    stop(gettext("Contour levels must be finite values or numeric expressions separated by commas."))
  probability <- measure %in% c("survivalProbability", "failureProbability")
  if (any(values < 0) || (probability && any(values > 1)))
    stop(if (probability) gettext("Probability contour levels must be between 0 and 1.") else gettext("Contour levels must be non-negative."))
  return(sort(unique(values)))
}
.sapPredictionContourNumberLevels <- function(values, number) {
  values <- values[is.finite(values)]
  if (!length(values) || diff(range(values)) == 0) return(numeric(0))
  limits <- range(values)
  # Pretty spacing makes the requested count approximate; endpoints do not form contours.
  levels <- jaspGraphs::getPrettyAxisBreaks(limits, n = number + 1L)
  return(levels[is.finite(levels) & levels > limits[1L] & levels < limits[2L]])
}
.sapPredictionContourLevels <- function(data, options, measure, common = NULL) {
  panels <- unique(as.character(data[["Panel"]]))
  custom <- if (options[["predictionsContoursLevels"]] == "custom") .sapPredictionContourCustomLevels(options, measure)
  if (options[["predictionsContoursLevels"]] == "number" && options[["predictionsContoursCommonAcrossFigures"]] && is.null(common))
    common <- .sapPredictionContourNumberLevels(data[["estimate"]], options[["predictionsContoursLevelsNumber"]])
  return(stats::setNames(lapply(panels, function(panel) {
    if (options[["predictionsContoursLevels"]] == "custom") return(custom)
    if (!is.null(common)) return(common)
    return(.sapPredictionContourNumberLevels(data[["estimate"]][as.character(data[["Panel"]]) == panel], options[["predictionsContoursLevelsNumber"]]))
  }), panels))
}
.sapPredictionCommonContourLevels <- function(groups, options, type, measure) {
  groups <- lapply(groups, function(fit) fit[!vapply(fit, jaspBase::isTryError, logical(1))])
  groups <- Filter(length, groups)
  if (!length(groups) || !.saSurvivalReady(options)) return(numeric(0))
  axes <- .sapModelPredictionAxes(options)
  targets <- unlist(lapply(groups, function(fit) {
    settings <- .sapPredictionContourPlotAxes(fit, options, axes)[["settings"]]
    settings[["predictionsDisplayConfidenceIntervals"]] <- FALSE
    at <- .sapModelPredictionAt(.sapPlotTimeFit(fit), settings, type, "panels")
    lapply(fit, function(model) list(model = model, at = at, settings = settings,
      scenario = .sapPredictionContourScenarios(model, settings, axes, 65L, plot = TRUE)))
  }), recursive = FALSE, use.names = FALSE)
  ticks <- sum(vapply(targets, function(target) ceiling(nrow(target[["scenario"]][["data"]]) / 128L), numeric(1)))
  startProgressbar(expectedTicks = ticks, label = gettext("Computing common contour levels"))
  ranges <- lapply(targets, function(target) {
    blocks <- .sapPredictionContourBlocks(target[["model"]], target[["scenario"]], target[["settings"]], type, target[["at"]], measure,
      summarize = function(value) {
        estimates <- value[["estimate"]]
        estimates <- estimates[is.finite(estimates)]
        return(if (length(estimates)) range(estimates))
      })
    unlist(blocks, use.names = FALSE)
  })
  return(.sapPredictionContourNumberLevels(unlist(ranges, use.names = FALSE), options[["predictionsContoursLevelsNumber"]]))
}
