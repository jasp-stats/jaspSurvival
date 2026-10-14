# New-observation outputs share selected fits, fitted bases and joint draws.
.sapCustomPredictionDependencies <- c(
  .sapModelPredictionOptions, .sapLifetimeSummaryOptions, "predictionsPredictFor", "predictionsManualInput", "predictionsCsvFile",
  .sapPredictionQuantileDependencies, .sapPredictionTimeDependencies,
  .sapPredictionCiDependencies, "predictionsOutputPlot", "predictionsOutputTable", "predictionsOutputExportPredictionsToDataset", "predictionsOutputExportPredictionsToDatasetColumnPrefix",
  "predictionsSurvivalTimeYAxisScale", "predictionsProbabilityYAxisScale",
  "predictionsDisplayPredictionInterval", "predictionsDisplayPredictionIntervalLevel", "predictionsMergeTablesAcrossMeasures", "plotLegend", "plotTheme", "colorPalette"
)
.sapCustomPredictions <- function(jaspResults, original, fitting, options) {

  if (options[["predictionsPredictFor"]] == "variables") return(NULL)
  selected <- any(vapply(.sapModelPredictionTimeOptions, function(name) options[[name]], logical(1))) ||
    (options[["predictionsOutcomeSurvivalTime"]] && length(.sapSelectedLifetimeSummaries(options)) > 0L)
  if (!selected) return(NULL)
  export <- options[["predictionsPredictFor"]] == "dataset" && options[["predictionsOutputExportPredictionsToDataset"]]
  if (!options[["predictionsOutputTable"]] && !options[["predictionsOutputPlot"]] && !export) return(NULL)
  if (!is.null(jaspResults[["customPredictionsResult"]])) {
    state <- jaspResults[["customPredictionsResult"]][["exportPlan"]]
    return(if (is.null(state)) NULL else state$object)
  }
  dependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies, .sapSimulationDependencies, .sapCustomPredictionDependencies)
  container    <- createJaspContainer(title = gettext("Predictions"))
  container$dependOn(dependencies)
  container$position <- 3
  jaspResults[["customPredictionsResult"]] <- container
  if (!.saSurvivalReady(options)) {
    table <- createJaspTable(title = gettext("Predictions"))
    table$addColumnInfo(name = "row", title = gettext("Observation"), type = "integer")
    container[["predictions"]] <- table
    return(NULL)
  }
  fits <- .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
  required <- unique(c(
    unlist(lapply(fits, function(fit) .sapModelVariables(attr(fit, "modelTerms"))), use.names = FALSE),
    unlist(options[["subgroup"]], use.names = FALSE)
  ))
  setup <- try(list(
    specification = .sapCustomPredictionSpecification(options, .sapPlotTimeFit(fits)),
    input         = .sapCustomPredictionInput(original, fitting, options, required)
  ), silent = TRUE)
  if (inherits(setup, "try-error")) {
    .sapCustomPredictionError(container, "inputError", gettext("Predictions"), setup)
    return(NULL)
  }
  specification <- setup[["specification"]]
  labels        <- .sapPredictionFitLabels(fits, options)
  records       <- lapply(fits, function(fit) try(.sapCustomPredictionRecord(setup[["input"]], original, fit, specification, options), silent = TRUE))
  outputs       <- list()
  for (kind in c("plots", "tables")) {
    if (!options[[if (kind == "plots") "predictionsOutputPlot" else "predictionsOutputTable"]])
      next
    output <- createJaspContainer(title = "")
    output$position <- if (kind == "plots") 1 else 2
    container[[kind]] <- output
    outputs[[kind]] <- output
  }
  for (i in seq_along(records)) {
    record <- records[[i]]
    key    <- paste0("model", i)
    if (inherits(record, "try-error")) {
      .sapCustomPredictionError(container, key, if (nzchar(labels[i])) labels[i] else gettext("Predictions"), record)
      next
    }
    for (kind in names(outputs)) {
      model <- outputs[[kind]]
      if (nzchar(labels[i])) {
        model <- createJaspContainer(title = labels[i])
        model$position <- i
        outputs[[kind]][[key]] <- model
      }
      if (kind == "plots")
        .sapCustomPredictionPlots(model, record, specification, options)
      else
        .sapCustomPredictionTables(model, record, specification, options)
    }
    if (!options[["predictionsOutputTable"]]) {
      messages <- .sapCustomPredictionMessages(record, specification)
      if (length(messages)) {
        table <- createJaspTable(title = gettext("Prediction Notes"))
        table$position <- 3
        container[[paste0("messages", i)]] <- table
        .saFillMessageTable(table, unique(messages))
      }
    }
  }
  if (!export) return(NULL)
  plan  <- list(columns = .sapCustomPredictionColumns(records, specification, original, options), dependencies = dependencies)
  state <- createJaspState()
  container[["exportPlan"]] <- state
  state$object <- plan
  return(plan)
}

.sapCustomPredictionError <- function(container, key, title, error) {

  table <- createJaspTable(title = title)
  container[[key]] <- table
  table$setError(conditionMessage(attr(error, "condition")))
}

.sapCustomPredictionRecord <- function(input, original, fit, specification, options) {

  if (inherits(fit, "try-error")) stop(attr(fit, "condition"))
  rows       <- .sapCustomPredictionRows(input, fit, options, original)
  prepared   <- .sapCustomPredictionData(input, rows, fit, options)
  prediction <- .sapCustomPredictionValues(fit, prepared[["data"]], specification, options)
  return(c(list(fit = fit), prepared, prediction))
}

.sapCustomPredictionEvaluations <- function(measure) {

  return(if (measure[["type"]] == "quantile") length(measure[["probability"]]) else if (measure[["kind"]] == "time") length(measure[["times"]]) else 1L)
}

.sapCustomPredictionValues <- function(fit, data, specification, options) {

  levels    <- specification[["levels"]]
  direction <- specification[["direction"]]
  attr(fit, "predictionX") <- attr(fit, "predictionLabels") <- NULL
  values   <- list()
  warnings <- character(0)
  for (measure in specification[["measures"]]) {
    evaluations <- .sapCustomPredictionEvaluations(measure)
    expected    <- nrow(data) * evaluations
    record      <- list(estimate = numeric(expected))
    if (!nrow(data)) {
      for (i in seq_along(levels)) for (side in .sapBoundFields(direction, "lower", "upper")) record[[paste0(side, i)]] <- numeric(0)
      values[[length(values) + 1L]] <- as.data.frame(record)
      next
    }
    at        <- if (measure[["type"]] == "quantile") measure[["probability"]] else measure[["times"]]
    arguments <- .sapPredictionArguments(fit, options, measure[["type"]], at, measure[["id"]])
    arguments[["newdata"]] <- data
    arguments[["ci"]] <- length(levels) > 0L
    arguments[["direction"]] <- direction
    for (i in seq_len(max(1L, length(levels)))) {
      arguments[["cl"]] <- if (length(levels)) levels[i] else .95
      result   <- do.call(.sapSummaryPredictions, arguments)
      warnings <- c(warnings, attr(result, "predictionWarnings"))
      if (i == 1L) record[["estimate"]] <- .sapCustomPredictionVector(result, "est", fit, nrow(data), evaluations)
      if (length(levels)) for (side in .sapBoundFields(direction, "lower", "upper")) record[[paste0(side, i)]] <- .sapCustomPredictionVector(result, if (side == "lower") "lcl" else "ucl", fit, nrow(data), evaluations)
    }
    if (any(lengths(record) != expected)) stop(gettext("The model predictions could not be matched to the input observations."))
    values[[length(values) + 1L]] <- as.data.frame(record)
  }
  if (!nrow(data)) warnings <- gettext("No observations with supported predictor values are available for this model.")
  return(list(values = values, warnings = unique(warnings)))
}

.sapCustomPredictionVector <- function(result, column, fit, rows, evaluations = 1L) {

  values <- unlist(lapply(result, function(row) row[[column]]), use.names = FALSE)
  if (fit[["ncovs"]] == 0L && length(values) == evaluations) values <- rep(values, rows)
  return(values)
}

.sapCustomPredictionTables <- function(container, record, specification, options) {

  # Lifetime percentiles, median and mean retain their own output tables.
  lifetime <- which(vapply(specification[["measures"]], function(measure) measure[["kind"]] == "lifetime", logical(1)))
  for (type in unique(vapply(specification[["measures"]][lifetime], `[[`, character(1), "type"))) {
    indices <- lifetime[vapply(specification[["measures"]][lifetime], function(measure) measure[["type"]] == type, logical(1))]
    table   <- .sapCustomPredictionTable(record, specification, indices)
    table$title <- .sapPredictionTitle(type, options)
    table$position <- min(indices)
    container[[paste0(type, "Table")]] <- table
  }
  timed  <- which(vapply(specification[["measures"]], function(measure) measure[["kind"]] == "time", logical(1)))
  groups <- if (options[["predictionsMergeTablesAcrossMeasures"]] && length(timed)) list(timed) else as.list(timed)
  for (indices in groups) {
    table <- .sapCustomPredictionTable(record, specification, indices, timed = TRUE)
    table$title <- if (length(indices) == 1L) specification[["measures"]][[indices]][["label"]] else gettext("At Specified Times")
    table$position <- min(indices)
    container[[paste0("timeTable", min(indices))]] <- table
  }

  return()
}

.sapCustomPredictionTableColumns <- function(table, specification, indices) {

  for (i in indices) {
    label  <- specification[["measures"]][[i]][["label"]]
    prefix <- paste0("measure", i, ".")
    table$addColumnInfo(name = paste0(prefix, "estimate"), title = gettext("Estimate"), overtitle = label, type = "number")
    for (j in seq_along(specification[["levels"]])) {
      level <- 100 * specification[["levels"]][j]
      if (specification[["direction"]] != "upper") table$addColumnInfo(name = paste0(prefix, "lower", j), title = gettextf("Lower %1$s%%", level), overtitle = label, type = "number")
      if (specification[["direction"]] != "lower") table$addColumnInfo(name = paste0(prefix, "upper", j), title = gettextf("Upper %1$s%%", level), overtitle = label, type = "number")
    }
  }
}

.sapCustomPredictionTable <- function(record, specification, indices, timed = FALSE) {

  table <- createJaspTable(title = if (timed) gettext("At Specified Times") else gettext("Lifetime Summaries"))
  table$addColumnInfo(name = "row", title = gettext("Observation"), type = "integer")
  if (timed) table$addColumnInfo(name = "time", title = gettext("Time or Horizon"), type = "number")
  for (j in seq_along(record[["data"]])) table$addColumnInfo(name = paste0("predictor", j), title = jaspBase::decodeColNames(names(record[["data"]])[j]), type = if (is.factor(record[["data"]][[j]])) "string" else "number")
  .sapCustomPredictionTableColumns(table, specification, indices)
  evaluations <- if (timed) length(specification[["times"]]) else 1L
  data        <- data.frame(row = rep(record[["rows"]], each = evaluations))
  if (timed) data[["time"]] <- rep(specification[["times"]], times = length(record[["rows"]]))
  for (j in seq_along(record[["data"]])) data[[paste0("predictor", j)]] <- rep(record[["data"]][[j]], each = evaluations)
  for (i in indices) {
    value <- record[["values"]][[i]]
    names(value) <- paste0("measure", i, ".", names(value))
    data <- cbind(data, value)
  }
  table$setData(data)
  table$showSpecifiedColumnsOnly <- TRUE
  for (message in .sapCustomPredictionMessages(record, specification)) table$addFootnote(message)
  return(table)
}

.sapCustomPredictionMessages <- function(record, specification) {

  messages <- c(record[["warnings"]], record[["referenceNotes"]])
  if (record[["omitted"]] > 0L) messages <- c(messages, gettextf("%1$i observations have missing or unsupported predictor values and were not predicted.", record[["omitted"]]))
  if (.sapConstraintActive(record[["fit"]]) && length(specification[["levels"]])) messages <- c(messages, gettext("Confidence intervals are unavailable because the fitted model is on the minimum-spread boundary."))
  return(unique(messages))
}

.sapCustomPredictionPlots <- function(container, record, specification, options) {

  measures   <- specification[["measures"]]
  percentile <- which(vapply(measures, function(measure) measure[["type"]] == "quantile", logical(1)))
  plots      <- measures[vapply(measures, function(measure) measure[["type"]] != "quantile", logical(1))]
  if (length(percentile)) plots <- c(list(list(type = "quantile", probability = specification[["plotPercentiles"]],
    label = gettext("Survival Time"), kind = "lifetime", id = "quantile")), plots)
  for (i in seq_along(plots)) {
    plot <- createJaspPlot(title = plots[[i]][["label"]], width = 550, height = 320)
    plot$position <- i
    container[[paste0("plot", i)]] <- plot
    .sapFillCustomPredictionPlot(plot, record, specification, options, plots[[i]])
  }

  return()
}

.sapFillCustomPredictionPlot <- function(plot, record, specification, options, measure) {

  scalar <- measure[["type"]] %in% c("mean", "median")
  if (measure[["kind"]] == "time") measure[["times"]] <- specification[["plotTimes"]]
  curveSpec <- specification
  curveSpec[["measures"]] <- list(measure)
  prediction <- try(.sapCustomPredictionValues(record[["fit"]], record[["data"]], curveSpec, options), silent = TRUE)
  if (inherits(prediction, "try-error")) {
    plot$setError(conditionMessage(attr(prediction, "condition")))
    return()
  }
  value <- prediction[["values"]][[1L]]
  at    <- if (scalar) 0 else if (measure[["type"]] == "quantile") measure[["probability"]] else measure[["times"]]
  data  <- data.frame(at = rep(at, times = length(record[["rows"]])), estimate = value[["estimate"]],
    Observation = factor(rep(record[["rows"]], each = length(at)), levels = record[["rows"]]))
  series <- unique(at)
  data[["Series"]] <- factor(data[["at"]], levels = series,
    labels = if (measure[["type"]] == "quantile") paste0(100 * series, "%") else as.character(series))
  data[["estimate"]][!is.finite(data[["estimate"]])] <- NA_real_
  if (!nrow(data) || !any(is.finite(data[["estimate"]]))) {
    plot$setError(gettext("No finite predictions are available for this plot."))
    return()
  }
  if (scalar) {
    figure <- ggplot2::ggplot(data, ggplot2::aes(x = Observation, y = estimate, group = 1))
    if (options[["predictionsDisplayPredictionInterval"]]) {
      rangeSpec <- specification
      level     <- options[["predictionsDisplayPredictionIntervalLevel"]]
      rangeSpec[["levels"]] <- numeric(0)
      rangeSpec[["measures"]] <- list(list(type = "quantile", probability = unname(.sapBoundProbabilities(level, "twoSided")), id = "quantile", kind = "lifetime"))
      predictionRange <- try(.sapCustomPredictionValues(record[["fit"]], record[["data"]], rangeSpec, options), silent = TRUE)
      if (inherits(predictionRange, "try-error")) {
        plot$setError(conditionMessage(attr(predictionRange, "condition")))
        return()
      }
      ranges <- matrix(predictionRange[["values"]][[1L]][["estimate"]], ncol = 2L, byrow = TRUE)
      data[["rangeLower"]] <- ranges[, 1L]
      data[["rangeUpper"]] <- ranges[, 2L]
      figure <- figure + ggplot2::geom_linerange(data = data, ggplot2::aes(ymin = rangeLower, ymax = rangeUpper), colour = "grey70", linewidth = 2, na.rm = TRUE)
    }
  } else figure <- ggplot2::ggplot(data, ggplot2::aes(x = Observation, y = estimate, colour = Series, group = Series))
  for (j in seq_along(specification[["levels"]])) {
    data[["lCi"]] <- if (specification[["direction"]] == "upper") NA_real_ else value[[paste0("lower", j)]]
    data[["uCi"]] <- if (specification[["direction"]] == "lower") NA_real_ else value[[paste0("upper", j)]]
    data[["lCi"]][!is.finite(data[["lCi"]])] <- NA_real_
    data[["uCi"]][!is.finite(data[["uCi"]])] <- NA_real_
    data <- .sapPlotBoundData(data, specification[["direction"]])
    figure <- figure + ggplot2::geom_errorbar(data = data, ggplot2::aes(ymin = boundLower, ymax = boundUpper), width = .18, na.rm = TRUE)
  }
  if (length(record[["rows"]]) > 1L) figure <- figure + jaspGraphs::geom_line(na.rm = TRUE)
  figure <- figure + ggplot2::geom_point(na.rm = TRUE) + ggplot2::scale_x_discrete(drop = FALSE)
  if (!scalar) {
    colorScale <- jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]])
    colorScale$palette <- .sapPlotPalette(colorScale$palette, nlevels(data[["Series"]]))
    figure <- figure + colorScale
  }
  figure <- figure + .sapPredictionOutcomeScale(options, measure[["type"]])
  figure <- figure + ggplot2::labs(x = gettext("Observation"),
    y = measure[["label"]], colour = if (scalar) NULL else if (measure[["type"]] == "quantile") gettext("Lifetime percentile") else gettext("Time"),
    fill = NULL, caption = NULL) + ggplot2::guides(fill = "none")
  axes <- try(.sapPredictionPrettyAxes(figure, probabilityAxis = measure[["type"]] == "survival"), silent = TRUE)
  if (inherits(axes, "try-error")) {
    plot$setError(conditionMessage(attr(axes, "condition")))
    return()
  }
  figure <- axes
  figure <- .sapPredictionPlotAddTheme(figure, options)
  if (length(series) == 1L) figure <- figure + ggplot2::theme(legend.position = "none")
  for (layer in figure$layers) if (inherits(layer$geom, "GeomRibbon")) layer$show.legend <- FALSE
  size <- .sapPredictionPlotSize(figure, options, 550, 320)
  plot$width <- size[["width"]]
  plot$height <- size[["height"]]
  plot$plotObject <- figure

  return()
}

.sapCustomPredictionColumns <- function(records, specification, original, options) {

  columns       <- list()
  prefixOptions <- options
  prefixOptions[["exportColumnPrefix"]] <- options[["predictionsOutputExportPredictionsToDatasetColumnPrefix"]]
  for (record in records) {
    if (inherits(record, "try-error")) next
    fit      <- record[["fit"]]
    prefix   <- .saExportModelPrefix(fit, prefixOptions, length(records) > 1L)
    identity <- .saExportFitIdentity(fit)
    for (i in seq_along(record[["values"]])) {
      measure     <- specification[["measures"]][[i]]
      evaluations <- .sapCustomPredictionEvaluations(measure)
      for (at in seq_len(evaluations)) for (field in names(record[["values"]][[i]])) {
        title <- measure[["label"]]
        if (measure[["kind"]] == "time") title <- gettextf("%1$s at %2$s", title, sprintf("%.17g", measure[["times"]][at]))
        suffix <- if (field == "estimate") title else {
          level <- as.integer(sub("lower|upper", "", field))
          paste(title, .sapBoundTitle(specification[["levels"]][level], specification[["direction"]]), if (startsWith(field, "lower")) gettext("lower") else gettext("upper"))
        }
        values  <- record[["values"]][[i]][[field]][seq.int(at, length.out = length(record[["rows"]]), by = evaluations)]
        aligned <- rep(NA_real_, nrow(original))
        aligned[record[["rows"]]] <- ifelse(is.finite(values), values, NA_real_)
        semantic <- list(producer = "custom", identity = identity, measure = measure[["id"]],
          percentile = if (measure[["type"]] == "quantile") measure[["probability"]][at], time = if (measure[["kind"]] == "time") measure[["times"]][at],
          field = sub("[0-9]+$", "", field), level = if (field != "estimate") specification[["levels"]][as.integer(sub("lower|upper", "", field))],
          direction = if (field != "estimate") specification[["direction"]])
        columns[[length(columns) + 1L]] <- list(name = paste0(prefix, suffix), value = aligned, type = "scale", semantic = semantic)
      }
    }
  }
  return(columns)
}
