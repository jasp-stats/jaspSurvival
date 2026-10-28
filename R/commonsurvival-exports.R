# Per-observation exports follow the same model selection as the displayed output.
.saExportOptions <- function(options) {

  return(c("exportResidualsCoxSnell", if (options[["analysisType"]] == "semiparametric")
    c("exportResidualsMartingale", "exportResidualsDeviance", "exportFittedValuesRelativeRisk", "exportFittedValuesLinearPredictor") else
    c("exportResidualsResponse", "exportFittedValuesMeanSurvivalTime", "exportFittedValuesMedianSurvivalTime"),
    if (options[["analysisType"]] == "mixture") c("exportMixtureClassificationComponentProbabilities", "exportMixtureClassificationComponentIndex")))
}

.saExportColumns <- function(jaspResults, options, additional = NULL, informationChanged = FALSE) {

  if (options[["analysisType"]] != "semiparametric")
    return(.sapExportColumnPlan(jaspResults, options, additional, informationChanged))

  exportOptions <- .saExportOptions(options)
  if (!.saSurvivalReady(options) || !any(vapply(exportOptions, function(name) options[[name]], logical(1))))
    return()
  if (!is.null(jaspResults[["exportColumns"]]))
    return()

  dependencies <- c(.saspDependencies, "exportColumnPrefix", exportOptions)
  container    <- createJaspContainer()
  container$dependOn(dependencies)
  container$position <- 601
  jaspResults[["exportColumns"]] <- container

  prepared <- .saExportRecords(list(jaspResults[["fit"]]$object), options, .saspExportValues)
  .saExportPreflight(prepared[["columns"]])
  for (record in prepared[["columns"]]) {
    name <- record[["name"]]
    container[[name]] <- createJaspColumn(columnName = name, dependencies = dependencies)
    .saSetExportColumn(container[[name]], record)
  }

  .saExportMessages(container, prepared[["messages"]])

  return()
}

# Prepare row-aligned column records before modifying the dataset.
.saExportRecords <- function(fits, options, valueFunction) {

  columns  <- list()
  messages <- character(0)
  for (fit in fits) {
    prefix <- .saExportModelPrefix(fit, options, length(fits) > 1L)
    if (jaspBase::isTryError(fit)) {
      messages <- c(messages, paste0(prefix, jaspBase::.extractErrorMessage(fit)))
      next
    }
    values <- valueFunction(fit, options)
    for (name in names(values)) {
      record     <- values[[name]]
      value      <- record[["value"]]
      columnName <- paste0(prefix, name)
      if (jaspBase::isTryError(value)) {
        messages <- c(messages, paste0(columnName, ": ", conditionMessage(attr(value, "condition"))))
        next
      }
      if (any(!is.finite(value))) {
        messages <- c(messages, gettextf("%1$s: non-finite values were exported as missing.", columnName))
        value[!is.finite(value)] <- NA_real_
      }
      column <- list(name = columnName, value = .saExportAlign(fit, value), type = record[["type"]])
      columns[[length(columns) + 1L]] <- column
    }
  }
  return(list(columns = columns, messages = messages))
}

.saSetExportColumn <- function(column, record) {

  if (record[["type"]] == "nominal")
    column$setNominal(record[["value"]])
  else
    column$setScale(record[["value"]])
}

.saExportMessages <- function(container, messages) {

  if (!length(messages))
    return()
  table <- createJaspTable(title = gettext("Export"))
  container[["messages"]] <- table
  .saFillMessageTable(table, unique(messages))
}

.saExportPreflight <- function(columns, columnExists = jaspBase:::columnExists, columnIsMine = jaspBase:::columnIsMine) {

  names <- vapply(columns, `[[`, character(1), "name")
  if (anyDuplicated(names))
    .quitAnalysis(gettextf(
      "More than one export requests the column '%1$s'. Specify distinct export prefixes.",
      names[which(duplicated(names))[1L]]
    ))
  for (i in seq_along(columns)) {
    name <- names[i]
    if (!columnExists(name))
      next
    if (!columnIsMine(name))
      .quitAnalysis(gettextf("The column '%1$s' already exists. Specify a different export prefix.", name))
  }
  return(invisible(TRUE))
}

# Merge fitted-observation exports and custom predictions under one ownership check.
.sapExportColumnPlan <- function(jaspResults, options, additional, informationChanged = FALSE) {

  exportOptions <- .saExportOptions(options)
  normal        <- any(vapply(exportOptions, function(name) options[[name]], logical(1)))
  if (!.saSurvivalReady(options) || (!normal && is.null(additional)) ||
    (!informationChanged && !is.null(jaspResults[["exportColumns"]])))
    return()
  dependencies <- unique(c(.sapGetDependencies(options), "interpretModel", "exportColumnPrefix", exportOptions,
    .sapCustomPredictionDependencies, .sapSelectedOutputDependencies, .sapSimulationDependencies, additional[["dependencies"]]))
  columns  <- list()
  messages <- character(0)
  if (normal) {
    fits     <- .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
    prepared <- .saExportRecords(fits, options, .sapExportValues)
    columns  <- prepared[["columns"]]
    messages <- prepared[["messages"]]
  }

  # Check the complete plan before creating or refreshing any columns.
  columns   <- c(columns, additional[["columns"]])
  container <- jaspResults[["exportColumns"]]
  # Desktop ownership permits refreshing this analysis's columns after settings change.
  .saExportPreflight(columns)
  if (is.null(container)) {
    container <- createJaspContainer()
    container$dependOn(dependencies)
    container$position <- 601
    jaspResults[["exportColumns"]] <- container
  }
  for (record in columns) {
    name     <- record[["name"]]
    existing <- !is.null(container[[name]])
    if (!existing)
      container[[name]] <- createJaspColumn(columnName = name, dependencies = dependencies)
    .saSetExportColumn(container[[name]], record)
  }
  .saExportMessages(container, messages)
  return()
}

.saFillMessageTable <- function(table, messages) {

  table$addColumnInfo(name = "message", title = gettext("Message"), type = "string")
  table$setData(data.frame(message = messages))

  return()
}

# Restore dataset order after fitting-row filtering.
.saExportAlign <- function(fit, values) {

  dataset      <- attr(fit, "dataset")
  rowNames     <- attr(dataset, "exportRowNames")
  rows         <- match(rownames(dataset), rowNames)
  if (length(values) != nrow(dataset) || anyNA(rows))
    stop(gettext("Exported values could not be matched to the original observations."))

  aligned <- rep(NA_real_, length(rowNames))
  # Replicas share fitted values, residuals and classification; export once.
  keep <- !duplicated(rows)
  aligned[rows[keep]] <- values[keep]

  return(aligned)
}

.saExportValue <- function(value, type = "scale") {

  return(list(value = value, type = type))
}

.sapExportValues <- function(fit, options) {

  dataset            <- attr(fit, "dataset")
  values             <- list()
  residualsAvailable <- options[["censoringType"]] != "interval"
  if (options[["exportFittedValuesMeanSurvivalTime"]] || (options[["exportResidualsResponse"]] && residualsAvailable)) {
    mean <- try(.sapCustomPredictionVector(.sapSummaryPredictions(fit, newdata = dataset, type = "mean", t = 0, ci = FALSE),
      "est", fit, nrow(dataset)), silent = TRUE)
    if (options[["exportFittedValuesMeanSurvivalTime"]])
      values[[gettext("Fitted mean survival time")]] <- .saExportValue(mean)
    if (options[["exportResidualsResponse"]] && residualsAvailable) {
      time <- dataset[[if (options[["censoringType"]] == "counting") options[["intervalEnd"]] else options[["timeToEvent"]]]]
      values[[gettext("Response residual")]] <- .saExportValue(if (jaspBase::isTryError(mean)) mean else time - mean)
    }
  }
  if (options[["exportFittedValuesMedianSurvivalTime"]])
    values[[gettext("Fitted median survival time")]] <- .saExportValue(try(stats::predict(fit, newdata = dataset, type = "quantile", p = 0.5)[[".pred_quantile"]], silent = TRUE))
  if (options[["exportResidualsCoxSnell"]] && residualsAvailable)
    values[[gettext("Cox-Snell residual")]] <- .saExportValue(try(.sapResidualValues(fit, type = "coxsnell"), silent = TRUE))

  if (options[["analysisType"]] == "mixture" && (options[["exportMixtureClassificationComponentProbabilities"]] || options[["exportMixtureClassificationComponentIndex"]])) {
    posterior <- if (attr(fit, "components") == 1) matrix(1, nrow(dataset), 1) else attr(fit, "mixture")[["posterior"]]
    if (options[["exportMixtureClassificationComponentProbabilities"]]) {
      for (k in seq_len(ncol(posterior)))
        values[[gettextf("Component %1$i probability", k)]] <- .saExportValue(posterior[, k])
    }
    if (options[["exportMixtureClassificationComponentIndex"]])
      values[[gettext("Component index")]] <- .saExportValue(max.col(posterior, ties.method = "first"), type = "nominal")
  }

  return(values)
}

.saspExportValues <- function(fit, options) {

  values <- list()
  if (options[["exportResidualsMartingale"]])
    values[[gettext("Martingale residual")]] <- .saExportValue(try(stats::residuals(fit, type = "martingale"), silent = TRUE))
  if (options[["exportResidualsDeviance"]])
    values[[gettext("Deviance residual")]] <- .saExportValue(try(stats::residuals(fit, type = "deviance"), silent = TRUE))
  if (options[["exportResidualsCoxSnell"]]) {
    # Martingale residual = event indicator - fitted cumulative hazard (also for counting-process data).
    values[[gettext("Cox-Snell residual")]] <- .saExportValue(try(fit[["y"]][, ncol(fit[["y"]])] - stats::residuals(fit, type = "martingale"), silent = TRUE))
  }
  if (options[["exportFittedValuesRelativeRisk"]])
    values[[gettext("Fitted relative risk")]] <- .saExportValue(try(stats::predict(fit, type = "risk"), silent = TRUE))
  if (options[["exportFittedValuesLinearPredictor"]])
    values[[gettext("Fitted linear predictor")]] <- .saExportValue(try(stats::predict(fit, type = "lp"), silent = TRUE))

  return(values)
}

.saExportModelPrefix <- function(fit, options, multiple) {

  parts <- trimws(options[["exportColumnPrefix"]])
  if (multiple) {
    family <- switch(attr(fit, "family"),
      "exp" = "EXP", "gamma" = "GA", "gengamma" = "GG", "gengamma.orig" = "GGO",
      "genf" = "GF", "genf.orig" = "GFO", "gompertz" = "GO", "llogis" = "LL",
      "lnorm" = "LN", "weibull" = "WB"
    )
    model       <- match(attr(fit, "modelId"), vapply(options[["modelTerms"]], function(x) x[["name"]], character(1)))
    modelPrefix <- paste0(family, "-M", model,
      if (options[["analysisType"]] == "mixture") paste0("-Mix", attr(fit, "components")))
    if (.sapHasSubgroups(options)) {
      subgroup    <- if (attr(fit, "subgroupLabel") == gettext("Full dataset")) "All" else paste0("G-", attr(fit, "subgroup"))
      modelPrefix <- paste0(modelPrefix, "-", subgroup)
    }
    parts <- c(parts, modelPrefix)
  }
  parts <- parts[nzchar(parts)]

  return(if (length(parts) > 0) paste0(paste(parts, collapse = ": "), ": ") else "")
}
