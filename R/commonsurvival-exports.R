# Per-observation exports follow the same model selection as the displayed output.
.saExportOptions <- function(options) {

  return(c("exportResidualsCoxSnell", if (options[["analysisType"]] == "semiparametric")
    c("exportResidualsMartingale", "exportResidualsDeviance", "exportFittedRisk", "exportFittedLinearPredictor") else
    c("exportResidualsResponse", "exportFittedMean", "exportFittedMedian"),
    if (options[["analysisType"]] == "mixture") c("exportMixtureProbabilities", "exportMixtureClassification")))
}

.saExportColumns <- function(jaspResults, options) {

  exportOptions <- .saExportOptions(options)
  if (!.saSurvivalReady(options) || !any(vapply(exportOptions, function(name) options[[name]], logical(1))))
    return()
  if (!is.null(jaspResults[["exportColumns"]]))
    return()

  cox <- options[["analysisType"]] == "semiparametric"
  dependencies <- c(if (cox) .saspDependencies else c(.sapGetDependencies(options), "interpretModel"),
    "exportColumnPrefix", exportOptions)
  container <- createJaspContainer()
  container$dependOn(dependencies)
  container$position <- if (cox) 10 else 6
  jaspResults[["exportColumns"]] <- container

  fits <- if (cox) list(jaspResults[["fit"]]$object) else
    .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
  columns  <- list()
  messages <- character()
  for (fit in fits) {
    prefix <- .saExportModelPrefix(fit, options, length(fits) > 1)
    if (jaspBase::isTryError(fit)) {
      messages <- c(messages, paste0(prefix, jaspBase::.extractErrorMessage(fit)))
      next
    }

    values <- if (cox) .saspExportValues(fit, options) else .sapExportValues(fit, options)
    for (name in names(values)) {
      columnName <- paste0(prefix, name)
      record <- values[[name]]
      value  <- record[["value"]]
      if (jaspBase::isTryError(value)) {
        messages <- c(messages, paste0(columnName, ": ", conditionMessage(attr(value, "condition"))))
        next
      }
      if (any(!is.finite(value))) {
        messages <- c(messages, gettextf("%1$s: non-finite values were exported as missing.", columnName))
        value[!is.finite(value)] <- NA_real_
      }
      columns[[columnName]] <- .saExportValue(.saExportAlign(fit, value), type = record[["type"]])
    }
  }

  # Check all names before writing any columns, including names owned by other analyses.
  for (name in names(columns)) {
    if (jaspBase:::columnExists(name) && !jaspBase:::columnIsMine(name))
      .quitAnalysis(gettextf("The column '%1$s' already exists. Specify a different export prefix.", name))
  }
  for (name in names(columns)) {
    container[[name]] <- createJaspColumn(columnName = name, dependencies = dependencies)
    if (columns[[name]][["type"]] == "nominal")
      container[[name]]$setNominal(columns[[name]][["value"]])
    else
      container[[name]]$setScale(columns[[name]][["value"]])
  }

  if (length(messages) > 0) {
    table <- createJaspTable(title = gettext("Export"))
    container[["messages"]] <- table
    .saFillMessageTable(table, unique(messages))
  }

  return()
}

.saFillMessageTable <- function(table, messages) {

  table$addColumnInfo(name = "message", title = gettext("Message"), type = "string")
  table$setData(data.frame(message = messages))

  return()
}

.saExportAlign <- function(fit, values) {

  dataset  <- attr(fit, "dataset")
  rowNames <- attr(dataset, "exportRowNames")
  rows     <- match(rownames(dataset), rowNames)
  if (length(values) != nrow(dataset) || anyNA(rows))
    stop(gettext("Exported values could not be matched to the original observations."))

  aligned <- rep(NA_real_, length(rowNames))
  aligned[rows] <- values

  return(aligned)
}

.saExportValue <- function(value, type = "scale") {
  return(list(value = value, type = type))
}

.sapExportValues <- function(fit, options) {

  dataset <- attr(fit, "dataset")
  values  <- list()
  residualsAvailable <- options[["censoringType"]] != "interval"
  if (options[["exportFittedMean"]] || (options[["exportResidualsResponse"]] && residualsAvailable)) {
    mean <- try(stats::predict(fit, newdata = dataset, type = "response")[[".pred_time"]], silent = TRUE)
    if (options[["exportFittedMean"]])
      values[[gettext("Fitted mean survival time")]] <- .saExportValue(mean)
    if (options[["exportResidualsResponse"]] && residualsAvailable) {
      time <- dataset[[if (options[["censoringType"]] == "counting") options[["intervalEnd"]] else options[["timeToEvent"]]]]
      values[[gettext("Response residual")]] <- .saExportValue(if (jaspBase::isTryError(mean)) mean else time - mean)
    }
  }
  if (options[["exportFittedMedian"]])
    values[[gettext("Fitted median survival time")]] <- .saExportValue(try(stats::predict(fit, newdata = dataset, type = "quantile", p = 0.5)[[".pred_quantile"]], silent = TRUE))
  if (options[["exportResidualsCoxSnell"]] && residualsAvailable)
    values[[gettext("Cox-Snell residual")]] <- .saExportValue(try(stats::residuals(fit, type = "coxsnell"), silent = TRUE))

  if (options[["analysisType"]] == "mixture" && (options[["exportMixtureProbabilities"]] || options[["exportMixtureClassification"]])) {
    posterior <- if (attr(fit, "components") == 1) matrix(1, nrow(dataset), 1) else attr(fit, "mixture")[["posterior"]]
    if (options[["exportMixtureProbabilities"]]) {
      for (k in seq_len(ncol(posterior)))
        values[[gettextf("Component %1$i probability", k)]] <- .saExportValue(posterior[, k])
    }
    if (options[["exportMixtureClassification"]])
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
  if (options[["exportFittedRisk"]])
    values[[gettext("Fitted relative risk")]] <- .saExportValue(try(stats::predict(fit, type = "risk"), silent = TRUE))
  if (options[["exportFittedLinearPredictor"]])
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
    model <- match(attr(fit, "modelId"), vapply(options[["modelTerms"]], function(x) x[["name"]], character(1)))
    modelPrefix <- paste0(family, "-M", model,
      if (options[["analysisType"]] == "mixture") paste0("-Mix", attr(fit, "components")))
    if (.sapHasSubgroups(options)) {
      subgroup <- if (attr(fit, "subgroupLabel") == gettext("Full dataset")) "All" else paste0("G-", attr(fit, "subgroup"))
      modelPrefix <- paste0(modelPrefix, "-", subgroup)
    }
    parts <- c(parts, modelPrefix)
  }
  parts <- parts[nzchar(parts)]

  return(if (length(parts) > 0) paste0(paste(parts, collapse = ": "), ": ") else "")
}
