# Per-observation exports follow the same model selection as the displayed output.
.sapmExportOptions <- c(
  "exportResidualsResponse", "exportResidualsCoxSnell", "exportFittedMean", "exportFittedMedian",
  "exportMixtureProbabilities", "exportMixtureClassification"
)

.sapmExportColumns <- function(jaspResults, options) {

  if (!.saSurvivalReady(options) || !any(vapply(.sapmExportOptions, function(name) options[[name]], logical(1))))
    return()
  if (!is.null(jaspResults[["exportColumns"]]))
    return()

  dependencies <- c(.sapGetDependencies(options), "interpretModel", "exportColumnPrefix", .sapmExportOptions)
  container <- createJaspContainer()
  container$dependOn(dependencies)
  container$position <- 6
  jaspResults[["exportColumns"]] <- container

  fits <- .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
  columns  <- list()
  messages <- character()
  for (fit in fits) {
    prefix <- .sapmExportModelPrefix(fit, options, length(fits) > 1)
    if (jaspBase::isTryError(fit)) {
      messages <- c(messages, paste0(prefix, jaspBase::.extractErrorMessage(fit)))
      next
    }

    values <- .sapmExportValues(fit, options)
    for (name in names(values)) {
      columnName <- paste0(prefix, name)
      value <- values[[name]]
      if (jaspBase::isTryError(value)) {
        messages <- c(messages, paste0(columnName, ": ", conditionMessage(attr(value, "condition"))))
        next
      }
      if (any(!is.finite(value))) {
        messages <- c(messages, gettextf("%1$s: non-finite values were exported as missing.", columnName))
        value[!is.finite(value)] <- NA_real_
      }
      columns[[columnName]] <- .sapmExportAlign(fit, value)
    }
  }

  # Check all names before writing any columns, including names owned by other analyses.
  for (name in names(columns)) {
    if (jaspBase:::columnExists(name) && !jaspBase:::columnIsMine(name))
      .quitAnalysis(gettextf("The column '%1$s' already exists. Specify a different export prefix.", name))
  }
  for (name in names(columns)) {
    container[[name]] <- createJaspColumn(columnName = name, dependencies = dependencies)
    if (endsWith(name, gettext("Component index")))
      container[[name]]$setNominal(columns[[name]])
    else
      container[[name]]$setScale(columns[[name]])
  }

  if (length(messages) > 0) {
    table <- createJaspTable(title = gettext("Export"))
    container[["messages"]] <- table
    table$addColumnInfo(name = "message", title = gettext("Message"), type = "string")
    table$setData(data.frame(message = unique(messages)))
  }

  return()
}

.sapmExportAlign <- function(fit, values) {

  dataset  <- attr(fit, "dataset")
  rowNames <- attr(dataset, "exportRowNames")
  rows     <- match(rownames(dataset), rowNames)
  if (length(values) != nrow(dataset) || anyNA(rows))
    stop(gettext("Exported values could not be matched to the original observations."))

  aligned <- rep(NA_real_, length(rowNames))
  aligned[rows] <- values

  return(aligned)
}

.sapmExportValues <- function(fit, options) {

  dataset <- attr(fit, "dataset")
  values  <- list()
  residualsAvailable <- options[["censoringType"]] != "interval"
  if (options[["exportFittedMean"]] || (options[["exportResidualsResponse"]] && residualsAvailable)) {
    mean <- try(stats::predict(fit, newdata = dataset, type = "response")[[".pred_time"]], silent = TRUE)
    if (options[["exportFittedMean"]])
      values[[gettext("Fitted mean survival time")]] <- mean
    if (options[["exportResidualsResponse"]] && residualsAvailable) {
      time <- dataset[[if (options[["censoringType"]] == "counting") options[["intervalEnd"]] else options[["timeToEvent"]]]]
      values[[gettext("Response residual")]] <- if (jaspBase::isTryError(mean)) mean else time - mean
    }
  }
  if (options[["exportFittedMedian"]])
    values[[gettext("Fitted median survival time")]] <- try(stats::predict(fit, newdata = dataset, type = "quantile", p = 0.5)[[".pred_quantile"]], silent = TRUE)
  if (options[["exportResidualsCoxSnell"]] && residualsAvailable)
    values[[gettext("Cox-Snell residual")]] <- try(stats::residuals(fit, type = "coxsnell"), silent = TRUE)

  if (options[["exportMixtureProbabilities"]] || options[["exportMixtureClassification"]]) {
    posterior <- if (attr(fit, "components") == 1) matrix(1, nrow(dataset), 1) else attr(fit, "mixture")[["posterior"]]
    if (options[["exportMixtureProbabilities"]]) {
      for (k in seq_len(ncol(posterior)))
        values[[gettextf("Component %1$i probability", k)]] <- posterior[, k]
    }
    if (options[["exportMixtureClassification"]])
      values[[gettext("Component index")]] <- max.col(posterior, ties.method = "first")
  }

  return(values)
}

.sapmExportModelPrefix <- function(fit, options, multiple) {

  parts <- trimws(options[["exportColumnPrefix"]])
  if (multiple) {
    family <- switch(attr(fit, "family"),
      "exp" = "EXP", "gamma" = "GA", "gengamma" = "GG", "gengamma.orig" = "GGO",
      "genf" = "GF", "genf.orig" = "GFO", "gompertz" = "GO", "llogis" = "LL",
      "lnorm" = "LN", "weibull" = "WB"
    )
    model <- match(attr(fit, "modelId"), vapply(options[["modelTerms"]], function(x) x[["name"]], character(1)))
    modelPrefix <- paste0(family, "-M", model, "-Mix", attr(fit, "components"))
    if (options[["subgroup"]] != "") {
      subgroup <- if (attr(fit, "subgroupLabel") == gettext("Full dataset")) "All" else paste0("G-", attr(fit, "subgroup"))
      modelPrefix <- paste0(modelPrefix, "-", subgroup)
    }
    parts <- c(parts, modelPrefix)
  }
  parts <- parts[nzchar(parts)]

  return(if (length(parts) > 0) paste0(paste(parts, collapse = ": "), ": ") else "")
}
