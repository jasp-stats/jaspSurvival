.sargExportDefinitions <- list(
  exportResidualsRegression = list(type = "regression", suffix = "RegressionResidual"),
  exportResidualsResponse   = list(type = "response", suffix = "ResponseResidual"),
  exportResidualsPearson    = list(type = "pearson", suffix = "PearsonResidual"),
  exportResidualsDeviance   = list(type = "deviance", suffix = "DevianceResidual"),
  exportResidualsQuantile   = list(type = "quantile", suffix = "QuantileResidual"),
  exportFittedValuesCumulativeFailures = list(type = "cumulative", suffix = "CumulativeEvents"),
  exportFittedValuesIntervalFailures   = list(type = "interval", suffix = "IntervalEvents")
)
.sargExportOptions <- names(.sargExportDefinitions)

.sargExportRecords <- function(fits, options, nRows, quantiles) {

  selected <- .sargExportDefinitions[vapply(.sargExportOptions, function(name) options[[name]] &&
    (.sargExportDefinitions[[name]][["type"]] != "regression" || .sargMethod(options) == "LS"), logical(1))]
  if (!length(selected))
    return(list(columns = list(), messages = character(0)))
  prefix <- trimws(options[["exportColumnPrefix"]])
  if (!nzchar(prefix))
    .quitAnalysis(gettext("Enter a non-empty prefix for exported columns."))
  columns <- list()
  messages <- character(0)
  for (definition in selected) {
    values <- rep(NA_real_, nRows)
    for (i in seq_along(fits)) {
      fit <- fits[[i]]
      value <- try(switch(definition[["type"]], cumulative = .sargFittedCumulativeFailures(fit), interval = .sargIntervalMean(fit),
        .sargResidualValue(fit, definition[["type"]], quantiles[[i]])), silent = TRUE)
      if (jaspBase::isTryError(value)) {
        messages <- c(messages, gettextf("%1$s, %2$s: %3$s", fit[["label"]], definition[["suffix"]], jaspBase::.extractErrorMessage(value)))
        next
      }
      if (any(!is.finite(value))) {
        messages <- c(messages, gettextf("%1$s, %2$s: non-finite values were exported as missing.", fit[["label"]], definition[["suffix"]]))
        value[!is.finite(value)] <- NA_real_
      }
      values[fit[["rows"]]] <- value
    }
    columns[[definition[["suffix"]]]] <- list(name = paste0(prefix, "_", definition[["suffix"]]), value = values, type = "scale")
  }
  return(list(columns = columns, messages = unique(messages)))
}

.sargExport <- function(jaspResults, dataset, options) {

  if (!.sargReady(options) || !any(vapply(.sargExportOptions, function(name) options[[name]], logical(1))) ||
      !is.null(jaspResults[["exportColumns"]]))
    return()
  fits <- .sargFits(jaspResults, valid = TRUE)
  if (!length(fits))
    return()
  quantiles <- if (options[["exportResidualsQuantile"]]) .sargQuantileState(jaspResults, options, fits)
  records <- .sargExportRecords(fits, options, nrow(dataset), quantiles)
  if (!length(records[["columns"]]))
    return()
  .saExportPreflight(records[["columns"]])
  container <- createJaspContainer()
  dependencies <- c(.sargFitDependencies, .sargExportOptions, "exportColumnPrefix")
  container$dependOn(dependencies)
  container$position <- 10
  jaspResults[["exportColumns"]] <- container
  for (key in names(records[["columns"]])) {
    record <- records[["columns"]][[key]]
    container[[key]] <- createJaspColumn(columnName = record[["name"]], dependencies = dependencies)
    .saSetExportColumn(container[[key]], record)
  }
  .saExportMessages(container, records[["messages"]])
  return()
}
