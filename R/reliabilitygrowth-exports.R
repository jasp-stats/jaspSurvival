.sargExportOptions <- c("exportResidualsRegression", "exportResidualsResponse", "exportResidualsPearson",
  "exportResidualsDeviance", "exportResidualsQuantile", "exportFittedValuesCumulativeFailures", "exportFittedValuesIntervalFailures")

.sargExportRecords <- function(fits, options, nRows, quantiles) {

  definitions <- list(
    list(option = "exportResidualsRegression", type = "regression", suffix = "RegressionResidual"),
    list(option = "exportResidualsResponse", type = "response", suffix = "ResponseResidual"),
    list(option = "exportResidualsPearson", type = "pearson", suffix = "PearsonResidual"),
    list(option = "exportResidualsDeviance", type = "deviance", suffix = "DevianceResidual"),
    list(option = "exportResidualsQuantile", type = "quantile", suffix = "QuantileResidual"),
    list(option = "exportFittedValuesCumulativeFailures", type = "cumulative", suffix = "CumulativeFailures"),
    list(option = "exportFittedValuesIntervalFailures", type = "interval", suffix = "IntervalFailures")
  )
  selected <- Filter(function(definition) options[[definition[["option"]]]] &&
    (definition[["type"]] != "regression" || .sargMethod(options) == "LS"), definitions)
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
      value <- try(switch(definition[["type"]], cumulative = .sargObserved(fit), interval = .sargIntervalMean(fit),
        .sargResidualValue(fit, definition[["type"]], quantiles[[i]])), silent = TRUE)
      if (inherits(value, "try-error")) {
        messages <- c(messages, gettextf("%1$s, %2$s: %3$s", fit[["label"]], definition[["suffix"]], jaspBase::.extractErrorMessage(value)))
        next
      }
      if (any(!is.finite(value))) {
        messages <- c(messages, gettextf("%1$s, %2$s: non-finite values were exported as missing.", fit[["label"]], definition[["suffix"]]))
        value[!is.finite(value)] <- NA_real_
      }
      values[fit[["rows"]]] <- value
    }
    columns[[definition[["suffix"]]]] <- list(name = paste0(prefix, "_", definition[["suffix"]]), value = values)
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
    column <- createJaspColumn(columnName = record[["name"]], dependencies = dependencies)
    container[[key]] <- column
    column$setScale(record[["value"]])
  }
  .saExportMessages(container, records[["messages"]])
  return()
}
