.sareCreateTable <- function(jaspResults, key, title, dependencies, position) {

  table <- createJaspTable(title = title)
  table$dependOn(c(.sareFitDependencies, dependencies))
  table$position <- position
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[[key]] <- table
  return(table)
}

.sareGroupColumn <- function(table, options) {

  if (.sapHasSubgroups(options))
    table$addColumnInfo(name = "subgroup", title = gettext("Subgroup"), type = "string")
  return()
}

.sareMessages <- function(jaspResults, options) {

  if (!is.null(jaspResults[["fitMessages"]]))
    return()
  fits <- .sareFits(jaspResults)
  rows <- lapply(fits, function(fit) {
    messages <- c(fit[["error"]], fit[["warnings"]])
    if (length(messages) > 0)
      data.frame(subgroup = fit[["label"]], message = messages)
  })
  data <- .saSafeRbind(rows)
  if (is.null(data))
    return()
  table <- .sareCreateTable(jaspResults, "fitMessages", gettext("Model Fitting"), character(0), 0)
  .sareGroupColumn(table, options)
  table$addColumnInfo(name = "message", title = gettext("Message"), type = "string")
  table$setData(data)
  return()
}

.sareStatistics <- function(jaspResults, options) {

  fits <- .sareFits(jaspResults, valid = TRUE)
  if (options[["dataSummary"]])
    .sareDataSummaryTable(jaspResults, options, fits)
  if (options[["modelSummary"]])
    .sareSummaryTable(jaspResults, options, fits)
  if (options[["coefficients"]])
    .sareCoefficientsTable(jaspResults, options, fits)
  if (options[["coefficientsCovarianceMatrix"]])
    .sareCovarianceTables(jaspResults, options, fits)
  if (options[["fittedModelEquation"]])
    .sareEquationsTable(jaspResults, options, fits)
  return()
}

.sareDataSummaryTable <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["dataSummaryTable"]]))
    return()
  table <- .sareCreateTable(jaspResults, "dataSummaryTable", gettext("Data Summary"), "dataSummary", 0.5)
  .sareGroupColumn(table, options)
  table$addColumnInfo(name = "intervals", title = gettext("Intervals"), type = "integer")
  table$addColumnInfo(name = "events", title = gettext("Events"), type = "integer")
  table$addColumnInfo(name = "time", title = gettext("Follow-up Time"), type = "number")
  table$addColumnInfo(name = "cumulativeMeanTimeBetweenEvents", title = gettext("Cumulative Mean Time Between Events"), type = "number")
  table$setData(.saSafeRbind(lapply(fits, function(fit) data.frame(
    subgroup                       = fit[["label"]],
    intervals                      = length(fit[["time"]]),
    events                         = sum(fit[["counts"]]),
    time                           = max(fit[["time"]]),
    cumulativeMeanTimeBetweenEvents = max(fit[["time"]]) / sum(fit[["counts"]])
  ))))
  return()
}

.sareSummaryTable <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()
  table <- .sareCreateTable(jaspResults, "summaryTable", gettext("Model Summary"), "modelSummary", 1)
  .sareGroupColumn(table, options)
  table$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
  table$addColumnInfo(name = "method", title = gettext("Estimation"), type = "string")
  table$addColumnInfo(name = "logLik", title = gettext("Log Lik."), type = "number")
  table$addColumnInfo(name = "df", title = "df", type = "integer")
  table$addColumnInfo(name = "aic", title = gettext("AIC"), type = "number", format = "dp:3")
  table$addColumnInfo(name = "bic", title = gettext("BIC"), type = "number", format = "dp:3")
  table$setData(.saSafeRbind(lapply(fits, function(fit) {
    native <- fit[["native"]]
    # Native grouped Poisson log likelihoods omit the count-factorial constant.
    constant <- if (fit[["method"]] == "MLE") sum(lgamma(fit[["counts"]] + 1)) else 0
    data.frame(
      subgroup = fit[["label"]], model = .sareModelLabel(fit[["model"]]), method = fit[["method"]],
      logLik = native[["logLik"]] - constant,
      aic    = native[["AIC"]] + 2 * constant,
      bic    = native[["BIC"]] + 2 * constant,
      df     = if (fit[["method"]] == "MLE") 2 else attr(stats::logLik(native[["model"]]), "df")
    )
  })))
  table$addCitation("ReliaGrowR (0.7): Reliability Growth Analysis and Repairable Systems Modeling. https://doi.org/10.32614/CRAN.package.ReliaGrowR")
  table$addFootnote(if (.sareMethod(options) == "MLE")
    gettext("Maximum likelihood uses interval event counts. Log-likelihood includes count-factorial terms; BIC uses the number of intervals. Optimizer convergence status is not supplied by ReliaGrowR.") else
    gettext("Fit statistics refer to the log-scale regression likelihood and cannot be compared directly with NHPP maximum-likelihood statistics. The parameter count includes residual variance."))
  return()
}

.sareCoefficientsTable <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["coefficientsTable"]]))
    return()
  table <- .sareCreateTable(jaspResults, "coefficientsTable", gettext("Coefficients Summary"),
    c("coefficients", "coefficientsConfidenceIntervals", "coefficientsConfidenceIntervalsLevel"), 2)
  .sareGroupColumn(table, options)
  table$addColumnInfo(name = "parameter", title = "", type = "string")
  table$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  table$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  if (options[["coefficientsConfidenceIntervals"]])
    .sapAddBoundColumns(table, options[["coefficientsConfidenceIntervalsLevel"]], "twoSided")
  table$setData(.saSafeRbind(lapply(fits, .sareCoefficientRows, level = options[["coefficientsConfidenceIntervalsLevel"]])))
  if (options[["model"]] == "piecewise")
    table$addFootnote(gettext("Coefficients describe the segmented regression of log cumulative events on log time. Change-point coefficients are reported as log times."))
  if (.sareMethod(options) == "MLE")
    table$addFootnote(gettext("Maximum-likelihood intervals use a normal approximation; positive parameters use the log scale."))
  return()
}

.sareEquationsTable <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["modelEquations"]]))
    return()
  table <- .sareCreateTable(jaspResults, "modelEquations", gettext("Fitted Model Equations"),
    c("fittedModelEquation", "fittedModelEquationDecimals"), 2.2)
  .sareGroupColumn(table, options)
  table$addColumnInfo(name = "model", title = gettext("Model"), type = "string")
  table$addColumnInfo(name = "equation", title = gettext("Equation"), type = "string")
  table$setData(.saSafeRbind(lapply(fits, .sareEquationRows, options = options)))
  table$addFootnote(gettext("M(t) is expected cumulative events, m(t) is instantaneous event intensity, and t / M(t) is cumulative mean time between events."))
  return()
}

.sareCoefficientRows <- function(fit, level) {

  estimates <- fit[["estimates"]]
  se <- if (fit[["inferenceAvailable"]]) sqrt(diag(fit[["covariance"]])) else rep(NA_real_, length(estimates))
  halfWidth <- .sareCritical(level, fit[["df"]]) * se
  lower <- estimates - halfWidth
  upper <- estimates + halfWidth
  positive <- fit[["positive"]]
  lower[positive] <- exp(log(estimates[positive]) - halfWidth[positive] / estimates[positive])
  upper[positive] <- exp(log(estimates[positive]) + halfWidth[positive] / estimates[positive])
  return(data.frame(subgroup = c(fit[["label"]], rep(NA_character_, length(estimates) - 1L)), parameter = names(estimates), estimate = as.numeric(estimates),
    se = as.numeric(se), lower = as.numeric(lower), upper = as.numeric(upper)))
}

.sareCovarianceTables <- function(jaspResults, options, fits) {

  if (!is.null(jaspResults[["covarianceMatrices"]]))
    return()
  title <- gettext("Coefficients Covariance Matrix")
  output <- if (length(fits) <= 1L) createJaspTable(title = title) else createJaspContainer(title = title)
  output$dependOn(c(.sareFitDependencies, "coefficientsCovarianceMatrix"))
  output$position <- 2.1
  jaspResults[["covarianceMatrices"]] <- output
  if (!length(fits)) {
    output$showSpecifiedColumnsOnly <- TRUE
    output$addColumnInfo(name = "parameter", title = "", type = "string")
    return()
  }
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    table <- if (length(fits) == 1L) output else createJaspTable(title = fit[["label"]])
    if (length(fits) > 1L) {
      table$position <- i
      output[[paste0("group", i)]] <- table
    }
    table$showSpecifiedColumnsOnly <- TRUE
    table$addColumnInfo(name = "parameter", title = "", type = "string")
    parameters <- names(fit[["estimates"]])
    for (j in seq_along(parameters))
      table$addColumnInfo(name = paste0("p", j), title = parameters[j], type = "number")
    if (!fit[["inferenceAvailable"]]) {
      table$setError(gettext("A reliable covariance matrix is unavailable."))
      next
    }
    values <- as.data.frame(fit[["covariance"]])
    names(values) <- paste0("p", seq_along(parameters))
    table$setData(cbind(data.frame(parameter = parameters), values))
    if (length(fits) == 1L && .sapHasSubgroups(options))
      table$addFootnote(gettextf("Subgroup: %1$s", fit[["label"]]))
    table$addFootnote(gettext("The covariance matrix uses the same reported parameter scale as the coefficient estimates."))
  }
  return()
}

.sareEquation <- function(fit, options) {

  number <- function(x) vapply(x, .sapEquationNumber, character(1), decimals = options[["fittedModelEquationDecimals"]])
  estimates <- number(fit[["estimates"]])
  switch(fit[["model"]],
    duane = paste0("t / M(t) = ", estimates[1], " * t^(", estimates[2], ")"),
    crowAmsaa = paste0("M(t) = ", estimates[1], " * t^(", estimates[2], ")"),
    logLinear = paste0("m(t) = exp(", estimates[1], " + (", estimates[2], ") * t)"),
    piecewise = {
      slopes <- fit[["native"]][["betas"]][["log_times"]][, "Est."]
      scales <- fit[["native"]][["lambdas"]][, "Est."]
      breaks <- .sareBreakpoints(fit)
      vapply(seq_along(slopes), function(i) gettextf("Segment %1$i: M(t) = %2$s * t^(%3$s); %4$s < t <= %5$s",
        i, number(scales[i]), number(slopes[i]), if (i == 1L) "0" else number(breaks[i - 1L]),
        if (i > length(breaks)) "Inf" else number(breaks[i])), character(1))
    })
}

.sareEquationRows <- function(fit, options) {

  equations <- .sareEquation(fit, options)
  return(data.frame(subgroup = c(fit[["label"]], rep(NA_character_, length(equations) - 1L)),
    model = c(.sareModelLabel(fit[["model"]]), rep(NA_character_, length(equations) - 1L)), equation = equations))
}
