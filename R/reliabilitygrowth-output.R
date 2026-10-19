.sargCreateTable <- function(jaspResults, key, title, dependencies, position) {
  table <- createJaspTable(title = title)
  table$dependOn(c(.sargFitDependencies, dependencies))
  table$position <- position
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[[key]] <- table
  return(table)
}

.sargColumn <- function(table, name, title, type = "number") {
  table$addColumnInfo(name = name, title = title, type = type)
}

.sargGroupColumn <- function(table, options) {
  if (length(unlist(options[["subgroup"]])) > 0 && any(unlist(options[["subgroup"]]) != ""))
    .sargColumn(table, "subgroup", gettext("Subgroup"), "string")
}

.sargSetRows <- function(table, rows) {
  if (length(rows) > 0)
    table$setData(do.call(rbind, rows))
  return()
}

.sargMessages <- function(jaspResults, options) {

  if (!is.null(jaspResults[["fitMessages"]]))
    return()
  fits <- .sargFits(jaspResults)
  rows <- lapply(fits, function(fit) {
    messages <- c(fit[["error"]], fit[["warnings"]])
    if (length(messages) > 0)
      data.frame(subgroup = fit[["label"]], message = messages)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0)
    return()
  table <- .sargCreateTable(jaspResults, "fitMessages", gettext("Model Fitting"), character(0), 0)
  .sargGroupColumn(table, options)
  .sargColumn(table, "message", gettext("Message"), "string")
  .sargSetRows(table, rows)
  return()
}

.sargStatistics <- function(jaspResults, options) {

  fits <- .sargFits(jaspResults, valid = TRUE)
  if (options[["dataSummary"]] && is.null(jaspResults[["dataSummaryTable"]])) {
    table <- .sargCreateTable(jaspResults, "dataSummaryTable", gettext("Data Summary"), "dataSummary", 0.5)
    .sargGroupColumn(table, options)
    .sargColumn(table, "intervals", gettext("Intervals"), "integer")
    .sargColumn(table, "failures", gettext("Failures"), "integer")
    .sargColumn(table, "time", gettext("Follow-up Time"))
    .sargColumn(table, "mtbf", gettext("Cumulative MTBF"))
    .sargSetRows(table, lapply(fits, function(fit) data.frame(subgroup = fit[["label"]], intervals = length(fit[["time"]]),
      failures = sum(fit[["counts"]]), time = max(fit[["time"]]), mtbf = max(fit[["time"]]) / sum(fit[["counts"]]))))
  }
  if (options[["modelSummary"]] && is.null(jaspResults[["summaryTable"]])) {
    table <- .sargCreateTable(jaspResults, "summaryTable", gettext("Model Summary"), "modelSummary", 1)
    .sargGroupColumn(table, options)
    .sargColumn(table, "model", gettext("Model"), "string")
    .sargColumn(table, "method", gettext("Estimation"), "string")
    .sargColumn(table, "logLik", gettext("Log Lik."))
    .sargColumn(table, "df", "df", "integer")
    table$addColumnInfo(name = "aic", title = "AIC", type = "number", format = "dp:3")
    table$addColumnInfo(name = "bic", title = "BIC", type = "number", format = "dp:3")
    .sargSetRows(table, lapply(fits, function(fit) {
      native <- fit[["native"]]
      # Native grouped Poisson log likelihoods omit the count-factorial constant.
      constant <- if (fit[["method"]] == "MLE") sum(lgamma(fit[["counts"]] + 1)) else 0
      data.frame(subgroup = fit[["label"]], model = .sargModelLabel(fit[["model"]]), method = fit[["method"]],
        logLik = native[["logLik"]] - constant, aic = native[["AIC"]] + 2 * constant, bic = native[["BIC"]] + 2 * constant,
        df = if (fit[["method"]] == "MLE") 2 else attr(stats::logLik(native[["model"]]), "df"))
    }))
    table$addCitation("ReliaGrowR (0.7): Reliability Growth Analysis and Repairable Systems Modeling. https://doi.org/10.32614/CRAN.package.ReliaGrowR")
    table$addFootnote(if (.sargMethod(options) == "MLE")
      gettext("Maximum likelihood uses interval failure counts. Log-likelihood includes count-factorial terms; BIC uses the number of intervals. Optimizer convergence status is not supplied by ReliaGrowR.") else
      gettext("Fit statistics refer to the log-scale regression likelihood and cannot be compared directly with NHPP maximum-likelihood statistics. The parameter count includes residual variance."))
  }
  if (options[["coefficients"]] && is.null(jaspResults[["coefficientsTable"]])) {
    table <- .sargCreateTable(jaspResults, "coefficientsTable", gettext("Coefficients Summary"),
      c("coefficients", "coefficientsConfidenceIntervals", "coefficientsConfidenceIntervalsLevel"), 2)
    .sargGroupColumn(table, options)
    .sargColumn(table, "parameter", "", "string")
    .sargColumn(table, "estimate", gettext("Estimate"))
    .sargColumn(table, "se", gettext("Standard Error"))
    if (options[["coefficientsConfidenceIntervals"]])
      .sapAddBoundColumns(table, options[["coefficientsConfidenceIntervalsLevel"]], "twoSided")
    .sargSetRows(table, lapply(fits, .sargCoefficientRows, level = options[["coefficientsConfidenceIntervalsLevel"]]))
    if (options[["model"]] == "piecewise")
      table$addFootnote(gettext("Coefficients describe the segmented regression of log cumulative failures on log time. Change-point coefficients are reported as log times."))
    if (.sargMethod(options) == "MLE")
      table$addFootnote(gettext("Maximum-likelihood intervals use a normal approximation; positive parameters use the log scale."))
  }
  if (options[["coefficientsCovarianceMatrix"]])
    .sargCovarianceTables(jaspResults, options, fits)
  if (options[["fittedModelEquation"]] && is.null(jaspResults[["modelEquations"]])) {
    table <- .sargCreateTable(jaspResults, "modelEquations", gettext("Fitted Model Equations"),
      c("fittedModelEquation", "fittedModelEquationDecimals"), 2.2)
    .sargGroupColumn(table, options)
    .sargColumn(table, "model", gettext("Model"), "string")
    .sargColumn(table, "equation", gettext("Equation"), "string")
    .sargSetRows(table, lapply(fits, .sargEquationRows, options = options))
    table$addFootnote(gettext("M(t) is expected cumulative failures, m(t) is instantaneous failure intensity, and cumulative MTBF is t / M(t)."))
  }
  return()
}

.sargCoefficientRows <- function(fit, level) {
  estimates <- fit[["estimates"]]
  se <- if (fit[["inferenceAvailable"]]) sqrt(diag(fit[["covariance"]])) else rep(NA_real_, length(estimates))
  halfWidth <- .sargCritical(level, fit[["df"]]) * se
  lower <- estimates - halfWidth
  upper <- estimates + halfWidth
  positive <- fit[["positive"]]
  lower[positive] <- exp(log(estimates[positive]) - halfWidth[positive] / estimates[positive])
  upper[positive] <- exp(log(estimates[positive]) + halfWidth[positive] / estimates[positive])
  return(data.frame(subgroup = c(fit[["label"]], rep(NA_character_, length(estimates) - 1L)), parameter = names(estimates), estimate = as.numeric(estimates),
    se = as.numeric(se), lower = as.numeric(lower), upper = as.numeric(upper)))
}

.sargCovarianceTables <- function(jaspResults, options, fits) {
  if (!is.null(jaspResults[["covarianceMatrices"]]))
    return()
  title <- gettext("Coefficients Covariance Matrix")
  output <- if (length(fits) <= 1L) createJaspTable(title = title) else createJaspContainer(title = title)
  output$dependOn(c(.sargFitDependencies, "coefficientsCovarianceMatrix"))
  output$position <- 2.1
  jaspResults[["covarianceMatrices"]] <- output
  if (!length(fits)) {
    output$showSpecifiedColumnsOnly <- TRUE
    .sargColumn(output, "parameter", "", "string")
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
    .sargColumn(table, "parameter", "", "string")
    parameters <- names(fit[["estimates"]])
    for (j in seq_along(parameters)) .sargColumn(table, paste0("p", j), parameters[j])
    if (!fit[["inferenceAvailable"]]) {
      table$setError(gettext("A reliable covariance matrix is unavailable."))
      next
    }
    values <- as.data.frame(fit[["covariance"]])
    names(values) <- paste0("p", seq_along(parameters))
    table$setData(cbind(data.frame(parameter = parameters), values))
    if (length(fits) == 1L && any(nzchar(unlist(options[["subgroup"]]))))
      table$addFootnote(gettextf("Subgroup: %1$s", fit[["label"]]))
    table$addFootnote(gettext("The covariance matrix uses the same reported parameter scale as the coefficient estimates."))
  }
  return()
}

.sargEquation <- function(fit, options) {
  number <- function(x) vapply(x, .sapEquationNumber, character(1), decimals = options[["fittedModelEquationDecimals"]])
  estimates <- number(fit[["estimates"]])
  switch(fit[["model"]],
    duane = paste0("MTBF(t) = ", estimates[1], " * t^(", estimates[2], ")"),
    crowAmsaa = paste0("M(t) = ", estimates[1], " * t^(", estimates[2], ")"),
    logLinear = paste0("m(t) = exp(", estimates[1], " + (", estimates[2], ") * t)"),
    piecewise = {
      slopes <- fit[["native"]][["betas"]][["log_times"]][, "Est."]
      scales <- fit[["native"]][["lambdas"]][, "Est."]
      breaks <- .sargBreakpoints(fit)
      vapply(seq_along(slopes), function(i) gettextf("Segment %1$i: M(t) = %2$s * t^(%3$s); %4$s < t <= %5$s",
        i, number(scales[i]), number(slopes[i]), if (i == 1L) "0" else number(breaks[i - 1L]),
        if (i > length(breaks)) "Inf" else number(breaks[i])), character(1))
    })
}

.sargEquationRows <- function(fit, options) {
  equations <- .sargEquation(fit, options)
  return(data.frame(subgroup = c(fit[["label"]], rep(NA_character_, length(equations) - 1L)),
    model = c(.sargModelLabel(fit[["model"]]), rep(NA_character_, length(equations) - 1L)), equation = equations))
}
