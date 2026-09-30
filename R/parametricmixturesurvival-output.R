#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# mixture output
.sapmComponentsTable            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureComponentsTable"]]))
    return()

  # the extract function automatically groups models by subgroup / distribution / components
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  # output dependencies
  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureComponentsTable", "coefficientsConfidenceInterval", "coefficientsConfidenceIntervalLevel")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapmComponentsTableFun,
    name          = "mixtureComponentsTable",
    title         = gettext("Component Mean and Median"),
    dependencies  = outputDependencies,
    position      = 2.2
  )

  return()
}
.sapmClassificationTable        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureClassificationTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureClassificationTable")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapmClassificationTableFun,
    name          = "mixtureClassificationTable",
    title         = gettext("Mixture Classification"),
    dependencies  = outputDependencies,
    position      = 2.3
  )

  return()
}
.sapmDiagnosticsTable           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureDiagnosticsTable"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureDiagnosticsTable")

  # every mixture model is a row of a single table
  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = list(fit),
    tableFunction = .sapmDiagnosticsTableFun,
    name          = "mixtureDiagnosticsTable",
    title         = gettext("Estimation Diagnostics"),
    dependencies  = outputDependencies,
    position      = 2.4
  )

  return()
}
.sapmComponentPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureComponentPlot"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapmFilterMixtures(.sapFlattenFit(fit, options), options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()
  fit <- .sapNestFit(fit)

  outputDependencies <- c(.sapGetDependencies(options), "compareModelsAcrossDistributions", "interpretModel", "alwaysDisplayModelInformation",
                          "mixtureComponentPlot", "mixtureComponentPlotType", "mixtureComponentPlotKaplanMeier",
                          "predictionsConfidenceInterval", "predictionsConfidenceIntervalLevel",
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeCustom",
                          "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapmComponentPlotFun,
    name          = "mixtureComponentPlot",
    title         = gettext("Mixture Components"),
    dependencies  = outputDependencies,
    position      = 3.5
  )

  return()
}
.sapmFilterMixtures             <- function(fit, options) {

  # mixture specific output is shown only for models with multiple components
  if (!.saSurvivalReady(options))
    return(fit)

  return(Filter(function(x) attr(x, "components") > 1, fit))
}

.sapmComponentsTableFun         <- function(fit, options) {

  # create the table
  componentsTable <- createJaspTable()
  .sapAddColumnSubgroup(     componentsTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( componentsTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   componentsTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        componentsTable, options, output = "coefficientsCovarianceMatrix")
  componentsTable$addColumnInfo(name = "component", title = gettext("Component"),      type = "string")
  componentsTable$addColumnInfo(name = "quantity",  title = "",                        type = "string")
  componentsTable$addColumnInfo(name = "est",       title = gettext("Estimate"),       type = "number")
  componentsTable$addColumnInfo(name = "se",        title = gettext("Standard Error"), type = "number")
  if (options[["coefficientsConfidenceInterval"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["coefficientsConfidenceIntervalLevel"]])
    componentsTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    componentsTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(componentsTable)

  # the mixing probabilities and the component parameters are reported in the coefficients table, the last
  # two quantities of each component are its mean and its median
  mixture    <- attr(fit, "mixture")
  quantities <- length(.sapmFamily(mixture[["family"]])[["pars"]]) + 3
  data       <- .sapmComponentsTableData(fit, options[["coefficientsConfidenceIntervalLevel"]])
  data       <- data[(seq_len(nrow(data)) - 1) %% quantities >= quantities - 2, , drop = FALSE]

  data[["component"]]    <- NA_character_
  data[["component"]][seq(1, nrow(data), by = 2)] <- gettextf("Component %1$i", seq_len(mixture[["components"]]))

  data$subgroup        <- NA
  data$distribution    <- NA
  data$components      <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$components[1]   <- attr(fit, "components")
  data$model[1]        <- attr(fit, "modelTitle")

  # add footnotes
  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    componentsTable$addFootnote(attr(fit, "label"))
  for (message in .sapConstraintNote(fit))
    componentsTable$addFootnote(message)
  componentsTable$addFootnote(gettext("The mean and the median are those of the fitted component distribution; they are not restricted to the observed follow-up."))
  if (anyNA(data[["est"]]))
    componentsTable$addFootnote(gettext("Some component means or medians are infinite or could not be evaluated numerically and are shown as missing."))
  if (!.sapConstraintActive(fit) && (anyNA(data[["se"]]) || (options[["coefficientsConfidenceInterval"]] && anyNA(data[c("lower", "upper")]))))
    componentsTable$addFootnote(gettext("Some standard errors or confidence intervals could not be evaluated and are shown as missing."))
  componentsTable$addFootnote(gettext("Standard errors and confidence intervals are based on the delta method."))
  if (length(fit[["covpars"]]) > 0)
    componentsTable$addFootnote(gettext("The component means and medians correspond to the reference level of factors and zero value of covariates."))
  for (message in .sapmFitMessages(fit, options))
    componentsTable$addFootnote(message, symbol = gettext("Warning:"))

  componentsTable$setData(data)
  componentsTable$showSpecifiedColumnsOnly <- TRUE

  return(componentsTable)
}
.sapmComponentsTableData        <- function(fit, level) {

  mixture    <- attr(fit, "mixture")
  family     <- .sapmFamily(mixture[["family"]])
  components <- mixture[["components"]]
  estimates  <- fit[["res.t"]][, "est"]

  # quantities of each component: mixing probability, parameters, mean, and median
  quantities <- function(estimates) {
    parameters <- .sapmBaseParameters(estimates, family, components)
    unlist(lapply(seq_len(components), function(k) {
      mean <- try(do.call(family[["mean"]], as.list(parameters[["base"]][[k]])), silent = TRUE)
      c(
        parameters[["probabilities"]][k],
        parameters[["base"]][[k]],
        if (jaspBase::isTryError(mean)) NA else mean,
        .sapmComponentMedian(family, parameters[["base"]][[k]])
      )
    }), use.names = FALSE)
  }

  # delta method with a numerical Jacobian (covariate effects do not affect the baseline quantities)
  estimate      <- quantities(estimates)
  standardError <- rep(NA_real_, length(estimate))
  if (!.sapConstraintActive(fit)) {
    jacobian  <- matrix(0, length(estimate), length(estimates))
    baseIndex <- setdiff(seq_along(estimates), fit[["covpars"]])
    for (i in baseIndex) {
      step          <- 1e-5 * max(abs(estimates[i]), 1)
      upper         <- lower <- estimates
      upper[i]      <- upper[i] + step
      lower[i]      <- lower[i] - step
      jacobian[, i] <- (quantities(upper) - quantities(lower)) / (2 * step)
    }
    standardError <- sqrt(pmax(diag(jacobian %*% fit[["cov"]] %*% t(jacobian)), 0))
  }

  estimate[!is.finite(estimate)]           <- NA
  standardError[!is.finite(standardError)] <- NA

  # confidence intervals are computed on the link scale of each quantity (logit for probabilities, log for positive quantities)
  logParameter <- vapply(family[["transforms"]], function(f) identical(f, log), logical(1))
  links        <- rep(c("logit", ifelse(logParameter, "log", "identity"), "log", "log"), components)
  z            <- stats::qnorm((1 + level) / 2)
  lower        <- upper <- rep(NA_real_, length(estimate))
  logitLink    <- links == "logit"    & !is.na(standardError)
  logLink      <- links == "log"      & !is.na(standardError) & estimate > 0
  identityLink <- links == "identity" & !is.na(standardError)

  lower[logitLink]    <- stats::plogis(stats::qlogis(estimate[logitLink]) - z * standardError[logitLink] / (estimate[logitLink] * (1 - estimate[logitLink])))
  upper[logitLink]    <- stats::plogis(stats::qlogis(estimate[logitLink]) + z * standardError[logitLink] / (estimate[logitLink] * (1 - estimate[logitLink])))
  lower[logLink]      <- exp(log(estimate[logLink]) - z * standardError[logLink] / estimate[logLink])
  upper[logLink]      <- exp(log(estimate[logLink]) + z * standardError[logLink] / estimate[logLink])
  lower[identityLink] <- estimate[identityLink] - z * standardError[identityLink]
  upper[identityLink] <- estimate[identityLink] + z * standardError[identityLink]
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  nQuantities <- length(family[["pars"]]) + 3
  component   <- rep(NA_character_, length(estimate))
  component[seq(1, length(estimate), by = nQuantities)] <- gettextf("Component %1$i", seq_len(components))

  return(data.frame(
    component = component,
    quantity  = rep(c(gettext("Mixing probability"), family[["pars"]], gettext("Mean"), gettext("Median")), components),
    est       = estimate,
    se        = standardError,
    lower     = lower,
    upper     = upper
  ))
}
.sapmClassificationTableFun     <- function(fit, options) {

  # create the table
  classificationTable <- createJaspTable()
  .sapAddColumnSubgroup(     classificationTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnDistribution( classificationTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnComponents(   classificationTable, options, output = "coefficientsCovarianceMatrix")
  .sapAddColumnModel(        classificationTable, options, output = "coefficientsCovarianceMatrix")
  classificationTable$addColumnInfo(name = "component",     title = gettext("Component"),                   type = "string")
  classificationTable$addColumnInfo(name = "probability",   title = gettext("Mixing probability"),          type = "number")
  classificationTable$addColumnInfo(name = "count",         title = gettext("Count"),                       type = "integer",  overtitle = gettext("Classified"))
  classificationTable$addColumnInfo(name = "proportion",    title = gettext("Proportion"),                  type = "number",   overtitle = gettext("Classified"))
  classificationTable$addColumnInfo(name = "meanPosterior", title = gettext("Mean posterior probability"),  type = "number",   overtitle = gettext("Classified"))

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(classificationTable)

  mixture    <- attr(fit, "mixture")
  components <- mixture[["components"]]
  posterior  <- mixture[["posterior"]]
  dataset    <- attr(fit, "dataset")
  weights    <- if (options[["weights"]] != "") dataset[[options[["weights"]]]] else rep(1, nrow(posterior))

  # observations are classified to the component with the highest posterior probability
  assigned <- max.col(posterior, ties.method = "first")
  data     <- data.frame(
    component     = gettextf("Component %1$i", seq_len(components)),
    probability   = .sapmComponentEstimates(fit, .sapmFamily(mixture[["family"]]), components)[["probabilities"]],
    count         = vapply(seq_len(components), function(k) sum(weights[assigned == k]), numeric(1)),
    meanPosterior = vapply(seq_len(components), function(k) {
      if (!any(assigned == k)) return(NA_real_)
      return(stats::weighted.mean(posterior[assigned == k, k], weights[assigned == k]))
    }, numeric(1))
  )
  data$proportion <- data$count / sum(weights)

  data$subgroup        <- NA
  data$distribution    <- NA
  data$components      <- NA
  data$model           <- NA
  data$subgroup[1]     <- attr(fit, "subgroup")
  data$distribution[1] <- attr(fit, "distribution")
  data$components[1]   <- attr(fit, "components")
  data$model[1]        <- attr(fit, "modelTitle")

  # relative entropy (1 = certain posterior classification)
  entropy <- -sum(weights * rowSums(ifelse(posterior > 0, posterior * log(posterior), 0)))
  entropy <- 1 - entropy / (sum(weights) * log(components))

  # add footnotes
  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    classificationTable$addFootnote(attr(fit, "label"))
  for (message in .sapConstraintNote(fit))
    classificationTable$addFootnote(message)
  classificationTable$addFootnote(gettextf("Observations are classified to the component with the highest posterior probability. The relative entropy of the classification is %1$.3f (values close to 1 indicate low uncertainty in the assignments).", entropy))
  for (message in .sapmFitMessages(fit, options))
    classificationTable$addFootnote(message, symbol = gettext("Warning:"))

  classificationTable$setData(data)
  classificationTable$showSpecifiedColumnsOnly <- TRUE

  return(classificationTable)
}
.sapmDiagnosticsTableFun        <- function(fit, options) {

  # create the table
  diagnosticsTable <- createJaspTable()
  if (options[["subgroup"]] != "")
    diagnosticsTable$addColumnInfo(name = "subgroup",     title = gettext("Subgroup"),     type = "string")
  diagnosticsTable$addColumnInfo(name = "distribution",   title = gettext("Distribution"), type = "string")
  diagnosticsTable$addColumnInfo(name = "model",          title = gettext("Model"),        type = "string")
  diagnosticsTable$addColumnInfo(name = "components",     title = gettext("Components"),   type = "integer")
  diagnosticsTable$addColumnInfo(name = "starts",         title = gettext("Starts"),       type = "integer")
  diagnosticsTable$addColumnInfo(name = "replication",    title = gettext("Replications"), type = "integer")
  diagnosticsTable$addColumnInfo(name = "logLik",         title = gettext("Log Lik."),     type = "number")
  diagnosticsTable$addColumnInfo(name = "nextBest",       title = gettext("Next Best Log Lik."), type = "number")
  diagnosticsTable$addColumnInfo(name = "degenerate",     title = gettext("Degenerate Candidates"), type = "integer")
  diagnosticsTable$addColumnInfo(name = "minEss",         title = gettext("Min. Component n (ESS)"), type = "number")
  diagnosticsTable$addColumnInfo(name = "minEvents",      title = gettext("Min. Component Events"),  type = "number")
  diagnosticsTable$addColumnInfo(name = "converged",       title = gettext("Optimizer Converged"), type = "string")
  diagnosticsTable$addColumnInfo(name = "hessian",        title = gettext("Hessian Positive Definite"), type = "string")

  if (!.saSurvivalReady(options) || is.null(fit))
    return(diagnosticsTable)

  data <- .saSafeRbind(lapply(fit, .sapmRowDiagnosticsTable))

  # add footnotes
  diagnosticsTable$addFootnote(gettext("Starts is the number of starting values that produced a solution and Replications the number of them that reached the reported solution (within 0.01 log-likelihood units)."))
  if (options[["mixtureConstrainSpread"]])
    diagnosticsTable$addFootnote(gettext("The converged candidate with the highest likelihood satisfying the spread bound is selected. Component-size and separation diagnostics are warnings and do not change this selection."))
  else
    diagnosticsTable$addFootnote(gettext("A candidate solution is degenerate when a component collapses on a few observations (fewer than 3 effective observations, a vanishing interquartile range, or a diverging parameter), or when optimization does not converge. The best non-degenerate candidate is selected. If all candidates are degenerate, the best of them is reported with a warning."))
  diagnosticsTable$addFootnote(gettext("Convergence and Hessian diagnostics refer to the selected candidate fit. Convergence does not rule out a local optimum or unreliable standard errors."))
  for (message in unique(unlist(lapply(fit, .sapConstraintNote))))
    diagnosticsTable$addFootnote(message)
  for (message in .sapCollectFitErrors(fit, options))
    diagnosticsTable$addFootnote(message, symbol = gettext("Error:"))
  for (message in .sapmSummaryMessages(fit, options)[["warnings"]])
    diagnosticsTable$addFootnote(message, symbol = gettext("Warning:"))

  diagnosticsTable$setData(data)
  diagnosticsTable$showSpecifiedColumnsOnly <- TRUE

  return(diagnosticsTable)
}
.sapmRowDiagnosticsTable        <- function(fit) {

  if (jaspBase::isTryError(fit))
    return(.sapRowModelInformation(fit))

  mixture <- attr(fit, "mixture")

  return(data.frame(
    .sapRowModelInformation(fit),
    starts          = mixture[["starts"]],
    replication     = mixture[["replication"]],
    logLik          = fit[["loglik"]],
    nextBest        = mixture[["nextBest"]],
    degenerate      = mixture[["degenerate"]],
    minEss          = mixture[["minEss"]],
    minEvents       = mixture[["minEvents"]],
    converged       = if (mixture[["converged"]]) gettext("yes") else gettext("no"),
    hessian         = if (is.na(mixture[["hessianPositiveDefinite"]])) NA_character_ else if (mixture[["hessianPositiveDefinite"]]) gettext("yes") else gettext("no")
  ))
}
.sapmComponentPlotFun           <- function(fit, options) {

  fit <- fit[[1]]

  estimateTitle <- switch(
    options[["mixtureComponentPlotType"]],
    "survival"           = gettext("Survival Probability"),
    "failureProbability" = gettext("Failure Probability"),
    "density"            = gettext("Density"),
    "hazard"             = gettext("Hazard")
  )

  if (!.saSurvivalReady(options) || jaspBase::isTryError(fit))
    return(createJaspPlot(title = estimateTitle))

  plotData <- try(.sapmComponentPlotData(fit, options))

  if (jaspBase::isTryError(plotData)) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempPlot)
  }

  predictionWarnings <- attr(plotData, "predictionWarnings")
  if (!any(is.finite(plotData[["at"]]) & is.finite(plotData[["estimate"]]))) {
    tempPlot <- createJaspPlot(title = estimateTitle)
    tempPlot$setError(paste(unique(c(gettext("No finite predictions are available for this plot."), predictionWarnings)), collapse = "\n"))
    return(tempPlot)
  }

  hasLevel <- length(unique(plotData[["Level"]])) > 1
  options[["predictionsConfidenceInterval"]] <- options[["predictionsConfidenceInterval"]] && !all(is.na(plotData[["lCi"]]))

  # the mixture is displayed in black and the components follow the color palette
  colors <- c("black", jaspGraphs::JASPcolors(options[["colorPalette"]], asFunction = TRUE)(attr(fit, "components")))
  names(colors) <- levels(plotData[["Component"]])

  plot <- ggplot2::ggplot(data = plotData)

  if (options[["mixtureComponentPlotType"]] %in% c("survival", "failureProbability") && options[["mixtureComponentPlotKaplanMeier"]] && options[["censoringType"]] == "right") {
    kmTable <- .sapKaplanMeierStepData(attr(fit, "dataset"), options, failureProbability = options[["mixtureComponentPlotType"]] == "failureProbability")
    plot    <- plot + jaspGraphs::geom_line(mapping = ggplot2::aes(x = at, y = estimate), data = kmTable, color = "grey60")
  }

  if (options[["predictionsConfidenceInterval"]]) {
    aesCall <- list(
      x        = as.name("at"),
      ymin     = as.name("lCi"),
      ymax     = as.name("uCi"),
      group    = if (hasLevel) as.name("Level")
    )
    geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]), data = plotData[plotData[["Component"]] == levels(plotData[["Component"]])[1], ], fill = "grey60", alpha = 0.30)
    plot <- plot + do.call(ggplot2::geom_ribbon, geomCall)
  }

  aesCall <- list(
    x        = as.name("at"),
    y        = as.name("estimate"),
    color    = as.name("Component"),
    linetype = if (hasLevel) as.name("Level")
  )
  geomCall <- list(mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]))
  plot <- plot + do.call(jaspGraphs::geom_line, geomCall) +
    ggplot2::scale_color_manual(values = colors, name = gettext("Component"))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(plotData[["at"]], na.rm = TRUE))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    plotData[["estimate"]],
    if (options[["predictionsConfidenceInterval"]]) plotData[["lCi"]],
    if (options[["predictionsConfidenceInterval"]]) plotData[["uCi"]]), na.rm = TRUE))

  plot <- plot + jaspGraphs::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks), oob = scales::oob_keep) +
    jaspGraphs::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks), oob = scales::oob_keep) +
    ggplot2::ylab(estimateTitle) + ggplot2::xlab(gettext("Time"))

  # the detailed theme is available only for the survival probability plots
  if (options[["plotTheme"]] == "detailed")
    options[["plotTheme"]] <- "jasp"
  plot <- .sapPredictionPlotAddTheme(plot, options)
  plot <- .sapPredictionPlotAddCaption(plot, predictionWarnings, 550)

  tempPlot <- createJaspPlot(width = 550, height = .sapPredictionPlotCaptionHeight(plot, 320))
  tempPlot$plotObject <- plot

  return(tempPlot)
}
.sapmComponentPlotData          <- function(fit, options) {

  mixture    <- attr(fit, "mixture")
  family     <- .sapmFamily(mixture[["family"]])
  components <- mixture[["components"]]
  type       <- options[["mixtureComponentPlotType"]]
  # the components might change rapidly, the time steps are not rounded for a smooth display
  options[["predictionsLifeTimeRoundSteps"]] <- FALSE
  times      <- .sapOptions2PredictionTime(options, fit, type = "mixtureComponents", plot = TRUE)
  ci         <- options[["predictionsConfidenceInterval"]]
  level      <- options[["predictionsConfidenceIntervalLevel"]]

  # the functions are evaluated with the covariate dependent parameters supplied by flexsurv
  mixtureDensity    <- function(t, start, ...) fit[["dfns"]][["d"]](t, ...)
  componentFunction <- function(k) {
    function(t, start, ...) {
      arguments  <- list(...)
      n          <- max(length(t), lengths(arguments))
      parameters <- lapply(stats::setNames(arguments[paste0(family[["pars"]], k)], family[["pars"]]), rep_len, length.out = n)
      t          <- rep_len(t, n)
      switch(
        type,
        "survival"           = do.call(family[["p"]], c(list(t), parameters, list(lower.tail = FALSE))),
        "failureProbability" = do.call(family[["p"]], c(list(t), parameters)),
        "density"            = .sapmStickBreaking(if (components > 1) do.call(cbind, lapply(arguments[paste0("v", seq_len(components - 1))], rep_len, length.out = n)), n)[, k] * do.call(family[["d"]], c(list(t), parameters)),
        "hazard"             = do.call(family[["h"]], c(list(t), parameters))
      )
    }
  }

  mixtureSummary <- switch(
    type,
    "survival"           = .sapSummaryPredictions(fit, type = "survival", t = times, ci = ci, cl = level),
    "failureProbability" = .sapSummaryPredictions(fit, type = "survival", t = times, ci = ci, cl = level),
    "density"            = .sapSummaryPredictions(fit, fn = mixtureDensity, t = times, ci = ci, cl = level),
    "hazard"             = .sapSummaryPredictions(fit, type = "hazard", t = times, ci = ci, cl = level)
  )
  componentSummaries <- lapply(seq_len(components), function(k) .sapSummaryPredictions(fit, fn = componentFunction(k), t = times, ci = FALSE))
  predictionWarnings <- unique(c(attr(mixtureSummary, "predictionWarnings"), unlist(lapply(componentSummaries, attr, "predictionWarnings"))))

  componentLabels <- c(gettext("Mixture"), gettextf("Component %1$i", seq_len(components)))
  out <- list()
  for (j in seq_along(mixtureSummary)) {

    levelLabel <- if (length(mixtureSummary) > 1) decodeColNames(names(mixtureSummary)[j]) else NA

    mixtureData <- data.frame(
      at        = mixtureSummary[[j]][[1]],
      estimate  = mixtureSummary[[j]][["est"]],
      lCi       = if (ci) mixtureSummary[[j]][["lcl"]] else NA,
      uCi       = if (ci) mixtureSummary[[j]][["ucl"]] else NA,
      Component = componentLabels[1],
      Level     = levelLabel
    )
    if (type == "failureProbability") {
      mixtureData[["estimate"]] <- 1 - mixtureData[["estimate"]]
      mixtureData[c("lCi", "uCi")] <- 1 - mixtureData[c("uCi", "lCi")]
    }

    out[[length(out) + 1]] <- mixtureData
    for (k in seq_len(components)) {
      out[[length(out) + 1]] <- data.frame(
        at        = componentSummaries[[k]][[j]][["time"]],
        estimate  = componentSummaries[[k]][[j]][["est"]],
        lCi       = NA,
        uCi       = NA,
        Component = componentLabels[k + 1],
        Level     = levelLabel
      )
    }
  }

  out <- do.call(rbind, out)
  out[["Component"]] <- factor(out[["Component"]], levels = componentLabels)

  # set any Inf to NA
  out[["estimate"]][is.infinite(out[["estimate"]])] <- NA
  out[["lCi"]][is.infinite(out[["lCi"]])]           <- NA
  out[["uCi"]][is.infinite(out[["uCi"]])]           <- NA
  attr(out, "predictionWarnings") <- predictionWarnings

  return(out)
}

# mixture messages
.sapmFitMessages                <- function(fit, options) {

  mixture  <- attr(fit, "mixture")
  messages <- c(.sapConstraintWarning(fit), .sapNativeFitWarnings(fit))

  if (is.null(mixture))
    return(messages)

  if (mixture[["precisionRejected"]] > 0)
    messages <- c(messages, sprintf(ngettext(
      mixture[["precisionRejected"]],
      "%1$i candidate fit was omitted because its likelihood could not be evaluated reliably at the available numerical precision.",
      "%1$i candidate fits were omitted because their likelihoods could not be evaluated reliably at the available numerical precision."
    ), mixture[["precisionRejected"]]))
  for (message in mixture[["warnings"]])
    messages <- c(messages, gettextf("Estimation warning: %1$s", message))

  # the reported solution is a local optimum whenever no other start reached it
  # (with only degenerate candidates the replication is zero and the degeneracy is reported instead)
  if (!mixture[["allDegenerate"]] && mixture[["replication"]] <= 1 && mixture[["starts"]] >= 2)
    messages <- c(messages, gettextf(
      "The reported solution was reached by only %1$i of %2$i starts; the estimates might be a local optimum. Consider more random starts.",
      mixture[["replication"]], mixture[["starts"]]
    ))

  if (mixture[["allDegenerate"]])
    messages <- c(messages, gettext("All candidate solutions were flagged as degenerate or failed to converge; the reported estimates may be unreliable. Consider fewer components or more starting values."))
  else if (!is.null(attr(fit, "constraints")) && mixture[["selectedDegenerate"]])
    messages <- c(messages, gettext("The selected solution triggered component diagnostics despite satisfying the spread bound. Inspect the component sizes and dispersion before interpreting the mixture."))
  if (!mixture[["allDegenerate"]] && is.finite(mixture[["minEvents"]]) && mixture[["minEvents"]] < 5)
    messages <- c(messages, gettextf("The smallest component is supported by %1$.1f effective events; such a component is weakly identified. Consider fewer components.", mixture[["minEvents"]]))

  if (length(mixture[["collapsed"]]) > 0)
    messages <- c(messages, sprintf(ngettext(
      length(mixture[["collapsed"]]),
      "Component %1$s has a negligible weight or diverging parameters; consider fewer components.",
      "Components %1$s have a negligible weight or diverging parameters; consider fewer components."
    ), paste(mixture[["collapsed"]], collapse = ", ")))

  coinciding <- .sapmCoincidingSets(mixture[["duplicated"]], mixture[["components"]])
  if (length(coinciding) == 1 && length(coinciding[[1]]) == mixture[["components"]])
    messages <- c(messages, gettext("All components coincide; the mixture is not identified and its standard errors are unreliable. Consider fewer components."))
  else for (set in coinciding)
    messages <- c(messages, gettextf(
      "Components %1$s coincide; the mixture is not identified and its standard errors are unreliable. Consider fewer components.",
      .sapmListLabel(set)
    ))

  # the coinciding components already explain an unreliable Hessian
  if (mixture[["hessianWarning"]] && nrow(mixture[["duplicated"]]) == 0)
    messages <- c(messages, gettext("The Hessian or parameter covariance could not be used reliably; standard errors and confidence intervals may be unavailable or unreliable."))

  return(messages)
}
.sapmSummaryMessages            <- function(fit, options) {

  messages       <- list(notes = NULL, warnings = NULL)
  successfulFits <- Filter(function(x) !jaspBase::isTryError(x), fit)
  isMixture      <- vapply(successfulFits, function(x) !is.null(attr(x, "mixture")), logical(1))
  mixtures       <- successfulFits[isMixture]

  messages[["notes"]] <- unique(unlist(lapply(successfulFits, .sapConstraintNote)))
  for (model in successfulFits[!isMixture]) {
    message <- paste(.sapmFitMessages(model, options), collapse = " ")
    if (message != "")
      messages[["warnings"]] <- c(messages[["warnings"]], paste0(.sapmCellLabel(model, options), ": ", message))
  }

  if (length(mixtures) == 0)
    return(messages)

  starts <- vapply(mixtures, function(x) attr(x, "mixture")[["starts"]], numeric(1))
  messages[["notes"]] <- c(messages[["notes"]], gettextf(
    "Mixture models were estimated by direct maximization of the likelihood from %1$s starting values per model (%2$s), each refined by %3$i EM iterations.",
    if (min(starts) == max(starts)) as.character(min(starts)) else gettextf("%1$i to %2$i", min(starts), max(starts)),
    .sapmStartLabels(options),
    options[["mixtureEmIterations"]]
  ))
  if (options[["censoringType"]] == "counting")
    messages[["notes"]] <- c(messages[["notes"]], gettext("Left-truncated data: the EM algorithm provides starting values only; the estimates are from the direct maximization of the likelihood."))

  # the messages of each model are reported in a single footnote, models of the same distribution with the same messages are reported together
  fitMessages <- vapply(mixtures, function(x) paste(.sapmFitMessages(x, options), collapse = " "), character(1))
  # A one-component fit is also nested in every mixture of the same family and model.
  fitMessages <- trimws(paste(fitMessages, .sapmLocalOptimumMessages(successfulFits)[isMixture]))
  cells       <- vapply(seq_along(mixtures), function(i) paste(attr(mixtures[[i]], "distribution"), attr(mixtures[[i]], "modelTitle"), attr(mixtures[[i]], "subgroupLabel"), fitMessages[i], sep = "\n"), character(1))
  for (cell in unique(cells[fitMessages != ""])) {
    index      <- which(cells == cell)
    components <- vapply(mixtures[index], function(x) attr(x, "components"), numeric(1))
    messages[["warnings"]] <- c(messages[["warnings"]], paste0(.sapmCellLabel(mixtures[[index[1]]], options, components), ": ", fitMessages[index[1]]))
  }

  return(messages)
}
.sapmLocalOptimumMessages       <- function(mixtures) {

  # a mixture with more components contains the mixture with fewer components, a lower log-likelihood
  # therefore shows that the reported estimates are a local optimum of the likelihood
  messages   <- rep("", length(mixtures))
  cells      <- vapply(mixtures, function(x) paste(attr(x, "family"), attr(x, "modelId"), attr(x, "subgroupLabel"), sep = "\n"), character(1))
  components <- vapply(mixtures, function(x) attr(x, "components"), numeric(1))
  logLik     <- vapply(mixtures, function(x) x[["loglik"]], numeric(1))

  for (cell in unique(cells)) {
    index <- which(cells == cell)
    index <- index[order(components[index])]
    # every model with fewer components is nested, the comparison uses the best fitting one of them
    for (i in seq_along(index)[-1]) {
      best <- index[which.max(logLik[index[seq_len(i - 1)]])]
      if (logLik[index[i]] < logLik[best] - 1e-6)
        messages[index[i]] <- gettextf(
          "The log-likelihood is lower than that of the nested model with %1$s; the reported estimates are a local optimum. Consider more random starts.",
          .sapComponentsLabel(components[best])
        )
    }
  }

  return(messages)
}
.sapmStartLabels                <- function(options) {

  labels <- c(
    if (options[["mixtureStartKmeans"]])    gettext("k-means"),
    if (options[["mixtureStartQuantiles"]]) gettext("quantiles"),
    if (options[["mixtureStartSplit"]])     gettext("splits of the solution with one component fewer"),
    if (options[["mixtureStartRandom"]])    gettextf("%1$i random", options[["mixtureStartRandomCount"]])
  )

  return(paste(labels, collapse = ", "))
}
.sapmCellLabel                  <- function(fit, options, components = attr(fit, "components")) {
  return(gettextf(
    "%1$s model %2$s with %3$s%4$s",
    attr(fit, "distribution"),
    attr(fit, "modelTitle"),
    if (length(components) == 1) .sapComponentsLabel(components) else gettextf("%1$s components", .sapmListLabel(components)),
    if (options[["subgroup"]] != "") paste0(" (", attr(fit, "subgroupLabel"), ")") else ""
  ))
}
.sapmListLabel                  <- function(x) {
  if (length(x) == 1)
    return(as.character(x))
  return(gettextf("%1$s and %2$s", paste(x[-length(x)], collapse = ", "), x[length(x)]))
}
.sapmCoincidingSets             <- function(duplicated, components) {

  # coinciding pairs are merged into sets of mutually coinciding components
  set <- seq_len(components)
  for (i in seq_len(nrow(duplicated)))
    set[set == set[duplicated[i, 2]]] <- set[duplicated[i, 1]]

  return(Filter(function(x) length(x) > 1, unname(split(seq_len(components), set))))
}
.sapmCoefficientsNames          <- function(coeffTable, fit) {

  mixture     <- attr(fit, "mixture")
  family      <- .sapmFamily(mixture[["family"]])
  components  <- mixture[["components"]]
  names       <- rownames(fit[["res"]])

  coeffTable[["mixtureComponent"]] <- NA_integer_

  for (k in seq_len(components)) {

    # component parameters
    for (par in family[["pars"]])
      coeffTable[["coefficient"]][names == paste0(par, k)] <- gettextf("%1$s (component %2$i)", par, k)

    # covariate effects on the location parameter of the component (the effects of the first component are not prefixed)
    index <- fit[["covpars"]][fit[["mx"]][[paste0(family[["location"]], k)]]]
    if (length(index) > 0) {
      coeffTable[["mixtureComponent"]][index] <- k
      if (k > 1) {
        prefix <- paste0(family[["location"]], k, "(")
        coeffTable[["coefficient"]][index] <- substr(names[index], nchar(prefix) + 1, nchar(names[index]) - 1)
      }
    }
  }

  # the stick-breaking weights are reported as the mixing probabilities of all components: the weights are
  # conditional on the preceding components and are easily mistaken for the probabilities themselves
  weightIndex <- which(names %in% paste0("v", seq_len(components - 1)))
  if (length(weightIndex) > 0) {

    probabilities <- .sapmMixingProbabilities(fit, fit[["cl"]])
    replacement   <- coeffTable[rep(weightIndex[1], components), , drop = FALSE]

    replacement[["coefficient"]]             <- gettextf("Mixing probability (component %1$i)", seq_len(components))
    replacement[["est"]]                     <- probabilities[["est"]]
    replacement[["se"]]                      <- probabilities[["se"]]
    replacement[["lower"]]                   <- probabilities[["lower"]]
    replacement[["upper"]]                   <- probabilities[["upper"]]
    replacement[["isRegressionCoefficient"]] <- FALSE
    replacement[["mixtureComponent"]]        <- NA_integer_

    coeffTable           <- rbind(
      coeffTable[seq_len(min(weightIndex) - 1), , drop = FALSE],
      replacement,
      coeffTable[-seq_len(max(weightIndex)), , drop = FALSE]
    )
    rownames(coeffTable) <- NULL
  }

  return(coeffTable)
}
.sapmMixingProbabilities        <- function(fit, level) {

  # the mixing probabilities, their standard errors, and their confidence intervals of all components
  mixture    <- attr(fit, "mixture")
  quantities <- length(.sapmFamily(mixture[["family"]])[["pars"]]) + 3
  data       <- .sapmComponentsTableData(fit, level)

  return(data[seq(1, nrow(data), by = quantities), c("est", "se", "lower", "upper")])
}
