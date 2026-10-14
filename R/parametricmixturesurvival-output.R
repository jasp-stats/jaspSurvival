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
  outputDependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
                          "mixtureComponentsTable", "coefficientsConfidenceInterval", "coefficientsConfidenceIntervalLevel")

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = .sapmComponentsTableFun,
    name           = "mixtureComponentsTable",
    title          = gettext("Component Mean and Median"),
    dependencies   = outputDependencies,
    position       = 2.2
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

  outputDependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
                          "mixtureClassificationTable")

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = .sapmClassificationTableFun,
    name           = "mixtureClassificationTable",
    title          = gettext("Mixture Classification"),
    dependencies   = outputDependencies,
    position       = 2.3
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

  outputDependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
                          "mixtureDiagnosticsTable")

  # every mixture model is displayed as a column of a single table
  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = list(fit),
    outputFunction = .sapmDiagnosticsTableFun,
    name           = "mixtureDiagnosticsTable",
    title          = gettext("Estimation Diagnostics"),
    dependencies   = outputDependencies,
    position       = 2.4
  )

  return()
}
.sapmComponentPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mixtureComponentPlot"]]))
    return()

  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)
  if (.saSurvivalReady(options) && length(fit) == 0)
    return()
  fit <- .sapNestFit(fit)

  outputDependencies <- c(.sapGetDependencies(options), .sapSelectedOutputDependencies,
                          "mixtureComponentPlot", "mixtureComponentPlotType", "mixtureComponentPlotObservedData", "mixtureComponentPlotMergePlotsAcrossFactors",
                          "mixtureComponentPlotTransformXAxis",
                          "mixtureComponentPlotHistogramBinWidthType", "mixtureComponentPlotHistogramManualNumberOfBins",
                          .sapPredictionCiDependencies,
                          "predictionsLifeTimeStepsType", "predictionsLifeTimeStepsNumber", "predictionsLifeTimeStepsFrom", "predictionsLifeTimeStepsSize",
                          "predictionsLifeTimeStepsTo", "predictionsLifeTimeCustom",
                          "colorPalette", "plotLegend", "plotTheme"
  )

  .sapSectionWrapper(
    jaspResults    = jaspResults,
    options        = options,
    fit            = fit,
    outputFunction = .sapmComponentPlotFun,
    name           = "mixtureComponentPlot",
    title          = gettext("Mixture Components"),
    dependencies   = outputDependencies,
    position       = 5.6
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
  .sapAddModelColumns(componentsTable, options, output = "coefficientsCovarianceMatrix")
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

  # Mixing probabilities and native parameters are reported in the coefficients table.
  data <- .sapmComponentsTableData(fit, options[["coefficientsConfidenceIntervalLevel"]], kinds = c("mean", "median"))

  data <- .sapAddRowModelInformation(data, fit)

  # add footnotes
  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    componentsTable$addFootnote(attr(fit, "label"))
  if (anyNA(data[["est"]]))
    componentsTable$addFootnote(gettext("Some component means or medians are infinite or could not be evaluated numerically and are shown as missing."))
  if (!.sapConstraintActive(fit) && (anyNA(data[["se"]]) || (options[["coefficientsConfidenceInterval"]] && anyNA(data[c("lower", "upper")]))))
    componentsTable$addFootnote(gettext("Some standard errors or confidence intervals could not be evaluated and are shown as missing."))
  componentsTable$addFootnote(gettext("Standard errors and confidence intervals are based on the delta method."))
  if (length(fit[["covpars"]]) > 0)
    componentsTable$addFootnote(gettext("The component means and medians correspond to the reference level of factors and zero value of covariates."))

  componentsTable$setData(data)
  componentsTable$showSpecifiedColumnsOnly <- TRUE

  return(componentsTable)
}
.sapmComponentsTableData        <- function(fit, level, kinds) {

  mixture    <- attr(fit, "mixture")
  family     <- .sapmFamily(mixture[["family"]])
  components <- mixture[["components"]]
  estimates  <- fit[["res.t"]][, "est"]

  # Evaluate only the requested baseline quantities, in their displayed order.
  quantities <- function(estimates) {
    parameters <- .sapmBaseParameters(estimates, family, components)
    unlist(lapply(seq_len(components), function(k) {
      vapply(kinds, function(kind) switch(kind,
        "probability" = parameters[["probabilities"]][k],
        "mean" = {
          mean <- try(do.call(family[["mean"]], as.list(parameters[["base"]][[k]])), silent = TRUE)
          if (jaspBase::isTryError(mean)) NA_real_ else mean
        },
        "median" = .sapmComponentMedian(family, parameters[["base"]][[k]])
      ), numeric(1))
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
  links        <- rep(ifelse(kinds == "probability", "logit", "log"), components)
  z            <- stats::qnorm((1 + level) / 2)
  lower        <- upper <- rep(NA_real_, length(estimate))
  logitLink    <- links == "logit"    & !is.na(standardError)
  logLink      <- links == "log"      & !is.na(standardError) & estimate > 0

  lower[logitLink]    <- stats::plogis(stats::qlogis(estimate[logitLink]) - z * standardError[logitLink] / (estimate[logitLink] * (1 - estimate[logitLink])))
  upper[logitLink]    <- stats::plogis(stats::qlogis(estimate[logitLink]) + z * standardError[logitLink] / (estimate[logitLink] * (1 - estimate[logitLink])))
  lower[logLink]      <- exp(log(estimate[logLink]) - z * standardError[logLink] / estimate[logLink])
  upper[logLink]      <- exp(log(estimate[logLink]) + z * standardError[logLink] / estimate[logLink])
  lower[!is.finite(lower)] <- NA
  upper[!is.finite(upper)] <- NA

  labels      <- c(probability = gettext("Mixing probability"), mean = gettext("Mean"), median = gettext("Median"))
  nQuantities <- length(kinds)
  component   <- rep(NA_character_, length(estimate))
  component[seq(1, length(estimate), by = nQuantities)] <- gettextf("Component %1$i", seq_len(components))

  return(data.frame(
    component = component,
    quantity  = rep(unname(labels[kinds]), components),
    est       = estimate,
    se        = standardError,
    lower     = lower,
    upper     = upper
  ))
}
.sapmClassificationTableFun     <- function(fit, options) {

  # create the table
  classificationTable <- createJaspTable()
  .sapAddModelColumns(classificationTable, options, output = "coefficientsCovarianceMatrix")
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

  data <- .sapAddRowModelInformation(data, fit)

  # relative entropy (1 = certain posterior classification)
  entropy <- -sum(weights * rowSums(ifelse(posterior > 0, posterior * log(posterior), 0)))
  entropy <- 1 - entropy / (sum(weights) * log(components))

  # add footnotes
  if (!is.null(attr(fit, "label")) && attr(fit, "label") != "")
    classificationTable$addFootnote(attr(fit, "label"))
  classificationTable$addFootnote(gettextf("Observations are classified to the component with the highest posterior probability. The relative entropy of the classification is %1$.3f (values close to 1 indicate low uncertainty in the assignments).", entropy))

  classificationTable$setData(data)
  classificationTable$showSpecifiedColumnsOnly <- TRUE

  return(classificationTable)
}
.sapmDiagnosticsTableFun        <- function(fit, options) {

  # create the table
  diagnosticsTable <- createJaspTable()
  diagnosticsTable$transpose <- TRUE
  textColumns <- c("model", "converged", "hessian")
  if (.sapHasSubgroups(options))
    diagnosticsTable$addColumnInfo(name = "subgroup",     title = gettext("Subgroup"),     type = "string")
  if (.sapHasSubgroups(options))
    textColumns <- c("distribution", textColumns)
  diagnosticsTable$addColumnInfo(name = "distribution",   title = gettext("Distribution"), type = if (.sapHasSubgroups(options)) "mixed" else "string")
  diagnosticsTable$addColumnInfo(name = "model",          title = gettext("Model"),        type = "mixed")
  diagnosticsTable$addColumnInfo(name = "components",     title = gettext("Components"),   type = "integer")
  diagnosticsTable$addColumnInfo(name = "starts",         title = gettext("Starts"),       type = "integer")
  diagnosticsTable$addColumnInfo(name = "replication",    title = gettext("Replications"), type = "integer")
  diagnosticsTable$addColumnInfo(name = "logLik",         title = gettext("Log Lik."),     type = "number")
  diagnosticsTable$addColumnInfo(name = "nextBest",       title = gettext("Next Best Log Lik."), type = "number")
  diagnosticsTable$addColumnInfo(name = "degenerate",     title = gettext("Degenerate Candidates"), type = "integer")
  diagnosticsTable$addColumnInfo(name = "minEss",         title = gettext("Min. Component n (ESS)"), type = "number")
  diagnosticsTable$addColumnInfo(name = "minEvents",      title = gettext("Min. Component Events"),  type = "number")
  diagnosticsTable$addColumnInfo(name = "converged",       title = gettext("Optimizer Converged"), type = "mixed")
  diagnosticsTable$addColumnInfo(name = "hessian",        title = gettext("Hessian Positive Definite"), type = "mixed")

  if (!.saSurvivalReady(options) || is.null(fit))
    return(diagnosticsTable)

  data <- .saSafeRbind(lapply(fit, .sapmRowDiagnosticsTable))
  for (column in intersect(textColumns, names(data)))
    data[[column]] <- jaspBase::createMixedColumn(data[[column]], rep("string", nrow(data)))

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
.sapmFitMessages                <- function(fit) {

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
    message <- paste(.sapmFitMessages(model), collapse = " ")
    if (message != "")
      messages[["warnings"]] <- c(messages[["warnings"]], paste0(.sapmCellLabel(model, options), ": ", message))
  }

  if (length(mixtures) == 0)
    return(messages)

  # the messages of each model are reported in a single footnote, models of the same distribution with the same messages are reported together
  fitMessages <- vapply(mixtures, function(x) paste(.sapmFitMessages(x), collapse = " "), character(1))
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
.sapmCellLabel                  <- function(fit, options, components = attr(fit, "components")) {
  return(gettextf(
    "%1$s model %2$s with %3$s%4$s",
    attr(fit, "distribution"),
    attr(fit, "modelTitle"),
    if (length(components) == 1) .sapComponentsLabel(components) else gettextf("%1$s components", .sapmListLabel(components)),
    if (.sapHasSubgroups(options)) paste0(" (", attr(fit, "subgroupLabel"), ")") else ""
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
    for (par in family[["pars"]]) {
      coeffTable[["coefficient"]][names == paste0(par, k)] <- par
      coeffTable[["mixtureComponent"]][names == paste0(par, k)] <- k
    }

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

    replacement[["coefficient"]]             <- gettext("mixing probability")
    replacement[["est"]]                     <- probabilities[["est"]]
    replacement[["se"]]                      <- probabilities[["se"]]
    replacement[["lower"]]                   <- probabilities[["lower"]]
    replacement[["upper"]]                   <- probabilities[["upper"]]
    replacement[["isRegressionCoefficient"]] <- FALSE
    replacement[["mixtureComponent"]]        <- seq_len(components)

    coeffTable           <- rbind(
      coeffTable[seq_len(min(weightIndex) - 1), , drop = FALSE],
      replacement,
      coeffTable[-seq_len(max(weightIndex)), , drop = FALSE]
    )
    rownames(coeffTable) <- NULL
  }

  coeffTable <- coeffTable[order(coeffTable[["mixtureComponent"]], coeffTable[["coefficient"]] != gettext("mixing probability")), , drop = FALSE]
  rownames(coeffTable) <- NULL

  return(coeffTable)
}
.sapmMixingProbabilities        <- function(fit, level) {

  # the mixing probabilities, their standard errors, and their confidence intervals of all components
  data <- .sapmComponentsTableData(fit, level, kinds = "probability")

  return(data[c("est", "se", "lower", "upper")])
}
