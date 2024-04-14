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

SemiParametricSurvivalAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  # semi-parametric allows only for right censored data -- set the options for generic functions downstream
  options[["censoringType"]] <- "right"

  if (.saSurvivalReady(options))
    dataset <- .saReadDataset(dataset, options)

  if (.saSurvivalReady(options)) {
    .saspFitCox(jaspResults, dataset, options)
  }

  .saspSummaryTable(jaspResults, dataset, options)
  .saspEstimatesTable(jaspResults, dataset, options)
  .saspHazardRatioTable(jaspResults, dataset, options)


  return()
}

.saspDependencies <- c("timeToEvent", "eventStatus", "eventIndicator", "factors", "covariates", "modelTerms")

.saspFitCox           <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["fit"]])) {

    fitContainer <- createJaspState()
    fitContainer$dependOn(.saspDependencies)
    jaspResults[["fit"]] <- fitContainer

    fit <- try(survival::coxph(
      formula = .saGetFormula(options, type = "Cox", null = FALSE),
      data    = dataset
    ))
    # fix scoping in ggsurvplot
    fit$call$formula <- eval(fit$call$formula)

    jaspResults[["fit"]]$object <- fit
  }

  if (is.null(jaspResults[["fitNull"]])) {

    fitNullContainer <- createJaspState()
    fitNullContainer$dependOn(.saspDependencies)
    jaspResults[["fitNull"]] <- fitNullContainer

    fitNull <- try(survival::coxph(
      formula = .saGetFormula(options, type = "Cox", null = TRUE),
      data    = dataset
    ))
    # fix scoping in ggsurvplot
    fitNull$call$formula <- eval(fitNull$call$formula)

    jaspResults[["fitNull"]]$object <- fitNull
  }

  return()
}
.saspSummaryTable     <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Cox Proportional Hazards Summary Table"))
  summaryTable$dependOn(.saspDependencies)
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  summaryTable$addColumnInfo(name = "mod",    title = gettext("Model"),    type = "string")
  summaryTable$addColumnInfo(name = "loglik", title = gettext("log Lik."), type = "number")
  summaryTable$addColumnInfo(name = "df",     title = gettext("df"),       type = "integer")
  summaryTable$addColumnInfo(name = "pvl",    title = gettext("p"),        type = "pvalue")
  summaryTable$addColumnInfo(name = "aic",    title = gettext("AIC"),      type = "number", format="dp:3")
  summaryTable$addColumnInfo(name = "bic",    title = gettext("BIC"),      type = "number", format="dp:3")

  if (!.saSurvivalReady(options))
    return()

  fit     <- jaspResults[["fit"]][["object"]]
  fitNull <- jaspResults[["fitNull"]][["object"]]

  if (jaspBase::isTryError(fitNull))
    summaryTable$addFootnote(fitNull, symbol = gettextf("The null model failed with the following message:"))
  else
    summaryTable$addRows(list(
      "mod"    = "H\u2080",
      "loglik" = as.numeric(logLik(fitNull)),
      "df"     = attr(logLik(fitNull), "df"),
      "pvl"    = pchisq(2 * (fitNull$loglik[2] - fitNull$loglik[1]), df = attr(logLik(fitNull), "df"), lower.tail = FALSE),
      "aic"    = AIC(fitNull),
      "bic"    = BIC(fitNull)
    ))

  if (jaspBase::isTryError(fit))
    summaryTable$setError(fit, symbol = gettextf("The model failed with the following message:"))
  else
    summaryTable$addRows(list(
      "mod"    = "H\u2081",
      "loglik" = as.numeric(logLik(fit)),
      "df"     = attr(logLik(fit), "df"),
      "pvl"    = pchisq(2 * (fit$loglik[2] - fit$loglik[1]), df = attr(logLik(fit), "df"), lower.tail = FALSE),
      "aic"    = AIC(fit),
      "bic"    = BIC(fit)
    ))

  if (!is.null(attr(dataset, "na.action")))
    summaryTable$addFootnote(gettextf("%1$i observations ommited due to missing values.", length(attr(dataset, "na.action"))))

  nullPredictors <- .saGetPredictors(options, null = TRUE)
  if (length(nullPredictors) != 0)
    summaryTable$addFootnote(gettextf("Null model contains nuisance parameters: %s", paste(nullPredictors, collapse = ", ")))

  return()
}
.saspEstimatesTable   <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["estimatesTable"]]))
    return()

  estimatesTable <- createJaspTable(title = gettext("Cox Proportional Hazards Estimates Table"))
  estimatesTable$dependOn(c(.saspDependencies, "coefficientEstimate", "vovkSellke"))
  estimatesTable$position <- 2
  jaspResults[["estimatesTable"]] <- estimatesTable

  # create empty table
  estimatesTable$addColumnInfo(name = "model",  title = gettext("Model"),          type = "string")
  estimatesTable$addColumnInfo(name = "param",  title = "",                        type = "string")
  estimatesTable$addColumnInfo(name = "est",    title = gettext("Estimate"),       type = "number")
  estimatesTable$addColumnInfo(name = "se",     title = gettext("Standard Error"), type = "number")
  estimatesTable$addColumnInfo(name = "zval",   title = gettext("z"),              type = "number")
  estimatesTable$addColumnInfo(name = "pval",   title = gettext("p"),              type = "pvalue")

  if (options[["vovkSellke"]]) {
    estimatesTable$addColumnInfo(name = "vsmpr",  title = gettextf("VS-MPR%s", "\u002A"), type = "number")
    estimatesTable$addFootnote(gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value,
  the maximum possible odds in favor of H%1$s over H%2$s equals
  1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37
  (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "\u2264"), symbol = "\u002A")
  }

  if (!.saSurvivalReady(options))
      return()

  fit     <- jaspResults[["fit"]][["object"]]
  fitNull <- jaspResults[["fitNull"]][["object"]]


  if (jaspBase::isTryError(fitNull)) {
    estimatesTable$addFootnote(gettextf("Null model contains nuisance parameters: %s", paste(nullPredictors, collapse = ", ")))
    estimates <- NULL
  } else
    estimates <- .saspCoxFitSummary(fitNull, options, "H\u2080")

  if (jaspBase::isTryError(fit)) {
    estimatesTable$setError(fit, symbol = gettextf("The model failed with the following message:"))
    return()
  } else
    estimates <- rbind(estimates, .saspCoxFitSummary(fit, options, "H\u2081"))


  if (!is.null(estimates) && options[["vovkSellke"]])
    estimates$vsmpr <- VovkSellkeMPR(estimates$pval)


  estimatesTable$setData(estimates)

  return()
}
.saspHazardRatioTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["hazardRatioTable"]]))
    return()

  hazardRatioTable <- createJaspTable(title = gettext("Hazard Ratios Estimates Table"))
  hazardRatioTable$dependOn(c(.saspDependencies, "coefficientHazardRatioEstimates", "coefficientCiLevel"))
  hazardRatioTable$position <- 3
  jaspResults[["hazardRatioTable"]] <- hazardRatioTable

  # create empty table
  overtitle <- gettextf("%.0f%% CI", 100 * options[["coefficientCiLevel"]])
  hazardRatioTable$addColumnInfo(name = "model",  title = gettext("Model"),          type = "string")
  hazardRatioTable$addColumnInfo(name = "param",  title = "",                        type = "string")
  hazardRatioTable$addColumnInfo(name = "est",    title = gettext("Hazard Ratio"),   type = "number")
  hazardRatioTable$addColumnInfo(name = "lower",  title = gettext("Lower"), type = "number", overtitle = overtitle)
  hazardRatioTable$addColumnInfo(name = "upper",  title = gettext("Upper"), type = "number", overtitle = overtitle)

  if (!.saSurvivalReady(options))
    return()

  fit     <- jaspResults[["fit"]][["object"]]
  fitNull <- jaspResults[["fitNull"]][["object"]]


  if (jaspBase::isTryError(fitNull)) {
    hazardRatioTable$addFootnote(gettextf("Null model contains nuisance parameters: %s", paste(nullPredictors, collapse = ", ")))
    estimates <- NULL
  } else
    estimates <- .saspCoxFitSummary(fitNull, options, "H\u2080", HR = TRUE, CI = options[["coefficientCiLevel"]])

  if (jaspBase::isTryError(fit)) {
    hazardRatioTable$setError(fit, symbol = gettextf("The model failed with the following message:"))
    return()
  } else
    estimates <- rbind(estimates, .saspCoxFitSummary(fit, options, "H\u2081", HR = TRUE, CI = options[["coefficientCiLevel"]]))


  if (!is.null(estimates) && options[["vovkSellke"]])
    estimates$vsmpr <- VovkSellkeMPR(estimates$pval)

  hazardRatioTable$setData(estimates)

  return()
}


.saspCoxFitSummary     <- function(fit, options, model, HR = FALSE, CI = 0.95) {

  if (HR)
    estimatesFit <- summary(fit, conf.int = CI)$conf.int[,-2,drop=FALSE]
  else
    estimatesFit <- summary(fit)$coefficients[,c("coef", "se(coef)", "z", "Pr(>|z|)"),drop=FALSE]

  if (is.null(estimatesFit))
    return()
  else if (is.null(dim(estimatesFit)))
    estimatesFit <- data.frame(t(estimatesFit))
  else
    estimatesFit <- data.frame(estimatesFit)

  if (HR)
    colnames(estimatesFit) <- c("est", "lower", "upper")
  else
    colnames(estimatesFit) <- c("est", "se", "zval", "pval")

  estimatesFit <- cbind(
    "model" = "",
    "param" = sapply(rownames(estimatesFit), function(x) .saTermNames(x, c(options[["covariates"]], options[["factors"]]))),
    estimatesFit)

  estimatesFit[1, "model"] <- model

  return(estimatesFit)
}

