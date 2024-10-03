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

  if (.saSurvivalReady(options))
    dataset <- .saReadDataset(dataset, options)

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  .saspFitCox(jaspResults, dataset, options)
  .saspFitCoxAssumptionTest(jaspResults, dataset, options)

  saveRDS(jaspResults[["fit"]]$object, file = "C:/JASP/fit.RDS")

  .saspSummaryTable(jaspResults, dataset, options)
  .saspTestsTable(jaspResults, dataset, options)

  if (options[["modelFit"]])
    .saspModelFitTable(jaspResults, dataset, options)

  if (options[["coefficientEstimate"]])
    .saspEstimatesTable(jaspResults, dataset, options)

  if (options[["coefficientHazardRatioEstimates"]])
    .saspHazardRatioTable(jaspResults, dataset, options)

  if (options[["plot"]])
    .saSurvivalPlot(jaspResults, dataset, options, type = "Cox")

  if (options[["proportionalHazardsTable"]])
    .saspProportionalHazardsTable(jaspResults, dataset, options)

  if (options[["proportionalHazardsPlot"]])
    .saspProportionalHazardsPlots(jaspResults, dataset, options)

  return()
}

.saspDependencies <- c("timeToEvent", "eventStatus", "eventIndicator", "factors", "covariates", "strata", "id", "cluster", "modelTerms", "method")

.saspFitCox               <- function(jaspResults, dataset, options) {

  if (!.saSurvivalReady(options))
    return()

  if (is.null(jaspResults[["fit"]])) {

    fitContainer <- createJaspState()
    fitContainer$dependOn(.saspDependencies)
    jaspResults[["fit"]] <- fitContainer

    fit <- try(coxph(
      formula = .saGetFormula(options, type = "Cox", null = FALSE),
      data    = dataset,
      method  = options[["method"]],
      id      = if (options[["id"]] != "")      dataset[[options[["id"]]]],
      cluster = if (options[["cluster"]] != "") dataset[[options[["cluster"]]]],
      weights = if (options[["weights"]] != "") options[["weights"]]
    ))
    # fix scoping in ggsurvplot
    fit$call$formula <- eval(fit$call$formula)

    jaspResults[["fit"]]$object <- fit
  }

  if (is.null(jaspResults[["fitNull"]])) {

    fitNullContainer <- createJaspState()
    fitNullContainer$dependOn(.saspDependencies)
    jaspResults[["fitNull"]] <- fitNullContainer
    fitNull <- try(coxph(
      formula = .saGetFormula(options, type = "Cox", null = TRUE),
      data    = dataset,
      method  = options[["method"]],
      id      = if (options[["id"]] != "")      dataset[[options[["id"]]]],
      cluster = if (options[["cluster"]] != "") dataset[[options[["cluster"]]]],
      weights = if (options[["weights"]] != "") options[["weights"]]
    ))
    # fix scoping in ggsurvplot
    fitNull$call$formula <- eval(fitNull$call$formula)

    jaspResults[["fitNull"]]$object <- fitNull
  }

  return()
}
.saspFitCoxAssumptionTest <- function(jaspResults, dataset, options) {

  if (!.saSurvivalReady(options))
    return()

  # fit only if diagnostics table/plot requested
  if (!options[["proportionalHazardsTable"]] && !options[["proportionalHazardsPlot"]])
    return()

  if (!is.null(jaspResults[["fitTest"]]))
    return()

  fitTestContainer <- createJaspState()
  fitTestContainer$dependOn(c(.saspDependencies, "proportionalHazardsTransformation", "proportionalHazardsTestTerms"))
  jaspResults[["fitTest"]] <- fitTestContainer

  if (is.null(jaspResults[["fit"]]))
    return()

  fit <- jaspResults[["fit"]][["object"]]

  if (jaspBase::isTryError(fit))
    return()

  fitTest <- cox.zph(
    fit       = fit,
    transform = options[["proportionalHazardsTransformation"]],
    terms     = options[["proportionalHazardsTestTerms"]]
  )

  jaspResults[["fitTest"]]$object <- fitTest

  return()
}
.saspTestsTable           <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["testsTable"]]))
    return()

  if (!(options[["testsLikelihoodRatio"]] || options[["testsWald"]] || options[["testsScore"]]))
    return()

  testsTable <- createJaspTable(title = gettext("Model Tests Table"))
  testsTable$dependOn(c(.saspDependencies, "testsLikelihoodRatio", "testsWald", "testsScore"))
  testsTable$position <- 2
  jaspResults[["testsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test",     title = gettext("Test"),         type = "string")
  testsTable$addColumnInfo(name = "chiSqr",   title = gettext("Chi Square"),   type = "number")
  testsTable$addColumnInfo(name = "df",       title = gettext("df"),           type = "integer")
  testsTable$addColumnInfo(name = "p",        title = gettext("p"),            type = "pvalue")

  if (length(options[["factors"]]) == 0 && length(options[["covariates"]]) == 0) {
    testsTable$addFootnote(gettext("At least one factor or covariate needs to be specified"))
    return()
  }

  fit        <- jaspResults[["fit"]][["object"]]
  fitSummary <- summary(fit)

  if (options[["testsLikelihoodRatio"]]) {
    testsTable$addRows(list(
      "test"   = gettext("Likelihood ratio"),
      "chiSqr" = fitSummary[["logtest"]][["test"]],
      "df"     = fitSummary[["logtest"]][["df"]],
      "p"      = fitSummary[["logtest"]][["pvalue"]]
    ))
  }

  if (options[["testsWald"]]) {
    testsTable$addRows(list(
      "test"   = gettext("Wald"),
      "chiSqr" = fitSummary[["waldtest"]][["test"]],
      "df"     = fitSummary[["waldtest"]][["df"]],
      "p"      = fitSummary[["waldtest"]][["pvalue"]]
    ))
  }

  if (options[["testsScore"]]) {
    testsTable$addRows(list(
      "test"   = gettext("Score"),
      "chiSqr" = fitSummary[["sctest"]][["test"]],
      "df"     = fitSummary[["sctest"]][["df"]],
      "p"      = fitSummary[["sctest"]][["pvalue"]]
    ))
  }

  testsTable$addFootnote(gettext("Test are based on the H\u2081 model."))

  return()
}
.saspSummaryTable         <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Cox Proportional Hazards Summary Table"))
  summaryTable$dependOn(.saspDependencies)
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  summaryTable$addColumnInfo(name = "mod",    title = gettext("Model"),    type = "string")
  summaryTable$addColumnInfo(name = "loglik", title = gettext("Log Lik."), type = "number")
  summaryTable$addColumnInfo(name = "df",     title = gettext("df"),       type = "integer")
#  summaryTable$addColumnInfo(name = "pvl",    title = gettext("p"),        type = "pvalue")
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
#      "pvl"    = pchisq(2 * (fitNull$loglik[2] - fitNull$loglik[1]), df = attr(logLik(fitNull), "df"), lower.tail = FALSE),
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
#      "pvl"    = pchisq(2 * (fit$loglik[2] - fit$loglik[1]), df = attr(logLik(fit), "df"), lower.tail = FALSE),
      "aic"    = AIC(fit),
      "bic"    = BIC(fit)
    ))

  summaryTable$addFootnote(gettextf("%1$i observations with %2$i events.", fit[["n"]], fit[["nevent"]]))

  if (!is.null(attr(dataset, "na.action")))
    summaryTable$addFootnote(gettextf("%1$i observations ommited due to missing values.", length(attr(dataset, "na.action"))))

  nullPredictors <- .saGetPredictors(options, null = TRUE)
  if (length(nullPredictors) != 0)
    summaryTable$addFootnote(gettextf("Null model contains nuisance parameters: %1$s", paste(nullPredictors, collapse = ", ")))

  if (options[["cluster"]] != "")
    summaryTable$addFootnote(gettextf("Robust variance estimation based on %1$s.", options[["cluster"]]))

  if (options[["id"]] != "")
    summaryTable$addFootnote(gettextf("Subject identification based on %1$s.", options[["id"]]))

  return()
}
.saspModelFitTable        <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["modelFitTable"]]))
    return()

  modelFitTable <- createJaspTable(title = gettext("Model Fit Summary Table"))
  modelFitTable$dependOn(c(.saspDependencies, "modelFit"))
  modelFitTable$position <- 3
  jaspResults[["modelFitTable"]] <- modelFitTable

  # create empty table
  modelFitTable$addColumnInfo(name = "mod",    title = gettext("Model"),    type = "string")
  modelFitTable$addColumnInfo(name = "concordanceEstimate",  title = gettext("Estimate"),       type = "number", overtitle = gettext("Concordance"))
  modelFitTable$addColumnInfo(name = "concordanceSe",        title = gettext("Standard Error"), type = "number", overtitle = gettext("Concordance"))

  if (!.saSurvivalReady(options))
    return()

  fit     <- jaspResults[["fit"]][["object"]]
  fitNull <- jaspResults[["fitNull"]][["object"]]

  if (jaspBase::isTryError(fitNull))
    modelFitTable$addFootnote(fitNull, symbol = gettextf("The null model failed with the following message:"))
  else
    modelFitTable$addRows(list(
      "mod"    = "H\u2080",
      "concordanceEstimate" = fitNull[["concordance"]][["concordance"]],
      "concordanceSe"       = fitNull[["concordance"]][["std"]]
    ))

  if (jaspBase::isTryError(fit))
    modelFitTable$setError(fit, symbol = gettextf("The model failed with the following message:"))
  else
    modelFitTable$addRows(list(
      "mod"    = "H\u2081",
      "concordanceEstimate" = fit[["concordance"]][["concordance"]],
      "concordanceSe"       = fit[["concordance"]][["std"]]
    ))

  return()
}
.saspEstimatesTable       <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["estimatesTable"]]))
    return()

  estimatesTable <- createJaspTable(title = gettext("Cox Proportional Hazards Estimates Table"))
  estimatesTable$dependOn(c(.saspDependencies, "coefficientEstimate", "vovkSellke", "coefficientsConfidenceIntervals", "coefficientsConfidenceIntervalsLevel"))
  estimatesTable$position <- 4
  jaspResults[["estimatesTable"]] <- estimatesTable

  # create empty table
  estimatesTable$addColumnInfo(name = "model",  title = gettext("Model"),          type = "string")
  estimatesTable$addColumnInfo(name = "param",  title = "",                        type = "string")
  estimatesTable$addColumnInfo(name = "est",    title = gettext("Estimate"),       type = "number")
  estimatesTable$addColumnInfo(name = "se",     title = gettext("Standard Error"), type = "number")
  if (options[["cluster"]] != "" || options[["id"]] != "")
    estimatesTable$addColumnInfo(name = "rse",    title = gettext("Robust Standard Error"), type = "number")
  if (options[["coefficientsConfidenceIntervals"]]) {
    overtitle <- gettextf("%.0f%% CI", 100 * options[["coefficientsConfidenceIntervalsLevel"]])
    estimatesTable$addColumnInfo(name = "lower",  title = gettext("Lower"),   type = "number", overtitle = overtitle)
    estimatesTable$addColumnInfo(name = "upper",  title = gettext("Upper"),   type = "number", overtitle = overtitle)
  }
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

  if (length(options[["strata"]]) > 0)
    estimatesTable$addFootnote(gettextf("Results are stratified via: %1$s.", paste0(options[["strata"]], collapse = ", ")))

  estimatesTable$setData(estimates)

  return()
}
.saspHazardRatioTable     <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["hazardRatioTable"]]))
    return()

  hazardRatioTable <- createJaspTable(title = gettext("Hazard Ratios Estimates Table"))
  hazardRatioTable$dependOn(c(.saspDependencies, "coefficientHazardRatioEstimates", "coefficientsConfidenceIntervals", "coefficientsConfidenceIntervalsLevel"))
  hazardRatioTable$position <- 5
  jaspResults[["hazardRatioTable"]] <- hazardRatioTable

  # create empty table
  hazardRatioTable$addColumnInfo(name = "model",  title = gettext("Model"),          type = "string")
  hazardRatioTable$addColumnInfo(name = "param",  title = "",                        type = "string")
  hazardRatioTable$addColumnInfo(name = "est",    title = gettext("Hazard Ratio"),   type = "number")
  if (options[["coefficientsConfidenceIntervals"]]) {
    overtitle <- gettextf("%.0f%% CI", 100 * options[["coefficientsConfidenceIntervalsLevel"]])
    hazardRatioTable$addColumnInfo(name = "lower",  title = gettext("Lower"), type = "number", overtitle = overtitle)
    hazardRatioTable$addColumnInfo(name = "upper",  title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  if (!.saSurvivalReady(options))
    return()

  fit     <- jaspResults[["fit"]][["object"]]
  fitNull <- jaspResults[["fitNull"]][["object"]]


  if (jaspBase::isTryError(fitNull)) {
    hazardRatioTable$addFootnote(gettextf("Null model contains nuisance parameters: %s", paste(nullPredictors, collapse = ", ")))
    estimates <- NULL
  } else
    estimates <- .saspCoxFitSummary(fitNull, options, "H\u2080", HR = TRUE)

  if (jaspBase::isTryError(fit)) {
    hazardRatioTable$setError(fit, symbol = gettextf("The model failed with the following message:"))
    return()
  } else
    estimates <- rbind(estimates, .saspCoxFitSummary(fit, options, "H\u2081", HR = TRUE))


  if (!is.null(estimates) && options[["vovkSellke"]])
    estimates$vsmpr <- VovkSellkeMPR(estimates$pval)

  hazardRatioTable$setData(estimates)

  return()
}
.saspProportionalHazardsTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["proportionalHazardsTable"]]))
    return()

  proportionalHazardsTable <- createJaspTable(title = gettext("Proporational Hazards Assumption Test Table"))
  proportionalHazardsTable$dependOn(c(.saspDependencies, "proportionalHazardsTable", "proportionalHazardsTransformation", "proportionalHazardsTestTerms"))
  proportionalHazardsTable$position <- 7
  jaspResults[["proportionalHazardsTable"]] <- proportionalHazardsTable

  # create empty table
  proportionalHazardsTable$addColumnInfo(name = "param",    title = "",                      type = "string")
  proportionalHazardsTable$addColumnInfo(name = "chisq",    title = gettext("Chi Square"),   type = "number")
  proportionalHazardsTable$addColumnInfo(name = "df",       title = gettext("df"),           type = "integer")
  proportionalHazardsTable$addColumnInfo(name = "p",        title = gettext("p"),            type = "pvalue")

  if (!.saSurvivalReady(options))
    return()

  fitTest <- jaspResults[["fitTest"]][["object"]]

  if (jaspBase::isTryError(fitTest)) {
    proportionalHazardsTable$setError(gettextf("The model test failed with the following message: %1$s", fitTest))
    return()
  }

  fitTestSummary <- cbind.data.frame(param = rownames(fitTest[["table"]]), fitTest[["table"]])
  fitTestSummary$param[fitTestSummary$param == "GLOBAL"] <- gettext("Global")
  fitTestSummary$param <- sapply(fitTestSummary$param, function(x) .saTermNames(x, c(options[["covariates"]], options[["factors"]])))

  proportionalHazardsTable$setData(fitTestSummary)
  proportionalHazardsTable$addFootnote(gettextf("Tests are performed using the %1$s survival times transformation.", options[["proportionalHazardsTransformation"]]))

  return()
}
.saspProportionalHazardsPlots <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["proportionalHazardsPlots"]]))
    return()

  proportionalHazardsPlots <- createJaspContainer(title = gettext("Proporational Hazards Assumption Test Plots"))
  proportionalHazardsPlots$dependOn(c(.saspDependencies, "proportionalHazardsPlot", "proportionalHazardsTransformation", "proportionalHazardsTestTerms"))
  proportionalHazardsPlots$position <- 8
  jaspResults[["proportionalHazardsPlots"]] <- proportionalHazardsPlots

  if (!.saSurvivalReady(options)) {
    surivalPlot <- createJaspPlot()
    proportionalHazardsPlots[["waitingPlot"]] <- surivalPlot
    return()
  }

  fitTest <- jaspResults[["fitTest"]][["object"]]

  if (jaspBase::isTryError(fitTest)) {
    surivalPlot <- createJaspPlot()
    surivalPlot$setError(gettextf("The model test failed with the following message: %1$s", fitTest))
    proportionalHazardsPlots[["waitingPlot"]] <- surivalPlot
    return()
  }

  for(i in 1:(nrow(fitTest$table) - 1)) {

    tempVariable    <- rownames(fitTest$table)[i]
    tempFitTestPlot <- plot(fitTest, plot = FALSE, var = tempVariable)

    # adapted from the survival:::plot.cox.zph
    tempDfPoints <- data.frame(
      x = fitTest$x,
      y = fitTest$y[,i]
    )
    tempDfPrediction <- data.frame(
      x = tempFitTestPlot$x,
      y = tempFitTestPlot$y[,1]
    )
    tempDfCiBand <- data.frame(
        x = c(tempFitTestPlot$x,     rev(tempFitTestPlot$x)),
        y = c(tempFitTestPlot$y[,2], rev(tempFitTestPlot$y[,3]))
    )

    if (fitTest$transform == "log") {
      tempDfPoints$x <- exp(tempDfPoints$x)
    }

    # x-ticks
    xTime <- fitTest$time
    indx  <- !duplicated(tempDfPoints$x)
    aprX  <- approx(tempDfPoints$x[indx], xTime[indx], seq(min(tempDfPoints$x), max(tempDfPoints$x), length = 5))

    # y-ticks
    yTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(tempDfPoints$y, tempDfCiBand$y)))

    tempPlot <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = tempDfCiBand, mapping = ggplot2::aes(x = x, y = y), fill = "grey", alpha = 0.5) +
      jaspGraphs::geom_line(data = tempDfPrediction, mapping = ggplot2::aes(x = x, y = y)) +
      jaspGraphs::geom_point(data = tempDfPoints, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::labs(
        x     = gettext("Time"),
        y     = gettext("Beta(t)")
      )
    tempPlot <- tempPlot +
      jaspGraphs::scale_x_continuous(breaks = aprX$x, labels = signif(aprX$y, 2)) +
      jaspGraphs::scale_y_continuous(limits = range(yTicks), breaks = yTicks)

    tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()

    tempJaspPlot <- createJaspPlot(
      plot   = tempPlot,
      title  = .saTermNames(tempVariable, c(options[["covariates"]], options[["factors"]])),
      width  = 450,
      height = 320
    )
    proportionalHazardsPlots[[paste0("plot", i)]] <- tempJaspPlot
  }

  return()
}


.saspCoxFitSummary     <- function(fit, options, model, HR = FALSE) {

  # extract coefficients
  estimatesFit <- summary(fit)$coefficients
  toExtract    <- c(
    "est"    = "coef",
    "se"     = "se(coef)",
    "rse"    = "robust se",
    "zval"   = "z",
    "pval"   = "Pr(>|z|)"
  )
  namesEstimatesFit <- colnames(estimatesFit)
  estimatesFit      <- estimatesFit[,namesEstimatesFit %in% toExtract, drop = FALSE]


  # make into a data.frame
  if (is.null(estimatesFit))            return()
  else if (is.null(dim(estimatesFit)))  estimatesFit <- data.frame(t(estimatesFit))
  else                                  estimatesFit <- data.frame(estimatesFit)

  # fix column names
  colnames(estimatesFit) <- names(toExtract)[toExtract %in% namesEstimatesFit]

  # add confidence intervals
  if (options[["coefficientsConfidenceIntervals"]]) {
    estimatesFit$lower <- estimatesFit[,"est"] + qnorm((1 - options[["coefficientsConfidenceIntervalsLevel"]]) / 2) * estimatesFit[,if("rse" %in% colnames(estimatesFit)) "rse" else "se"]
    estimatesFit$upper <- estimatesFit[,"est"] - qnorm((1 - options[["coefficientsConfidenceIntervalsLevel"]]) / 2) * estimatesFit[,if("rse" %in% colnames(estimatesFit)) "rse" else "se"]
  }

  # transform to HR if needed
  if (HR) {
    estimatesFit <- estimatesFit[,!colnames(estimatesFit) %in% c("se", "rse", "zval", "pval"), drop = FALSE]
    estimatesFit <- exp(estimatesFit)
  }

  estimatesFit <- cbind(
    "model" = "",
    "param" = sapply(rownames(estimatesFit), function(x) .saTermNames(x, c(options[["covariates"]], options[["factors"]]))),
    estimatesFit)

  estimatesFit[1, "model"] <- model

  return(estimatesFit)
}

