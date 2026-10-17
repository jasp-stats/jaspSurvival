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

# Residual plots share model selection, dependencies, and section construction.
.sapResidualPlots <- function(jaspResults, options) {

  specifications <- list(
    list(option = "residualPlotResidualVsTime", name = "residualsVsTimePlot", title = gettext("Residuals vs. Time"),
         builder = .sapResidualsVsTimePlotFun, position = 5.1),
    list(option = "residualPlotResidualVsPredictors", name = "residualsVsPredictorsPlot", title = gettext("Residuals vs. Predictors"),
         builder = .sapResidualsVsPredictorsPlotFun, position = 5.2),
    list(option = "residualPlotResidualVsPredicted", name = "residualVsPredictedPlot", title = gettext("Residuals vs. Predicted"),
         builder = .sapResidualsVsPredictedPlotFun, position = 5.3),
    list(option = "residualPlotResidualHistogram", name = "residualHistogram", title = gettext("Residual Histogram"),
         builder = .sapResidualHistogramPlotFun, position = 5.4)
  )
  requested <- Filter(function(specification) {
    options[[specification[["option"]]]] && is.null(jaspResults[[specification[["name"]]]]) &&
      (specification[["option"]] != "residualPlotResidualVsTime" || options[["censoringType"]] == "right")
  }, specifications)
  if (length(requested) == 0)
    return()

  fit <- .sapFlattenFit(.sapExtractFit(jaspResults, options, type = "selected"), options)
  for (specification in requested) {
    .sapSectionWrapper(
      jaspResults    = jaspResults,
      options        = options,
      fit            = fit,
      outputFunction = specification[["builder"]],
      name           = specification[["name"]],
      title          = specification[["title"]],
      dependencies   = c(.sapGetDependencies(options), "interpretModel", specification[["option"]], "residualPlotResidualType"),
      position       = specification[["position"]]
    )
  }

  return()
}

.sapResidualsVsTimePlotFun       <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  res     <- .sapResiduals(fit, options)
  if (jaspBase::isTryError(res)) {
    tempPlot$setError(conditionMessage(attr(res, "condition")))
    return(tempPlot)
  }

  tempPlot$plotObject <- try(.saResidualsPlot(time, res, gettext("Time"), .saResidualsPlotName(options)))

  return(tempPlot)
}
.sapResidualsVsPredictorsPlotFun <- function(fit, options) {

  residualPlotResidualVsPredictors <- createJaspContainer()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(residualPlotResidualVsPredictors)

  # extract the dataset and compute residuals
  predictorsFit <- model.matrix(fit)
  # the predictors of mixture models are repeated for the location parameter of each component
  if (!is.null(attr(fit, "mixture")))
    predictorsFit <- predictorsFit[, fit[["mx"]][[fit[["dlist"]][["location"]]]], drop = FALSE]
  predictorsFit <- .saResidualsPredictors(predictorsFit, stats::model.frame(fit), options[["factors"]])
  res <- .sapResiduals(fit, options)
  if (jaspBase::isTryError(res)) {
    residualPlotResidualVsPredictors$setError(conditionMessage(attr(res, "condition")))
    return(residualPlotResidualVsPredictors)
  }

  for (i in seq_len(ncol(predictorsFit))) {
    tempPredictorName <- .saTermNames(colnames(predictorsFit)[i], variables = c(options[["covariates"]], options[["factors"]]))
    residualPlotResidualVsPredictors[[paste0("residualPlotResidualVsPredictors", i)]] <- createJaspPlot(
      plot         = .saResidualsPlot(x = predictorsFit[,i], y = res, xlab = tempPredictorName, ylab = .saResidualsPlotName(options)),
      title        = gettextf("Residuals vs. %1$s", tempPredictorName),
      position     = i,
      width        = 450,
      height       = 320
    )
  }

  return(residualPlotResidualVsPredictors)
}
.sapResidualsVsPredictedPlotFun  <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  pred    <- try(unlist(predict(fit)))
  res     <- .sapResiduals(fit, options)
  if (jaspBase::isTryError(res)) {
    tempPlot$setError(conditionMessage(attr(res, "condition")))
    return(tempPlot)
  }

  if (jaspBase::isTryError(pred)) {
    tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempPlot)
  }

  tempPlot$plotObject <- try(.saResidualsPlot(pred, res, gettext("Predicted Time"), .saResidualsPlotName(options)))

  return(tempPlot)
}
.sapResidualHistogramPlotFun     <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  res <- .sapResiduals(fit, options)
  if (jaspBase::isTryError(res)) {
    tempPlot$setError(conditionMessage(attr(res, "condition")))
    return(tempPlot)
  }

  tempPlot$plotObject <- try(jaspGraphs::jaspHistogram(res, xName =.saResidualsPlotName(options)))

  return(tempPlot)
}
.sapResiduals <- function(fit, options) {

  # flexsurv cannot compute Cox-Snell residuals from interval-censored outcomes.
  # Keep an unsupported diagnostic from aborting the remaining analysis output.
  return(try({
    if (options[["censoringType"]] == "interval" && options[["residualPlotResidualType"]] == "coxSnell")
      stop(gettext("Cox-Snell residuals are not available for interval-censored data."), call. = FALSE)

    residuals(fit, type = switch(
      options[["residualPlotResidualType"]],
      "response" = "response",
      "coxSnell" = "coxsnell"
    ))
  }, silent = TRUE))
}
