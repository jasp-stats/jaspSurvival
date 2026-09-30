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


# diagnostics plots
.sapResidualsVsTimePlot                <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualsVsTimePlot"]]) || options[["censoringType"]] != "right")
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualVsTime", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualsVsTimePlotFun,
    name          = "residualsVsTimePlot",
    title         = gettext("Residuals vs. Time"),
    dependencies  = outputDependencies,
    position      = 5.1
  )

  return()
}
.sapResidualsVsPredictorsPlot          <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualsVsPredictorsPlot"]]))
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualVsPredictors", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualsVsPredictorsPlotFun,
    name          = "residualsVsPredictorsPlot",
    title         = gettext("Residuals vs. Predictors"),
    dependencies  = outputDependencies,
    position      = 5.2
  )

  return()
}
.sapResidualsVsPredictedPlot           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualVsPredictedPlot"]]))
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualVsPredicted", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualsVsPredictedPlotFun,
    name          = "residualVsPredictedPlot",
    title         = gettext("Residuals vs. Predicted"),
    dependencies  = outputDependencies,
    position      = 5.3
  )

  return()
}
.sapResidualHistogramPlot              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualHistogram"]]))
    return()

  # extract all models individually
  fit <- .sapExtractFit(jaspResults, options, type = "selected")
  fit <- .sapFlattenFit(fit, options)

  # output dependencies
  outputDependencies <- c(.sapDependencies, "interpretModel", "residualPlotResidualHistogram", "residualPlotResidualType")

  .sapSectionWrapper(
    jaspResults   = jaspResults,
    options       = options,
    fit           = fit,
    tableFunction = .sapResidualHistogramPlotFun,
    name          = "residualHistogram",
    title         = gettext("Residual Histogram"),
    dependencies  = outputDependencies,
    position      = 5.4
  )

  return()
}

.sapResidualsVsTimePlotFun       <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  dataset <- attr(fit, "dataset")
  time    <- .saExtractSurvTimes(dataset, options)
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  tempPlot$plotObject <- try(.saspResidualsPlot(time, res, gettext("Time"), switch(
    options[["residualPlotResidualType"]],
    "response" = gettext("Response"),
    "coxSnell" = gettext("Cox-Snell")
  )))

  return(tempPlot)
}
.sapResidualsVsPredictorsPlotFun <- function(fit, options) {

  residualPlotResidualVsPredictors <- createJaspContainer()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(residualPlotResidualVsPredictors)

  # extract the dataset and compute residuals
  predictorsFit <- model.matrix(fit)
  res           <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  for (i in seq_len(ncol(predictorsFit))) {
    tempPredictorName <- .saTermNames(colnames(predictorsFit)[i], variables = c(options[["covariates"]], options[["factors"]]))
    residualPlotResidualVsPredictors[[paste0("residualPlotResidualVsPredictors", i)]] <- createJaspPlot(
      plot         = .saspResidualsPlot(x = predictorsFit[,i], y = res, xlab = tempPredictorName, ylab = switch(
        options[["residualPlotResidualType"]],
        "response" = gettext("Response"),
        "coxSnell" = gettext("Cox-Snell")
      )),
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
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  if (jaspBase::isTryError(pred)) {
    tempPlot$setError(gettext("The model failed to produce predictions. Consider simplifying the model."))
    return(tempPlot)
  }

  tempPlot$plotObject <- try(.saspResidualsPlot(pred, res, gettext("Predicted Time"), switch(
    options[["residualPlotResidualType"]],
    "response" = gettext("Response"),
    "coxSnell" = gettext("Cox-Snell")
  )))

  return(tempPlot)
}
.sapResidualHistogramPlotFun     <- function(fit, options) {

  tempPlot <- createJaspPlot()

  if (jaspBase::isTryError(fit) || length(fit) == 0)
    return(tempPlot)

  # extract the dataset and compute residuals
  res     <- residuals(fit, type = switch(
    options[["residualPlotResidualType"]],
    "response" = "response",
    "coxSnell" = "coxsnell"
  ))

  tempPlot$plotObject <- try(jaspGraphs::jaspHistogram(res, xName =switch(
    options[["residualPlotResidualType"]],
    "response" = gettext("Response"),
    "coxSnell" = gettext("Cox-Snell")
  )))

  return(tempPlot)
}
