context("Survival regressions")

test_that("residual predictors use fitted factors and contrasts", {
  dataset <- data.frame(time = c(3, 1, 4, 2, 7, 5, 6, 8), status = c(1, 0, 1, 1, 0, 1, 0, 1),
                        x = c(2, 5, 1, 7, 4, 3, 8, 6), xBin = rep(c(0, 1), 4), unused = factor(rep("A", 8)),
                        f = factor(c("A", "B", "B", "A", "A", "B", "A", "B")))
  for (custom in c(FALSE, TRUE)) {
    if (custom) stats::contrasts(dataset[["f"]]) <- stats::contr.sum(2)
    fits <- list(
      survival::coxph(survival::Surv(time, status) ~ x + xBin + f, data = dataset, x = TRUE, model = TRUE),
      flexsurv::flexsurvreg(survival::Surv(time, status) ~ x + xBin + f, data = dataset, dist = "lnorm")
    )
    for (fit in fits) {
      matrix <- stats::model.matrix(fit)
      contrasts <- if (!is.null(fit[["contrasts"]])) fit[["contrasts"]] else attr(matrix, "contrasts")
      contrasts[["unused"]] <- "contr.treatment"
      predictors <- jaspSurvival:::.saspResidualsPredictors(matrix, stats::model.frame(fit), c("unused", "f"), contrasts)
      expect_true(is.factor(predictors[[if (custom) "f1" else "fB"]]))
      expect_true(is.numeric(predictors[["x"]]))
      expect_true(is.numeric(predictors[["xBin"]]))
    }
  }
  omittedFits <- list(
    survival::coxph(survival::Surv(time, status) ~ x + xBin, data = dataset, x = TRUE, model = TRUE),
    flexsurv::flexsurvreg(survival::Surv(time, status) ~ x + xBin, data = dataset, dist = "lnorm")
  )
  for (fit in omittedFits) {
    predictors <- jaspSurvival:::.saspResidualsPredictors(stats::model.matrix(fit), stats::model.frame(fit), c("unused", "f"))
    expect_named(predictors, c("x", "xBin"))
    expect_true(all(vapply(predictors, is.numeric, logical(1))))
  }
  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1, data = dataset[1:5, ], dist = "lnorm")
  predictors <- jaspSurvival:::.saspResidualsPredictors(stats::model.matrix(fit), stats::model.frame(fit), "unused")
  expect_equal(dim(predictors), c(5L, 0L))
})

test_that("Cox residual plots complete with omitted one-level factors", {
  opts <- jaspTools::analysisOptions(testthat::test_path("jaspfiles", "other", "coxph_full_output.jasp"))[[2]]
  opts[["timeToEvent"]] <- "time"
  opts[["eventStatus"]] <- "status"
  opts[["eventIndicator"]] <- "1"
  opts[["covariates"]] <- "x"
  opts[["factors"]] <- "unused"
  opts[["modelTerms"]] <- list(list(components = list("x"), isNuisance = TRUE))
  opts[["residualPlotResidualType"]] <- "martingale"
  opts[["plot"]] <- FALSE
  opts[["proportionalHazardsTable"]] <- FALSE
  opts[["proportionalHazardsPlot"]] <- FALSE
  opts[["residualPlotResidualVsPredicted"]] <- FALSE
  # GUI defaults from qml_components/SurvivalExport.qml.
  opts[c("exportResidualsCoxSnell", "exportResidualsMartingale", "exportResidualsDeviance", "exportFittedRisk", "exportFittedLinearPredictor")] <- rep(list(FALSE), 5)
  opts[["exportColumnPrefix"]] <- ""
  dataset <- data.frame(time = c(3, 1, 4, 2, 7, 5, 6, 8), status = c(1, 0, 1, 1, 0, 1, 0, 1),
                        x = c(2, 5, 1, 7, 4, 3, 8, 6), unused = factor(rep("A", 8)))
  for (request in c("residualPlotResidualHistogram", "residualPlotResidualVsTime", "residualPlotResidualVsPredictors")) {
    opts[c("residualPlotResidualHistogram", "residualPlotResidualVsTime", "residualPlotResidualVsPredictors")] <- rep(list(FALSE), 3)
    opts[[request]] <- TRUE
    encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
    results <- jaspTools::runAnalysis("SemiParametricSurvivalAnalysis", encoded[["dataset"]], encoded[["options"]], encodedDataset = TRUE, view = FALSE)
    expect_identical(results[["status"]], "complete", info = results[["results"]][["errorMessage"]])
    expect_true(length(results[["state"]][["figures"]]) > 0)
    expect_false(any(vapply(results[["results"]][["residualsPlots"]][["collection"]], function(x) identical(x[["status"]], "error"), logical(1))))
  }
})

test_that("location-effect diagnostic is invariant to covariate origin", {
  findAssignment <- function(node) {
    if (is.call(node) && identical(node[[1]], as.name("<-")) && identical(node[[2]], as.name("divergingEffects"))) return(list(node))
    if (!is.recursive(node)) return(list())
    unlist(lapply(as.list(node), findAssignment), recursive = FALSE)
  }
  assignment <- findAssignment(body(jaspSurvival:::.sapmFitMixture))
  expect_length(assignment, 1L)
  evaluate <- function(covariates, beta) {
    eval(assignment[[1]], list2env(list(covariates = covariates, estimates = list(beta = beta), components = length(beta)),
                                 parent = asNamespace("jaspSurvival")))
  }
  raw <- matrix(2000:2010, ncol = 1)
  centered <- raw - 2000
  beta <- list(.02, .02)
  expect_identical(evaluate(raw, beta), evaluate(centered, beta))
  expect_false(any(evaluate(raw, beta)))
  expect_true(evaluate(matrix(c(0, 40), ncol = 1), list(1)))
  expect_false(evaluate(matrix(numeric(0), nrow = 3, ncol = 0), list(numeric(0))))
  family <- jaspSurvival:::.sapmFamily("lnorm")
  response <- survival::Surv(seq_len(11), rep(FALSE, 11))
  for (k in 1:2) {
    rawParameters <- jaspSurvival:::.sapmParameters(family, c(meanlog = -40 + (k - 1) * 3, sdlog = .5), beta[[k]], raw)
    centeredParameters <- jaspSurvival:::.sapmParameters(family, c(meanlog = (k - 1) * 3, sdlog = .5), beta[[k]], centered)
    expect_equal(jaspSurvival:::.sapmComponentLikelihood(family, response, rawParameters),
                 jaspSurvival:::.sapmComponentLikelihood(family, response, centeredParameters), tolerance = 1e-12)
  }
})

test_that("native prediction estimates retain all mixture fit warnings", {
  dataset <- data.frame(time = exp(seq(-1, 4, length.out = 40)), status = TRUE)
  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1, data = dataset, dist = "lnorm")
  set.seed(1)
  plain <- jaspSurvival:::.sapSummaryPredictions(fit, type = "survival", t = c(1, 2), ci = TRUE)
  expect_length(attr(plain, "predictionWarnings"), 0L)
  attr(fit, "mixture") <- list(components = 2L, starts = 2L, replication = 2L, allDegenerate = FALSE,
    selectedDegenerate = FALSE, precisionRejected = 0L, minEvents = 20, warnings = rep("CONTROLLED_WARNING_SENTINEL", 2),
    collapsed = integer(0), duplicated = matrix(integer(0), nrow = 0, ncol = 2), hessianWarning = TRUE)
  set.seed(1)
  predicted <- jaspSurvival:::.sapSummaryPredictions(fit, type = "survival", t = c(1, 2), ci = TRUE)
  messages <- attr(predicted, "predictionWarnings")
  expect_true(all(jaspSurvival:::.sapmFitMessages(fit, options = NULL) %in% messages))
  expect_equal(anyDuplicated(messages), 0L)
  expect_identical(lapply(plain, function(x) x[c("est", "lcl", "ucl")]), lapply(predicted, function(x) x[c("est", "lcl", "ucl")]))
})

test_that("neutral right-zero component fits retain full-row parameters", {
  dataset <- data.frame(time = c(0, 1, 2, 3, 5, 8), status = c(0, 1, 1, 1, 0, 1), x = c(7, 2, 4, 1, 3, 5))
  formula <- survival::Surv(time, status) ~ x
  response <- stats::model.response(stats::model.frame(formula, dataset))
  for (distribution in c("exp", "lnorm", "weibull", "llogis", "gamma")) {
    for (bounded in c(FALSE, TRUE)) {
      if (bounded && distribution == "exp") next
      family <- jaspSurvival:::.sapmFamily(distribution)
      constraint <- if (bounded) jaspSurvival:::.sapmConstraintSpec(list(mixtureConstrainSpread = TRUE, mixtureSpreadType = "absolute", mixtureMinimumLogTimeSd = .1), distribution, dataset, NULL) else NULL
      fit <- jaspSurvival:::.sapmMStep(formula, dataset, as.matrix(dataset["x"]), family, rep(.5, 6), NULL, constraint)
      omitted <- jaspSurvival:::.sapmMStep(formula, dataset[-1, ], as.matrix(dataset[-1, "x", drop = FALSE]), family, rep(.5, 5), NULL, constraint)
      expect_equal(c(fit[["base"]], fit[["beta"]]), c(omitted[["base"]], omitted[["beta"]]), tolerance = 1e-8)
      expect_length(fit[["parameters"]][[family[["location"]]]], 6L)
      logLikelihood <- jaspSurvival:::.sapmComponentLikelihood(family, response, fit[["parameters"]], log = TRUE)
      expect_true(all(is.finite(logLikelihood)))
      expect_equal(logLikelihood[1], 0)
      expect_equal(exp(logLikelihood[1]), 1)
      expect_identical(sum(.5 * logLikelihood), sum(.5 * logLikelihood[-1]))
    }
  }
})

test_that("neutral interval zero is omitted and genuine intervals are retained", {
  dataset <- data.frame(lower = c(0, 1, 2, 3, 5, 8), upper = c(NA, 1, 2, 3, NA, 8))
  formula <- survival::Surv(lower, upper, type = "interval2") ~ 1
  family <- jaspSurvival:::.sapmFamily("lnorm")
  noCovariates <- function(n) matrix(numeric(0), nrow = n, ncol = 0)
  fit <- jaspSurvival:::.sapmMStep(formula, dataset, noCovariates(6), family, rep(.5, 6), NULL)
  omitted <- jaspSurvival:::.sapmMStep(formula, dataset[-1, ], noCovariates(5), family, rep(.5, 5), NULL)
  expect_equal(fit[["base"]], omitted[["base"]], tolerance = 1e-8)
  dataset[["upper"]][1] <- 1
  intervalResponse <- stats::model.response(stats::model.frame(formula, dataset))
  interval <- jaspSurvival:::.sapmMStep(formula, dataset, noCovariates(6), family, rep(.5, 6), NULL)
  dataset[["lower"]][1] <- NA
  leftResponse <- stats::model.response(stats::model.frame(formula, dataset))
  left <- jaspSurvival:::.sapmMStep(formula, dataset, noCovariates(6), family, rep(.5, 6), NULL)
  expect_equal(interval[["base"]], left[["base"]], tolerance = 1e-8)
  expect_equal(jaspSurvival:::.sapmComponentLikelihood(family, intervalResponse, interval[["parameters"]], log = TRUE),
               jaspSurvival:::.sapmComponentLikelihood(family, leftResponse, left[["parameters"]], log = TRUE), tolerance = 1e-8)
  neutral <- data.frame(lower = rep(0, 3), upper = rep(NA_real_, 3))
  expect_error(jaspSurvival:::.sapmMStep(formula, neutral, noCovariates(3), family, rep(.5, 3), NULL), "positive censoring times", fixed = TRUE)
  exactZero <- data.frame(time = c(0, 1, 2), status = rep(1, 3))
  expect_error(jaspSurvival:::.sapmMStep(survival::Surv(time, status) ~ 1, exactZero, noCovariates(3), family, rep(.5, 3), NULL), "Invalid survival times", fixed = TRUE)
})

test_that("active component spread bounds survive neutral-row fitting", {
  dataset <- data.frame(time = c(0, 1, 2, 3, 5, 8), status = c(0, 1, 1, 1, 0, 1))
  formula <- survival::Surv(time, status) ~ 1
  for (distribution in c("lnorm", "weibull", "llogis", "gamma")) {
    family <- jaspSurvival:::.sapmFamily(distribution)
    constraint <- jaspSurvival:::.sapmConstraintSpec(list(mixtureConstrainSpread = TRUE, mixtureSpreadType = "absolute", mixtureMinimumLogTimeSd = 3), distribution, dataset, NULL)
    fit <- jaspSurvival:::.sapmMStep(formula, dataset, matrix(numeric(0), 6, 0), family, rep(.5, 6), NULL, constraint)
    omitted <- jaspSurvival:::.sapmMStep(formula, dataset[-1, ], matrix(numeric(0), 5, 0), family, rep(.5, 5), NULL, constraint)
    point <- jaspSurvival:::.sapmConstraintPoint(fit[["base"]], constraint, family, 1L)
    expect_true(point[["feasible"]])
    expect_true(any(point[["active"]]))
    expect_equal(fit[["base"]], omitted[["base"]], tolerance = 1e-8)
  }
})

test_that("weighted censoring summaries count censored frequency weights", {
  dataset <- data.frame(event = c(TRUE, FALSE), weights = c(3, 4))
  for (type in c("right", "left", "counting")) {
    summary <- jaspSurvival:::.saCensoringSummaryFun(dataset, list(censoringType = type, weights = "weights", eventStatus = "event"))
    expect_equal(summary[["count"]], c(3, 4))
    unweighted <- jaspSurvival:::.saCensoringSummaryFun(dataset, list(censoringType = type, weights = "", eventStatus = "event"))
    expect_equal(unweighted[["count"]], c(1, 1))
  }
  interval <- data.frame(lower = c(1, NA, 2, 3), upper = c(1, 2, NA, 4), weights = c(3, 4, 5, 6))
  summary <- jaspSurvival:::.saCensoringSummaryFun(interval, list(censoringType = "interval", weights = "weights", intervalStart = "lower", intervalEnd = "upper"))
  expect_equal(summary[["count"]], c(3, 4, 5, 6))
})

test_that("mixture analysis retains neutral rows and suppresses active-bound inference", {
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  opts[["timeToEvent"]] <- "time"
  opts[["eventStatus"]] <- "status"
  opts[["eventIndicator"]] <- "1"
  opts[["distribution"]] <- "logNormal"
  opts[["setSeed"]] <- TRUE
  opts[["seed"]] <- 1
  opts[["mixtureStartQuantiles"]] <- FALSE
  opts[["mixtureStartSplit"]] <- FALSE
  opts[["mixtureStartRandom"]] <- FALSE
  opts[["mixtureEmIterations"]] <- 2
  opts[["probabilityPlot"]] <- FALSE
  opts[["mixtureComponentPlot"]] <- FALSE
  opts[["coefficients"]] <- TRUE
  dataset <- data.frame(time = c(0, exp(seq(-.3, .3, length.out = 20)), exp(seq(2.7, 3.3, length.out = 20))), status = c(0, rep(1, 40)))
  for (bounded in c(FALSE, TRUE)) {
    opts[["mixtureConstrainSpread"]] <- bounded
    opts[["mixtureSpreadType"]] <- "absolute"
    opts[["mixtureMinimumLogTimeSd"]] <- .5
    encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
    results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded[["dataset"]], encoded[["options"]], encodedDataset = TRUE, view = FALSE)
    expect_identical(results[["status"]], "complete", info = results[["results"]][["errorMessage"]])
    fits <- Filter(function(x) is.list(x) && !is.null(x[["fullDataset"]]), results[["state"]][["other"]])
    expect_length(fits, 1L)
    fit <- fits[[1]][["fullDataset"]][["lnorm"]][["2"]][[1]]
    expect_s3_class(fit, "flexsurvreg")
    mixture <- attr(fit, "mixture")
    expect_equal(dim(mixture[["posterior"]]), c(41L, 2L))
    expect_equal(rowSums(mixture[["posterior"]]), rep(1, 41), tolerance = 1e-12)
    estimates <- jaspSurvival:::.sapmComponentEstimates(fit, jaspSurvival:::.sapmFamily("lnorm"), 2L)
    expect_equal(mixture[["posterior"]][1, ], estimates[["probabilities"]], tolerance = 1e-12)
    if (bounded) {
      expect_true(attr(fit, "constraints")[["active"]])
      expect_true(all(is.na(fit[["cov"]])))
      expect_true(all(is.na(fit[["res"]][, colnames(fit[["res"]]) != "est"])))
    }
  }
})

test_that("relative spread bounds use the native one-component reference and survive time rescaling", {
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  opts[["timeToEvent"]] <- "time"
  opts[["eventStatus"]] <- "status"
  opts[["censoringType"]] <- "right"
  opts[["weights"]] <- ""
  opts[["covariates"]] <- list()
  opts[["factors"]] <- list()
  opts[["modelTerms"]] <- list()
  opts[["mixtureConstrainSpread"]] <- TRUE
  opts[["mixtureSpreadType"]] <- "relative"
  # PercentField GUI default 1% is passed to R as the fraction 0.01.
  opts[["mixtureMinimumLogTimeSdRelative"]] <- .01
  dataset <- data.frame(time = exp(seq(-1, 3, length.out = 40)), status = TRUE)
  native <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1, data = dataset, dist = "lnorm", hessian = FALSE)
  constraint <- jaspSurvival:::.sapmConstraintSpec(opts, "lnorm", dataset, opts[["modelTerms"]])
  expect_equal(constraint[["referenceLogTimeSd"]], unname(native[["res"]]["sdlog", "est"]), tolerance = 1e-8)
  expect_equal(constraint[["minimumLogTimeSd"]], .01 * unname(native[["res"]]["sdlog", "est"]), tolerance = 1e-8)
  expect_identical(constraint[["relativePercent"]], 1)
  rescaled <- dataset
  rescaled[["time"]] <- 60 * rescaled[["time"]]
  scaledConstraint <- jaspSurvival:::.sapmConstraintSpec(opts, "lnorm", rescaled, opts[["modelTerms"]])
  expect_equal(scaledConstraint[["referenceLogTimeSd"]], constraint[["referenceLogTimeSd"]], tolerance = 1e-8)
  expect_equal(scaledConstraint[["minimumLogTimeSd"]], constraint[["minimumLogTimeSd"]], tolerance = 1e-8)
})
