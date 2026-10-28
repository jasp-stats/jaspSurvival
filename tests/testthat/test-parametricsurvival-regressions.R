# Independent references: survival::lung and the Weibull/exponential likelihoods.
# Population anchors also reproduce the independent PR 30 review (19 Oct 2026):
# https://claude.ai/artifact/EHzskkZn7dL7wfedcUbPci, finding 8.
.parametricReviewCase <- function(weighted = FALSE) {
  file <- testthat::test_path("jaspfiles", "other", "flexsurv-lung.jasp")
  options <- jaspTools::analysisOptions(file)[[1L]]
  options[["analysisType"]] <- "parametric"
  options[["timeToEvent"]] <- "time"
  options[["eventStatus"]] <- "event"
  options[["eventIndicator"]] <- "1"
  options[["covariates"]] <- "age"
  options[["factors"]] <- "sex"
  options[["subgroup"]] <- character(0)
  options[["weights"]] <- if (weighted) "frequency" else ""
  options[["distribution"]] <- "weibull"
  options[["modelAdvancedSpecification"]] <- FALSE
  options[["modelAncillaryParameterRegression"]] <- FALSE
  options[["restrictParameters"]] <- FALSE
  options[["modelTerms"]] <- list(list(name = "model1", title = "Model 1", components = list("age", "sex")))
  options <- jaspSurvival:::.sapPrepareModelTerms(options)
  options[["predictionsPredictFor"]] <- "variables"
  options[["predictionsOutputPlotType"]] <- "standard"
  options[["predictionsXAxisVariables"]] <- "sex"
  options[["predictionsSeparateLines"]] <- options[["predictionsLifetimeTimeId"]]
  options[["predictionsSeparatePlots"]] <- character(0)
  options[["predictionsUnassignedReferenceValues"]] <- "average"
  options[["predictionsDisplayConfidenceIntervals"]] <- FALSE
  options[["predictionsLifetimePercentilesGrid"]] <- "custom"
  options[["predictionsLifetimePercentilesValues"]] <- "25, 50, 75"
  options[["setSeed"]] <- TRUE
  options[["seed"]] <- 1L
  options[["confidenceIntervalSimulationDraws"]] <- 256L
  options[["simulationContext"]] <- NULL
  lung <- survival::lung
  data <- data.frame(time = lung$time, event = as.integer(lung$status == 2), age = lung$age,
    sex = factor(lung$sex), frequency = rep(1:3, length.out = nrow(lung)))
  attr(data, "subgroup") <- attr(data, "subgroupLabel") <- "Full dataset"
  return(list(options = options, data = data, model = options[["modelTerms"]][[1L]]))
}

test_that("Fixed Weibull shape equals an independently fitted exponential model", {
  case <- .parametricReviewCase()
  options <- case$options
  options[["restrictParameters"]] <- TRUE
  options[["restrictParametersValues"]] <- list(list(restriction = list("weibull:1"), shape = "1", scale = ""))
  fit <- jaspSurvival:::.sapFitModel(case$data, options, "weibull", case$model, 1L)
  # A censored exponential likelihood has the same score as this exposure GLM.
  reference <- stats::glm(event ~ age + sex + offset(log(time)), data = case$data, family = stats::poisson())
  rate <- exp(stats::predict(reference) - log(case$data$time))
  logLik <- sum(case$data$event * log(rate) - rate * case$data$time)
  expect_equal(fit[["loglik"]], logLik, tolerance = 1e-8)
  expect_equal(unname(fit[["res.t"]][-1L, "est"]), unname(-stats::coef(reference)), tolerance = 1e-6)
  expect_equal(fit[["npars"]], 3)
  expect_equal(fit[["loglik"]], -1156.099, tolerance = 1e-6)
})

test_that("Single free parameter inference agrees with native and analytic references", {
  case <- .parametricReviewCase()
  options <- case$options
  model <- case$model
  model[["components"]] <- list()
  options[["restrictParameters"]] <- TRUE
  options[["restrictParametersValues"]] <- list(list(restriction = list("weibull:1"), shape = "1", scale = ""))
  fit <- jaspSurvival:::.sapFitModel(case$data, options, "weibull", model, 1L)
  events <- sum(case$data[["event"]])
  scale <- sum(case$data[["time"]]) / events
  expect_equal(unname(fit[["res"]]["scale", "est"]), scale, tolerance = 1e-5)
  expect_equal(unname(fit[["res"]]["scale", "se"]), scale / sqrt(events), tolerance = 1e-5)
  expect_equal(unname(fit[["res"]]["scale", 2:3]), scale * exp(c(-1, 1) * stats::qnorm(.975) / sqrt(events)), tolerance = 1e-5)
  expect_true(all(is.na(fit[["res"]]["shape", -1L])))
  expect_null(attr(fit, "inferenceUnavailable", exact = TRUE))
  predictions <- jaspSurvival:::.sapSummaryPredictions(fit, t = c(100, 500), ci = TRUE, B = 256L, seed = 1L)
  expect_true(all(is.finite(unlist(predictions[[1L]][c("lcl", "ucl")]))))

  options[["restrictParametersValues"]][[1L]][["shape"]] <- ""
  options[["restrictParametersValues"]][[1L]][["scale"]] <- "400"
  fit <- jaspSurvival:::.sapFitModel(case$data, options, "weibull", model, 1L)
  reference <- flexsurv::flexsurvreg(survival::Surv(time, event) ~ 1, data = case$data,
    dist = "weibull", inits = c(1, 400), fixedpars = 2L)
  expect_equal(unname(fit[["res"]]["shape", ]), unname(reference[["res"]]["shape", ]), tolerance = 1e-5)
  expect_true(all(is.na(fit[["res"]]["scale", -1L])))

  options[["restrictParameters"]] <- FALSE
  fit <- jaspSurvival:::.sapFitModel(case$data, options, "exp", model, 1L)
  expect_equal(unname(fit[["res"]]["rate", "se"]), 1 / (scale * sqrt(events)), tolerance = 1e-5)
  expect_null(attr(fit, "inferenceUnavailable", exact = TRUE))
})

test_that("Probability plots omit the missing confidence interval note", {
  case <- .parametricReviewCase()
  options <- case$options
  options[["probabilityPlotStratifyBy"]] <- FALSE
  options[["probabilityPlotConfidenceIntervals"]] <- TRUE
  fit <- jaspSurvival:::.sapFitModel(case$data, options, "weibull", case$model, 1L)
  fit <- jaspSurvival:::.sapNativeUnavailableInference(fit, "Unavailable covariance for regression test")
  plotData <- jaspSurvival:::.sapProbabilityPlotData(list(fit), options)
  note <- gettext("Some confidence intervals could not be evaluated and are shown as missing.")
  expect_true(nrow(plotData[["curveData"]]) > 0L)
  expect_true(all(is.na(plotData[["curveData"]][c("lCi", "uCi")])))
  expect_true(note %in% attr(plotData[["curveData"]], "predictionWarnings"))
  expect_false(note %in% plotData[["notes"]])
  plot <- jaspSurvival:::.sapCreateProbabilityPlot(list(fit), options)
  expect_false(any(grepl(note, plot$labels$caption, fixed = TRUE)))
})

test_that("Probability point shapes describe strata and subgroups, not distributions", {
  case <- .parametricReviewCase()
  options <- case$options
  options[["probabilityPlotStratifyBy"]] <- FALSE
  options[["probabilityPlotConfidenceIntervals"]] <- FALSE
  fits <- lapply(c("llogis", "lnorm", "weibull"), function(distribution)
    jaspSurvival:::.sapFitModel(case$data, options, distribution, case$model, 1L))
  pointShapes <- function(fits, options) {
    plot <- jaspSurvival:::.sapCreateProbabilityPlot(fits, options)
    layer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
    return(list(data = plot$layers[[layer]]$data, shape = ggplot2::ggplot_build(plot)$data[[layer]]$shape))
  }
  single <- pointShapes(fits[1L], options)
  merged <- pointShapes(fits, options)
  expect_equal(unique(single$shape), 21)
  expect_equal(unique(merged$shape), unique(single$shape))

  options[["probabilityPlotStratifyBy"]] <- TRUE
  options[["probabilityPlotStratifyByVariables"]] <- list(list(variable = "sex", stratify = TRUE))
  options[["probabilityPlotMergePlotsAcrossStrata"]] <- TRUE
  stratified <- pointShapes(fits, options)
  expect_length(unique(stratified$shape), 2L)
  expect_true(all(vapply(split(stratified$shape, stratified$data$Level), function(shape)
    length(unique(shape)) == 1L, logical(1))))

  options[["probabilityPlotStratifyBy"]] <- FALSE
  options[["subgroup"]] <- "sex"
  options[["probabilityPlotMergePlotsAcrossSubgroups"]] <- TRUE
  subgroupFits <- unlist(lapply(c("Group 1", "Group 2"), function(group) lapply(fits, function(fit) {
    attr(fit, "subgroup") <- attr(fit, "subgroupLabel") <- group
    return(fit)
  })), recursive = FALSE)
  grouped <- pointShapes(subgroupFits, options)
  expect_length(unique(grouped$shape), 2L)
  expect_true(all(vapply(split(grouped$shape, grouped$data$Level), function(shape)
    length(unique(shape)) == 1L, logical(1))))
})

test_that("One-sided coefficient limits use the requested normal tail", {
  case <- .parametricReviewCase()
  fit <- jaspSurvival:::.sapFitModel(case$data, case$options, "weibull", case$model, 1L)
  for (direction in c("lower", "upper")) {
    bounds <- jaspSurvival:::.sapCoefficientBounds(fit, .95, direction)
    expected <- fit[["res.t"]]["age", "est"] + (if (direction == "lower") -1 else 1) *
      stats::qnorm(.95) * fit[["res.t"]]["age", "se"]
    expect_equal(unname(bounds["age", direction]), unname(expected), tolerance = 1e-12)
    expect_true(all(is.na(bounds[, if (direction == "lower") "upper" else "lower"])))
  }
  expect_equal(unname(jaspSurvival:::.sapCoefficientBounds(fit, .95, "lower")["age", "lower"]),
    -0.02370105, tolerance = 1e-6)
})

test_that("Population summaries equal independent averages over lung profiles", {
  case <- .parametricReviewCase()
  fit <- jaspSurvival:::.sapFitModel(case$data, case$options, "weibull", case$model, 1L)
  options <- case$options
  options[["predictionsUnassignedReferenceValues"]] <- "population"
  profile <- case$data[1L, c("age", "sex")]
  profile[["sex"]] <- factor("1", levels = levels(case$data[["sex"]]))
  fit <- jaspSurvival:::.sapPredictionConditionedFit(fit, profile, options, "sex")
  shape <- fit[["res"]]["shape", "est"]
  scale <- exp(fit[["res.t"]]["scale", "est"] + fit[["res.t"]]["age", "est"] * case$data[["age"]])
  survival <- function(t) mean(stats::pweibull(t, shape, scale, lower.tail = FALSE))
  value <- function(type, at) jaspSurvival:::.sapPopulationPointSummary(fit, type, at)[[1L]][["est"]]
  expect_equal(value("survival", c(200, 500)), c(survival(200), survival(500)), tolerance = 1e-10)
  expect_equal(value("hazard", 200), mean(stats::dweibull(200, shape, scale)) / survival(200), tolerance = 1e-10)
  expect_equal(value("cumhaz", 200), -log(survival(200)), tolerance = 1e-10)
  expect_equal(value("mean", 0), mean(scale * gamma(1 + 1 / shape)), tolerance = 1e-10)
  # Native RMST uses numerical quadrature with looser accuracy than this reference.
  expect_equal(value("rmst", 500), stats::integrate(Vectorize(survival), 0, 500, rel.tol = 1e-10)$value, tolerance = 1e-7)
  median <- stats::uniroot(function(t) survival(t) - .5, c(1, 2000), tol = 1e-9)$root
  expect_equal(value("median", 0), median, tolerance = 1e-8)
  expect_equal(value("survival", c(200, 500)), c(.632528, .217299), tolerance = 1e-6)
  expect_equal(value("median", 0), 273.852, tolerance = 1e-5)
})

test_that("Both lifetime ratios share reference profiles and joint uncertainty", {
  case <- .parametricReviewCase()
  fit <- jaspSurvival:::.sapFitModel(case$data, case$options, "weibull", case$model, 1L)
  for (mode in c("average", "population")) {
    options <- case$options
    options[["predictionsUnassignedReferenceValues"]] <- mode
    options[["predictionsDisplayConfidenceIntervals"]] <- TRUE
    scenarios <- jaspSurvival:::.sapModelPredictionScenarios(fit, options)
    for (type in c("quantile", "median", "mean")) {
      at <- if (type == "quantile") c(.25, .5, .75) else 0
      values <- function(measure) jaspSurvival:::.sapPredictionRatioSummary(fit, scenarios, options, at, type, measure)
      ratio <- values("timeRatio")
      acceleration <- values("relativeSurvivalTime")
      extract <- function(rows, field) unlist(lapply(rows, `[[`, field), use.names = FALSE)
      expect_equal(extract(ratio, "est") * extract(acceleration, "est"), rep(1, length(extract(ratio, "est"))), tolerance = 1e-12)
      expect_true(all(is.finite(extract(ratio, "lcl"))))
      expect_equal(extract(ratio, "lcl") * extract(acceleration, "ucl"), rep(1, length(extract(ratio, "est"))), tolerance = 1e-4)
      expect_true(any(grepl("Reference:", attr(ratio, "predictionWarnings"), fixed = TRUE)))
    }
  }
})

test_that("Prediction ranges evaluate event-time quantiles rather than coefficient bounds", {
  case <- .parametricReviewCase()
  fit <- jaspSurvival:::.sapFitModel(case$data, case$options, "weibull", case$model, 1L)
  options <- case$options
  options[["predictionsDisplayPredictionIntervalLevel"]] <- .9
  interval <- jaspSurvival:::.sapModelPredictionRange(list(fit), options, "mean")
  shape <- fit[["res"]]["shape", "est"]
  scale <- exp(fit[["res.t"]]["scale", "est"] + fit[["res.t"]]["age", "est"] * mean(case$data[["age"]]) +
    c(0, fit[["res.t"]]["sex2", "est"]))
  expect_equal(interval[["lower"]], stats::qweibull(.05, shape, scale), tolerance = 1e-8)
  expect_equal(interval[["upper"]], stats::qweibull(.95, shape, scale), tolerance = 1e-8)
})

test_that("CSV input retains prediction values and rejects missing predictors", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(data.frame(age = c(45, 70), sex = c("1", "2")), path, row.names = FALSE)
  data <- jaspSurvival:::.sapCustomPredictionCsv(path, c("age", "sex"))
  expect_equal(as.numeric(data[["age"]]), c(45, 70))
  expect_equal(as.character(data[["sex"]]), c("1", "2"))
  expect_error(jaspSurvival:::.sapCustomPredictionCsv(path, c("age", "unknown")), "missing these columns")
})

test_that("Frequency-derived bases, fits and time grids match repeated rows", {
  case <- .parametricReviewCase(weighted = TRUE)
  rows <- rep(seq_len(nrow(case$data)), case$data[["frequency"]])
  repeated <- case$data[rows, ]
  repeated[["frequency"]] <- 1
  for (kind in c("none", "scale", "polynomial", "naturalSpline")) {
    model <- case$model
    if (kind != "none")
      model[["transformations"]] <- list(age = list(transformation = kind, polynomialDegree = 3L, splineDf = 3L))
    compact <- jaspSurvival:::.sapFitModel(case$data, case$options, "weibull", model, 1L)
    expanded <- jaspSurvival:::.sapFitModel(repeated, case$options, "weibull", model, 1L)
    expect_equal(compact[["loglik"]], expanded[["loglik"]], tolerance = 1e-8)
    expect_equal(unname(compact[["res.t"]][, "est"]), unname(expanded[["res.t"]][, "est"]), tolerance = 1e-6)
    expect_equal(unname(compact[["cov"]]), unname(expanded[["cov"]]), tolerance = 1e-6)
    expect_equal(compact[["N"]], sum(case$data[["frequency"]]))
    expect_equal(nrow(compact[["data"]][["m"]]), nrow(case$data))
    expect_equal(jaspSurvival:::.sapOptions2PredictionTime(case$options, compact, "time", FALSE),
      jaspSurvival:::.sapOptions2PredictionTime(case$options, expanded, "time", FALSE), tolerance = 1e-6)
  }
})

test_that("Moment-initialized families retain the repeated-frequency optimum", {
  case <- .parametricReviewCase(weighted = TRUE)
  repeated <- case$data[rep(seq_len(nrow(case$data)), case$data[["frequency"]]), ]
  repeated[["frequency"]] <- 1
  for (family in c("gamma", "gengamma", "genf")) {
    compact <- jaspSurvival:::.sapFitModel(case$data, case$options, family, case$model, 1L)
    expanded <- jaspSurvival:::.sapFitModel(repeated, case$options, family, case$model, 1L)
    expect_false(inherits(compact, "try-error"))
    expect_false(inherits(expanded, "try-error"))
    expect_equal(compact[["loglik"]], expanded[["loglik"]], tolerance = 1e-8)
    expect_equal(stats::BIC(compact), stats::BIC(expanded), tolerance = 1e-8)
  }
})

test_that("Gompertz starts retain finite censoring tails across time units and frequencies", {
  case <- .parametricReviewCase()
  options <- case$options
  options[["weights"]] <- "frequency"
  options[["covariates"]] <- options[["factors"]] <- character(0)
  model <- case$model
  model[["components"]] <- list()
  model[["factorVariables"]] <- character(0)
  data <- data.frame(time = c(1000, 2000, 3000, 5000), event = c(1, 1, 1, 0), frequency = c(1, 1, 1, 3))
  expect_equal(flexsurv::pgompertz(5000, .001, 1 / stats::weighted.mean(data$time, data$frequency)), 1)

  # Independently profile out the baseline rate in the censored likelihood.
  profile <- function(slope) {
    exposure <- if (slope == 0) data$time else expm1(slope * data$time) / slope
    rate <- sum(data$frequency * data$event) / sum(data$frequency * exposure)
    sum(data$frequency * (data$event * (log(rate) + slope * data$time) - rate * exposure))
  }
  reference <- stats::optimize(function(slope) -profile(slope / 5000), c(-2, 2), tol = 1e-10)
  for (unit in c(1, .001, 1000)) {
    current <- data
    current$time <- current$time * unit
    compact <- jaspSurvival:::.sapFitModel(current, options, "gompertz", model, 1L, silent = TRUE)
    expect_false(inherits(compact, "try-error"))
    expect_null(attr(compact, "inferenceUnavailable", exact = TRUE))
    expect_true(all(is.finite(compact[["res"]][, "se"])))
    expect_equal(compact[["loglik"]], -reference$objective - sum(data$frequency * data$event) * log(unit), tolerance = 1e-7)
    expect_equal(unname(compact[["res"]]["shape", "est"]) * unit, reference$minimum / 5000, tolerance = 1e-7)
    expect_equal(sum(compact[["logliki"]] * current$frequency), compact[["loglik"]], tolerance = 1e-10)
    expect_equal(compact[["AIC"]], -2 * compact[["loglik"]] + 2 * compact[["npars"]], tolerance = 1e-10)
    expect_equal(compact[["BIC"]], -2 * compact[["loglik"]] + log(sum(current$frequency)) * compact[["npars"]], tolerance = 1e-10)

    # Analytic observed information on (shape, log(rate)), without projection.
    shape <- compact[["res"]]["shape", "est"]
    rate <- compact[["res"]]["rate", "est"]
    at <- shape * current$time
    exposure <- expm1(at) / shape
    first <- (at * exp(at) - expm1(at)) / shape^2
    second <- (at^2 * exp(at) - 2 * at * exp(at) + 2 * expm1(at)) / shape^3
    information <- rate * matrix(c(sum(current$frequency * second), rep(sum(current$frequency * first), 2),
      sum(current$frequency * exposure)), 2L)
    expect_equal(unname(compact[["cov"]]), unname(solve(information)), tolerance = 1e-7)
    predictions <- jaspSurvival:::.sapSummaryPredictions(compact, t = c(1000, 4000) * unit, ci = TRUE, B = 256L, seed = 1L)
    expect_true(all(is.finite(unlist(predictions[[1L]][c("est", "lcl", "ucl")]))))
    expect_equal(predictions[[1L]][["est"]], exp(-rate * expm1(shape * c(1000, 4000) * unit) / shape), tolerance = 1e-10)

    expanded <- current[rep(seq_len(nrow(current)), current$frequency), ]
    expanded$frequency <- 1
    unweighted <- options
    unweighted[["weights"]] <- ""
    repeated <- jaspSurvival:::.sapFitModel(expanded, unweighted, "gompertz", model, 1L, silent = TRUE)
    expect_false(inherits(repeated, "try-error"))
    expect_equal(compact[["loglik"]], repeated[["loglik"]], tolerance = 1e-7)
  }

  fitCall <- list(formula = survival::Surv(time, event) ~ 1, data = transform(data, time = time / 1000))
  expect_equal(jaspSurvival:::.sapFrequencyInitials(fitCall, "gompertz"), c(.001, 1 / mean(fitCall$data$time)))
})

test_that("Gompertz time normalization composes with rate and shape regression", {
  case <- .parametricReviewCase(weighted = TRUE)
  data <- expand.grid(age = c(500, 1000), sex = factor(c("1", "2")), probability = seq(.1, .9, length.out = 8L))
  data$frequency <- rep(1:3, length.out = nrow(data))
  shape <- .0001 + .00004 * (data$sex == "2")
  rate <- exp(-9 + .0005 * data$age + .2 * (data$sex == "2"))
  lifetime <- flexsurv::qgompertz(data$probability, shape, rate)
  data$time <- pmin(lifetime, 7000)
  data$event <- as.integer(lifetime <= 7000)
  model <- case$model
  ancillary <- model
  ancillary[["components"]] <- list("sex")
  model[["ancillaryEquations"]] <- list("1" = ancillary)
  fit <- jaspSurvival:::.sapFitModel(data, case$options, "gompertz", model, 1L, silent = TRUE)
  expect_false(inherits(fit, "try-error"))
  expect_null(attr(fit, "inferenceUnavailable", exact = TRUE))

  data$scaledTime <- data$time / 7000
  data$scaledAge <- (data$age - 750) / 250
  reference <- flexsurv::flexsurvreg(survival::Surv(scaledTime, event) ~ scaledAge + sex, anc = list(shape = ~ sex),
    data = data, weights = frequency, dist = "gompertz", inits = c(1, 1))
  mapping <- diag(c(1 / 7000, 1, 1 / 250, 1, 1 / 7000))
  mapping[2L, 3L] <- -750 / 250
  point <- as.vector(mapping %*% reference[["res.t"]][, "est"])
  point[2L] <- point[2L] - log(7000)
  expect_equal(unname(fit[["res.t"]][, "est"]), point, tolerance = 1e-4)
  expect_equal(unname(fit[["cov"]]), unname(mapping %*% reference[["cov"]] %*% t(mapping)), tolerance = 1e-4)
  expect_equal(fit[["loglik"]], reference[["loglik"]] - sum(data$frequency * data$event) * log(7000), tolerance = 1e-7)
  expect_equal(unname(fit[["data"]][["Y"]][, "time"]), data$time)
})

test_that("Gompertz time normalization preserves fixed parameters in original units", {
  case <- .parametricReviewCase()
  options <- case$options
  options[["weights"]] <- "frequency"
  options[["covariates"]] <- options[["factors"]] <- character(0)
  options[["restrictParameters"]] <- TRUE
  model <- case$model
  model[["components"]] <- list()
  model[["factorVariables"]] <- character(0)
  data <- data.frame(time = c(1000, 2000, 3000, 5000), event = c(1, 1, 1, 0), frequency = c(1, 1, 1, 3))
  data$scaledTime <- data$time / 5000
  for (fixed in list(c(shape = .0001), c(rate = .00015), c(shape = 0, rate = .00015))) {
    restriction <- list(restriction = list("gompertz:1"), shape = "", rate = "")
    restriction[names(fixed)] <- as.list(as.character(fixed))
    options[["restrictParametersValues"]] <- list(restriction)
    fit <- jaspSurvival:::.sapFitModel(data, options, "gompertz", model, 1L, silent = TRUE)
    initial <- c(shape = 1, rate = 1)
    initial[names(fixed)] <- fixed * 5000
    reference <- flexsurv::flexsurvreg(survival::Surv(scaledTime, event) ~ 1, data = data, weights = frequency,
      dist = "gompertz", inits = initial, fixedpars = match(names(fixed), names(initial)))
    expect_false(inherits(fit, "try-error"))
    expect_null(attr(fit, "inferenceUnavailable", exact = TRUE))
    expect_equal(unname(fit[["res"]][, "est"]), unname(reference[["res"]][, "est"]) / 5000, tolerance = 1e-7)
    expect_equal(fit[["loglik"]], reference[["loglik"]] - 3 * log(5000), tolerance = 1e-7)
    expect_equal(unname(fit[["res"]][names(fixed), "est"]), unname(fixed), tolerance = 1e-12)
    expect_true(all(is.na(fit[["res"]][names(fixed), "se"])))
  }
})

test_that("Gompertz time normalization handles interval censoring and delayed entry", {
  case <- .parametricReviewCase()
  options <- case$options
  options[["weights"]] <- "frequency"
  options[["covariates"]] <- options[["factors"]] <- character(0)
  options[["intervalStart"]] <- "start"
  options[["intervalEnd"]] <- "stop"
  model <- case$model
  model[["components"]] <- list()
  model[["factorVariables"]] <- character(0)
  for (kind in c("interval", "counting")) {
    options[["censoringType"]] <- kind
    data <- if (kind == "interval") data.frame(start = c(1000, 1000, 3000, 3000, 4500, 5000),
      stop = c(1000, 2000, 3000, 4000, 5000, Inf), frequency = c(1, 2, 1, 1, 1, 1)) else
      data.frame(start = c(0, 500, 1000, 1000, 1500, 2000), stop = c(1000, 2000, 3000, 3500, 4500, 5000),
        event = c(1, 1, 1, 1, 0, 0), frequency = c(1, 2, 1, 2, 1, 1))
    fit <- jaspSurvival:::.sapFitModel(data, options, "gompertz", model, 1L, silent = TRUE)
    response <- if (kind == "interval") survival::Surv(data$start / 5000, data$stop / 5000, type = "interval2") else
      survival::Surv(data$start / 5000, data$stop / 5000, data$event)
    reference <- flexsurv::flexsurvreg(response ~ 1, data = data, weights = frequency, dist = "gompertz", inits = c(1, 1))
    correction <- sum(data$frequency * (unclass(response)[, ncol(response)] == 1)) * log(5000)
    expect_false(inherits(fit, "try-error"))
    expect_null(attr(fit, "inferenceUnavailable", exact = TRUE))
    expect_equal(fit[["loglik"]], reference[["loglik"]] - correction, tolerance = 1e-7)
    expect_equal(unname(fit[["res"]][, "est"]), unname(reference[["res"]][, "est"]) / 5000, tolerance = 1e-7)
  }
})

test_that("Component survival measures are standalone and density contributions sum", {
  family <- jaspSurvival:::.sapmFamily("weibull")
  fit <- structure(list(), components = 2L)
  arguments <- list(shape1 = 2, scale1 = 10, shape2 = 4, scale2 = 30, v1 = .3)
  time <- c(0, 1, 10, 30, 60)
  for (k in 1:2) {
    shape <- arguments[[paste0("shape", k)]]
    scale <- arguments[[paste0("scale", k)]]
    curve <- function(type) jaspSurvival:::.sapmComponentCurve(time, arguments, k, family, fit, type)
    survival <- stats::pweibull(time, shape, scale, lower.tail = FALSE)
    expect_equal(curve("survival"), survival)
    expect_equal(curve("failureProbability"), 1 - survival)
    expect_equal(curve("hazard"), stats::dweibull(time, shape, scale) / survival)
    expect_equal(curve("cumulativeHazard"), (time / scale)^shape)
    rmst <- vapply(time, function(t) if (t == 0) 0 else
      stats::integrate(function(v) stats::pweibull(v, shape, scale, lower.tail = FALSE), 0, t)$value, numeric(1))
    expect_equal(curve("restrictedMeanSurvivalTime"), rmst, tolerance = 1e-7)
  }
  density <- function(k) jaspSurvival:::.sapmComponentCurve(time, arguments, k, family, fit, "density")
  expect_equal(density(1) + density(2), .3 * stats::dweibull(time, 2, 10) + .7 * stats::dweibull(time, 4, 30))
})
