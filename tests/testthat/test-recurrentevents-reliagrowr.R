# ReliaGrowR 0.7 internals that R/recurrentevents-*.R relies on, and the checks
# that reject fits ReliaGrowR stops at its optimizer bounds. A failure after a
# ReliaGrowR update means those backend assumptions need revisiting.
# Independent references: grouped Poisson profile likelihoods on t / max(t)
# maximized with stats::optimize, and stats::dpois for full log likelihoods.

.recurrentFit <- function(time, counts, model, method = "mle", breaks = "") {
  options <- list(time = "time", eventCount = "count", timeType = "cumulative", model = model,
    estimationMethod = method, piecewiseStartingBreakpoints = nzchar(breaks),
    piecewiseStartingBreakpointsValues = breaks, setSeed = TRUE, seed = 1L)
  record <- jaspSurvival:::.sareFitModel(data.frame(time = time, count = counts), options)
  record[["label"]] <- "Full dataset"
  return(record)
}

.logLinearProfileMle <- function(time, counts) {
  s     <- time / max(time)
  lower <- c(0, s[-length(s)])
  logIntegral <- function(from, to, b) if (b == 0) log(to - from) else b * from + log(expm1(b * (to - from)) / b)
  profile <- function(b) sum(counts * (mapply(logIntegral, lower, s, b) - logIntegral(0, 1, b)))
  b <- stats::optimize(profile, c(-40, 40), maximum = TRUE, tol = 1e-10)$maximum
  return(c(a = log(sum(counts)) - logIntegral(0, 1, b), b = b))
}

.powerLawProfileMle <- function(time, counts) {
  s     <- time / max(time)
  ratio <- c(0, s[-length(s)]) / s
  # s_i^beta - s_{i-1}^beta = s_i^beta * (1 - ratio_i^beta), without cancellation.
  profile <- function(logBeta) sum(counts * (exp(logBeta) * log(s) + log(-expm1(exp(logBeta) * log(ratio)))))
  return(exp(stats::optimize(profile, c(-40, 3), maximum = TRUE, tol = 1e-12)$maximum))
}

.growthTime   <- c(50, 120, 210, 330, 480, 650)
.growthCounts <- c(4, 5, 4, 5, 4, 3)

test_that("ReliaGrowR power-law fits keep the covariance order and scale used for reporting", {
  native <- ReliaGrowR::nhpp(.growthTime / max(.growthTime), .growthCounts, model_type = "Power Law", method = "MLE")
  # Rescaling and the delta method assume the native covariance order (beta, lambda).
  expect_equal(native$vcov[1, 1], native$params_se$beta_se^2)
  # On t / max(t) the profile rate equals the event total, far from the 1e-10 lambda bound.
  expect_equal(native$params$lambda, sum(.growthCounts), tolerance = 1e-6)

  fit <- .recurrentFit(.growthTime, .growthCounts, "crowAmsaa")
  expect_equal(fit$estimates[["beta"]], .powerLawProfileMle(.growthTime, .growthCounts), tolerance = 1e-6)
  expect_equal(fit$estimates[["lambda"]], native$params$lambda / max(.growthTime)^native$params$beta)
  expect_equal(sqrt(fit$covariance["beta", "beta"]), native$params_se$beta_se)

  # The reported covariance inverts the observed information in the original time units.
  negLogLik <- function(par) {
    increments <- par[1] * diff(c(0, .growthTime^par[2]))
    -sum(stats::dpois(.growthCounts, increments, log = TRUE))
  }
  information <- stats::optimHess(fit$estimates, negLogLik, control = list(ndeps = 1e-5 * fit$estimates))
  expect_equal(unname(fit$covariance), unname(solve(information)), tolerance = 1e-4)

  # Reported parameters follow the time unit: hours to seconds.
  seconds <- .recurrentFit(3600 * .growthTime, .growthCounts, "crowAmsaa")
  expect_equal(seconds$estimates[["beta"]], fit$estimates[["beta"]], tolerance = 1e-8)
  expect_equal(seconds$estimates[["lambda"]], fit$estimates[["lambda"]] / 3600^fit$estimates[["beta"]], tolerance = 1e-8)
})

test_that("ReliaGrowR log likelihoods omit only the Poisson count constant", {
  for (model in c("crowAmsaa", "logLinear")) {
    native <- ReliaGrowR::nhpp(.growthTime / max(.growthTime), .growthCounts,
      model_type = if (model == "crowAmsaa") "Power Law" else "Log-Linear", method = "MLE")
    logLik <- sum(stats::dpois(.growthCounts, diff(c(0, native$fitted_values)), log = TRUE))
    expect_equal(native$logLik, logLik + sum(lgamma(.growthCounts + 1)), info = model)

    row <- jaspSurvival:::.sareSummaryRow(.recurrentFit(.growthTime, .growthCounts, model))
    expect_equal(row$logLik, logLik, info = model)
    expect_equal(row$aic, -2 * logLik + 4, info = model)
    expect_equal(row$bic, -2 * logLik + 2 * log(length(.growthCounts)), info = model)
  }
})

test_that("Log-linear fits stopped at ReliaGrowR's optimizer bounds are rejected", {
  time   <- 1:10
  growth <- round(exp(0.8 * time))
  # The intensity rises about exp(8)-fold over follow-up, beyond the |b| <= 5 bound on t / max(t).
  for (counts in list(growth, rev(growth))) {
    reference <- .logLinearProfileMle(time, counts)
    expect_gt(abs(reference[["b"]]), 5)
    native <- ReliaGrowR::nhpp(time / max(time), counts, model_type = "Log-Linear")
    expect_equal(abs(native$params$b), 5)
    expect_error(.recurrentFit(time, counts, "logLinear"), "numerical bound")
  }

  # About 6e8 events put the rate intercept beyond the a <= 20 bound.
  flat <- rep(2e8, 3)
  expect_gt(.logLinearProfileMle(1:3, flat)[["a"]], 20)
  expect_equal(ReliaGrowR::nhpp((1:3) / 3, flat, model_type = "Log-Linear")$params$a, 20)
  expect_error(.recurrentFit(1:3, flat, "logLinear"), "numerical bound")
})

test_that("Log-linear fits just inside ReliaGrowR's optimizer bounds are kept", {
  growth <- round(exp(0.4 * (1:10)))
  for (counts in list(growth, rev(growth), rep(1e8, 3))) {
    time      <- seq_along(counts)
    reference <- .logLinearProfileMle(time, counts)
    expect_lt(abs(reference[["b"]]), 5)
    expect_lt(reference[["a"]], 20)
    fit <- .recurrentFit(time, counts, "logLinear")
    # Reported on the original time scale: a = a* - log(max t), b = b* / max(t).
    expect_equal(fit$estimates[["a"]], reference[["a"]] - log(max(time)), tolerance = 1e-3)
    expect_equal(fit$estimates[["b"]], reference[["b"]] / max(time), tolerance = 1e-3)
  }
})

test_that("Power-law optimizer failures near beta = 0 are reported readably", {
  # True beta below the 1e-6 bound: ReliaGrowR 0.7 fails with non-finite finite
  # differences. Should it return the bound instead, the fit must still be rejected.
  time   <- c(1, 1e3, 2e3)
  counts <- c(1e6, 1, 1)
  expect_lt(.powerLawProfileMle(time, counts), 1e-6)
  native <- try(ReliaGrowR::nhpp(time / max(time), counts, model_type = "Power Law", method = "MLE"), silent = TRUE)
  if (!jaspBase::isTryError(native))
    expect_lte(native$params$beta, 1e-6 * 1.01)
  expect_error(.recurrentFit(time, counts, "crowAmsaa"), "did not converge|numerical bound")

  # True beta of about 0.0014: ReliaGrowR 0.7 still fails, with an infinite objective.
  time   <- c(1e-6, 0.5, 1)
  counts <- c(100, 1, 1)
  expect_gt(.powerLawProfileMle(time, counts), 1e-3)
  expect_error(ReliaGrowR::nhpp(time, counts, model_type = "Power Law", method = "MLE"))
  expect_error(.recurrentFit(time, counts, "crowAmsaa"), "did not converge")
  # The least-squares estimate the message suggests remains available.
  expect_true(is.finite(.recurrentFit(time, counts, "crowAmsaa", "ls")$estimates[["beta"]]))
})

test_that("ReliaGrowR outputs keep the fields and scales the backend reads", {
  time       <- seq(10, 160, by = 10)
  counts     <- c(6, 7, 8, 8, 8, 8, 9, 8, 4, 3, 4, 3, 3, 3, 3, 3)
  cumulative <- cumsum(counts)

  # Duane cumulates its time argument, so the backend passes interval durations.
  duane <- ReliaGrowR::duane(diff(c(0, time)), counts)
  expect_equal(duane$Cumulative_Time, time)
  expect_equal(unname(duane$model$model[[1L]]), log(time / cumulative))
  expect_equal(unname(duane$Fitted_Values), unname(exp(stats::fitted(duane$model))))

  # Least-squares bands are confidence intervals for the fitted curve, not prediction intervals.
  ls <- ReliaGrowR::nhpp(time, counts, model_type = "Power Law", method = "LS")
  expect_warning(prediction <- ReliaGrowR::predict_nhpp(ls, time, conf_level = 0.9), "Hindcasting is allowed", fixed = TRUE)
  reference <- exp(stats::predict(ls$model, newdata = data.frame(log_times = log(time)), interval = "confidence", level = 0.9))
  expect_equal(unname(prediction$cum_events), unname(reference[, "fit"]))
  expect_equal(unname(prediction$lower_bounds), unname(reference[, "lwr"]))
  expect_equal(unname(prediction$upper_bounds), unname(reference[, "upr"]))
  expect_warning(prediction <- ReliaGrowR::predict_duane(duane, time, conf_level = 0.9), "Hindcasting is allowed", fixed = TRUE)
  expect_true(all(c("mtbf", "lower_bounds", "upper_bounds") %in% names(prediction)))

  # Piecewise change points are on the log-time scale with native standard errors.
  set.seed(1)
  piecewise <- ReliaGrowR::rga(time, counts, times_type = "cumulative_failure_times", model_type = "Piecewise NHPP",
    breaks = 60, method = "LS")
  expect_true(all(c("Est.", "St.Err") %in% colnames(piecewise$model$psi)))
  expect_true(exp(piecewise$model$psi[, "Est."]) > min(time) && exp(piecewise$model$psi[, "Est."]) < max(time))
  slopes <- piecewise$betas$log_times[, "Est."]
  expect_equal(piecewise$growth_rate, 1 - slopes)
  expect_equal(unname(piecewise$betas_se), unname(piecewise$betas$log_times[, "St.Err."]))
  expect_equal(piecewise$lambdas[, "Est."], exp(segmented::intercept(piecewise$model)$log_times[, "Est."]))
  expect_warning(prediction <- ReliaGrowR::predict_rga(piecewise, time, conf_level = 0.9), "Hindcasting is allowed", fixed = TRUE)
  expect_equal(as.numeric(prediction$cum_failures), as.numeric(exp(stats::fitted(piecewise$model))))

  # Every model passes through the backend's fit and prediction paths.
  measures <- c("survivalProbability", "failureProbability", "cumulativeEvents", "eventIntensity",
    "cumulativeMeanTimeBetweenEvents", "instantaneousMeanTimeBetweenEvents")
  fits <- list(
    duane     = .recurrentFit(time, counts, "duane", "ls"),
    lsPower   = .recurrentFit(time, counts, "crowAmsaa", "ls"),
    mlePower  = .recurrentFit(time, counts, "crowAmsaa"),
    logLinear = .recurrentFit(time, counts, "logLinear"),
    piecewise = .recurrentFit(time, counts, "piecewise", "ls", breaks = "60")
  )
  for (name in names(fits)) {
    fit <- fits[[name]]
    expect_true(fit$inferenceAvailable, info = name)
    fitted <- unname(jaspSurvival:::.sareFittedCumulativeEvents(fit))
    expect_true(all(is.finite(fitted) & diff(c(0, fitted)) > 0), info = name)
    # Grouped Poisson maximum likelihood reproduces the observed event total at the last endpoint;
    # ReliaGrowR's L-BFGS-B stopping rule reaches it to about 1e-5.
    if (fit$method == "MLE")
      expect_equal(fitted[length(time)], sum(counts), tolerance = 1e-4, info = name)
    for (measure in measures) {
      prediction <- jaspSurvival:::.sarePredict(fit, c(25, 160, 200), measure, 0.95, "twoSided")
      expect_true(all(is.finite(prediction$estimate)), info = paste(name, measure))
      expect_equal(prediction$forecast, c(FALSE, FALSE, TRUE), info = paste(name, measure))
    }
  }
  expect_equal(jaspSurvival:::.sareBreakpoints(fits$piecewise), exp(piecewise$model$psi[, "Est."]), tolerance = 1e-6)
})
