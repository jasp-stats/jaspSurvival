test_that("Membership probabilities retain the original partitions and random draws", {
  options <- jaspTools::analysisOptions("ParametricMixtureSurvivalAnalysis")
  options[["setSeed"]] <- TRUE
  options[["seed"]] <- 1
  family <- jaspSurvival:::.sapmFamily("weibull")
  survObject <- survival::Surv(seq_len(60), rep(1, 60))

  for (components in 2:4) {
    previous <- list(
      parameters = rep(list(list(shape = 2, scale = 20)), components - 1),
      posterior = matrix(1 / (components - 1), 60, components - 1)
    )
    options[["mixtureStartMembershipProbabilities"]] <- "0.95"
    softened <- jaspSurvival:::.sapmStarts(options, family, survObject, survObject, components, previous)
    seed <- .Random.seed
    options[["mixtureStartMembershipProbabilities"]] <- "0.95, 0.99"
    paired <- jaspSurvival:::.sapmStarts(options, family, survObject, survObject, components, previous)

    original <- !grepl("Probability2$", vapply(paired, `[[`, character(1), "name"))
    expect_identical(paired[original], softened)
    expect_identical(.Random.seed, seed)
    expect_length(paired, if (components == 4) 32 else 30)
    expect_true(all(vapply(paired, function(start) {
      posterior <- start[["posterior"]]
      all(posterior > 0) && all(abs(rowSums(posterior) - 1) < 1e-12)
    }, logical(1))))

    options[["mixtureStartMembershipProbabilities"]] <- "0.9, 0.95, 0.99"
    triple <- jaspSurvival:::.sapmStarts(options, family, survObject, survObject, components, previous)
    expect_length(triple, 3 * length(softened))
    expect_identical(.Random.seed, seed)
    expect_equal(triple[[1]][["posterior"]][1, ],
                 ifelse(paired[[1]][["posterior"]][1, ] > 0.5, 0.9, 0.1 / (components - 1)))
  }
})

test_that("Membership probabilities require valid distinct values", {
  options <- jaspTools::analysisOptions("ParametricMixtureSurvivalAnalysis")
  options[["mixtureStartMembershipProbabilities"]] <- "0.95, 0.99"
  expect_equal(jaspSurvival:::.sapmStartMembershipProbabilities(options), c(0.95, 0.99))
  for (input in c("", "NA, 0.95", "Inf, 0.95", "foo, 0.95",
                  "0.5", "1", "0.49, 0.95", "0.95, 0.95")) {
    options[["mixtureStartMembershipProbabilities"]] <- input
    expect_error(jaspSurvival:::.sapmStartMembershipProbabilities(options), "Membership probabilities")
  }
  options[["mixtureStartMembershipProbabilities"]] <- 0.95
  expect_equal(jaspSurvival:::.sapmStartMembershipProbabilities(options), 0.95)

  options[["mixtureStartMembershipProbabilities"]] <- "0.999999999999999"
  family <- jaspSurvival:::.sapmFamily("weibull")
  survObject <- survival::Surv(seq_len(60), rep(1, 60))
  starts <- jaspSurvival:::.sapmStarts(options, family, survObject, survObject, 2, NULL)
  expect_true(all(vapply(starts, function(start) all(start[["posterior"]] > 0), logical(1))))
})

test_that("Equal-sized groups and tail partitions can be selected independently", {
  options <- jaspTools::analysisOptions("ParametricMixtureSurvivalAnalysis")
  for (name in c("mixtureStartKmeans", "mixtureStartSplit", "mixtureStartRandom"))
    options[[name]] <- FALSE
  options[["mixtureStartMembershipProbabilities"]] <- "0.95"
  family <- jaspSurvival:::.sapmFamily("weibull")
  survObject <- survival::Surv(seq_len(60), rep(1, 60))

  options[["mixtureStartTails"]] <- FALSE
  equal <- jaspSurvival:::.sapmStarts(options, family, survObject, survObject, 2, NULL)
  expect_identical(vapply(equal, `[[`, character(1), "name"), "quantiles")
  options[["mixtureStartQuantiles"]] <- FALSE
  options[["mixtureStartTails"]] <- TRUE
  tails <- jaspSurvival:::.sapmStarts(options, family, survObject, survObject, 2, NULL)
  expect_identical(vapply(tails, `[[`, character(1), "name"), c("tails1", "tails2"))
})

test_that("Sharper split starts preserve censoring probabilities and other components", {
  family <- jaspSurvival:::.sapmFamily("weibull")
  previous <- list(
    parameters = list(list(shape = 2, scale = 10), list(shape = 2, scale = 30)),
    posterior = matrix(rep(c(0.25, 0.75), each = 3), 3, 2)
  )
  time <- stats::qweibull(c(0.25, 0.4, 0.6), shape = 2, scale = 10)

  for (type in c("right", "counting", "interval")) {
    survObject <- switch(type,
      "right" = survival::Surv(time, c(1, 0, 0)),
      "counting" = survival::Surv(rep(0, 3), time, c(1, 0, 0)),
      "interval" = survival::Surv(c(time[1], NA, NA), time, type = "interval2")
    )
    softened <- jaspSurvival:::.sapmSplitPosterior(family, survObject, previous, 1)
    sharper <- jaspSurvival:::.sapmSplitPosterior(family, survObject, previous, 1, softness = 0.001)
    expect_equal(sharper[, 3], previous[["posterior"]][, 2])
    expect_equal(rowSums(sharper[, 1:2]), previous[["posterior"]][, 1])
    uncertain <- if (type == "interval") 3 else 2
    expect_equal(sharper[uncertain, ], softened[uncertain, ])
    expect_gt(sharper[1, 1], softened[1, 1])
  }
})

test_that("Default sharper starts recover the compact component in the lung example", {
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  options <- jaspTools::analysisOptions(jaspFile)[[1]]
  options[["mixtureStartTails"]] <- TRUE
  options[["mixtureStartMembershipProbabilities"]] <- "0.95, 0.99"
  for (name in c("survivalProbabilityPlot", "probabilityPlot", "mixtureComponentPlot",
                 "survivalProbabilityTable", "mixtureComponentsTable", "mixtureClassificationTable"))
    options[[name]] <- FALSE
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)
  encoded <- jaspTools:::encodeOptionsAndDataset(options, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options,
                                   encodedDataset = TRUE, view = FALSE)
  expect_identical(results[["status"]], "complete")

  findMixture <- function(object) {
    if (inherits(object, "flexsurvreg") && !is.null(attr(object, "mixture")))
      return(object)
    if (is.list(object)) for (child in object) {
      fit <- findMixture(child)
      if (!is.null(fit))
        return(fit)
    }
    return(NULL)
  }
  fit <- findMixture(results[["state"]][["other"]])
  expect_s3_class(fit, "flexsurvreg")
  expect_equal(fit[["loglik"]], -1146.572853, tolerance = 1e-6, scale = 1)
  expect_equal(stats::AIC(fit), 2303.145706, tolerance = 1e-5, scale = 1)
  expect_equal(stats::BIC(fit), 2320.292434, tolerance = 1e-5, scale = 1)

  estimates <- fit[["res"]][, "est"]
  probability <- estimates[["v1"]]
  time <- dataset[["time"]]
  density <- probability * stats::dweibull(time, estimates[["shape1"]], estimates[["scale1"]]) +
    (1 - probability) * stats::dweibull(time, estimates[["shape2"]], estimates[["scale2"]])
  survival <- probability * stats::pweibull(time, estimates[["shape1"]], estimates[["scale1"]], lower.tail = FALSE) +
    (1 - probability) * stats::pweibull(time, estimates[["shape2"]], estimates[["scale2"]], lower.tail = FALSE)
  independent <- sum(ifelse(as.character(dataset[["status"]]) == "2", log(density), log(survival)))
  expect_equal(fit[["loglik"]], independent, tolerance = 1e-7, scale = 1)
  expect_equal(attr(fit, "mixture")[["starts"]], 30)
  expect_gt(attr(fit, "mixture")[["minEvents"]], 5)
  expect_length(jaspSurvival:::.sapmFitMessages(fit), 0)
})
