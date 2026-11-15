test_that("Sequential ties retain every failure's plotting position", {
  options <- list(probabilityPlotRankAdjustment = "johnson", probabilityPlotTies = "sequential",
    probabilityPlotPlottingPosition = "median")
  time <- rep(1:5, each = 2)
  points <- jaspSurvival:::.sapProbabilityPlotRankData(time, rep(TRUE, 10), options)
  expect_equal(points$time, time)
  expect_equal(points$probability, stats::qbeta(0.5, 1:10, 11 - (1:10)))
  # The median of the largest order statistic solves p^10 = 1/2.
  expect_equal(tail(points$probability, 1), 0.5^(1 / 10))

  allTied <- jaspSurvival:::.sapProbabilityPlotRankData(rep(1, 10), rep(TRUE, 10), options)
  expect_equal(allTied$probability, points$probability)
})

test_that("Sequential ties preserve censoring-adjusted ranks across plotting methods", {
  time <- c(1, 1, 1, 2, 3, 3, 4)
  event <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  # Johnson's recursion gives 1, 2, 3.5, 5, 6.5 after the two withdrawals.
  # KM failure probabilities are 1/7, 2/7, 11/21, 16/21, then 1 - (5/21)/10.
  ranks <- list(johnson = c(1, 2, 3.5, 5, 6.5), kaplanMeier = c(1, 2, 11/3, 16/3, 41/6))
  for (adjustment in names(ranks)) {
    rank <- ranks[[adjustment]]
    expected <- list(median = stats::qbeta(0.5, rank, 8 - rank),
      benard = (rank - 0.3) / 7.4, hazen = (rank - 0.5) / 7,
      mean = rank / 8, kaplanMeier = rank / 7, blom = (rank - 0.375) / 7.25)
    for (method in names(expected)) {
      options <- list(probabilityPlotRankAdjustment = adjustment, probabilityPlotTies = "sequential",
        probabilityPlotPlottingPosition = method)
      points <- jaspSurvival:::.sapProbabilityPlotRankData(time, event, options)
      expect_equal(points$time, time[event])
      expect_equal(points$probability, expected[[method]])
    }
  }
})

test_that("Sequential probability points expand frequency weights into units", {
  options <- list(timeToEvent = "time", eventStatus = "event", weights = "frequency", censoringType = "right",
    probabilityPlotRankAdjustment = "johnson", probabilityPlotTies = "sequential",
    probabilityPlotPlottingPosition = "median")
  data <- data.frame(time = 1:5, event = TRUE, frequency = 2L)
  points <- jaspSurvival:::.sapProbabilityPlotEmpiricalData(data, options)
  expect_equal(points$time, rep(1:5, each = 2))
  expect_equal(points$probability, stats::qbeta(0.5, 1:10, 11 - (1:10)))
})

test_that("Other tie handlers retain their selected ranks", {
  options <- list(probabilityPlotRankAdjustment = "johnson", probabilityPlotPlottingPosition = "mean")
  expected <- list(none = 1:10, sequential = 1:10, highest = seq(2, 10, 2),
    lowest = seq(1, 9, 2), mean = seq(1.5, 9.5, 2))
  for (handler in names(expected)) {
    options$probabilityPlotTies <- handler
    points <- jaspSurvival:::.sapProbabilityPlotRankData(rep(1:5, each = 2), rep(TRUE, 10), options)
    expect_equal(points$probability, expected[[handler]] / 11)
    untied <- jaspSurvival:::.sapProbabilityPlotRankData(1:10, rep(TRUE, 10), options)
    expect_equal(untied$probability, (1:10) / 11)
  }
})
