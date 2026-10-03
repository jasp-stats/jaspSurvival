context("Other: flexsurv_flexsurvreg_example_ovarian")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_example_ovarian.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE, view = FALSE)
  expect_identical(results[["status"]], "complete")

  table <- results[["results"]][["censoringSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(12, "Events", 14, "Censored"))

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("mu", "Generalized gamma", 6.42623862566761, 4.98415324004556,
     0.735771369778749, 7.86832401128966, "sigma", "", 1.42618378985483,
     0.887565055810066, 0.345110552370915, 2.29166322978801, "Q",
     "", -0.766107560580138, -3.33964742462226, 1.31305467056634,
     1.80743230346199, "shape", "Weibull", 1.10805973956938, 0.674053646122464,
     0.281009159375663, 1.82151137897932, "scale", "", 1225.41895892538,
     690.421182604192, 358.714386981839, 2174.97907470001))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(199.898133347768, 203.672422961833, 0.348823674318625, 3, "Generalized gamma",
     -96.9490666738842, 1, 199.907802094262, 202.423995170304, 0.651176325681375,
     2, "Weibull", -97.9539010471308, 2))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  # The KM curve starts at survival one; its CI starts at the first event.
  time <- dataset[[opts[["timeToEvent"]]]]
  event <- dataset[[opts[["eventStatus"]]]] == opts[["eventIndicator"]]
  km <- summary(survival::survfit(survival::Surv(time, event) ~ 1,
    conf.int = opts[["predictionsConfidenceIntervalLevel"]]))
  kmLayers <- Filter(function(layer) identical(layer$aes_params$colour, "grey60"), testPlot$layers)
  kmRibbon <- Filter(function(layer) inherits(layer$geom, "GeomRibbon"), kmLayers)[[1]]$data
  kmCurve <- Filter(function(layer) !inherits(layer$geom, "GeomRibbon"), kmLayers)[[1]]$data
  expect_equal(kmCurve$at[1], 0)
  expect_equal(kmCurve$estimate[1], 1)
  expect_equal(max(kmCurve$at), max(time))
  expect_equal(min(kmRibbon$at), min(time[event]))
  for (column in c("estimate", "lCi", "uCi")) {
    expected <- km[[switch(column, estimate = "surv", lCi = "lower", uCi = "upper")]]
    actual <- vapply(km$time, function(at) tail(kmCurve[[column]][kmCurve$at == at], 1), numeric(1))
    expect_equal(actual, expected)
  }

  # Adaptive sampling must retain accurate curves, regardless of node count.
  denseTimes <- seq(0, max(time), length.out = 5001)
  distributions <- c("Generalized gamma" = "gengamma", "Weibull" = "weibull")
  for (label in names(distributions)) {
    series <- testPlot$data[testPlot$data$Distribution == label, ]
    nativeFit <- flexsurv::flexsurvreg(survival::Surv(time, event) ~ 1, dist = distributions[[label]])
    expected <- summary(nativeFit, t = denseTimes, ci = FALSE)[[1]]$est
    actual <- stats::approx(series$at, series$estimate, xout = denseTimes)$y
    expect_equal(series$estimate, summary(nativeFit, t = series$at, ci = FALSE)[[1]]$est)
    expect_equal(range(series$at), range(denseTimes))
    expect_true(all(is.finite(actual)))
    expect_lte(max(abs(actual - expected)), 0.001 * diff(range(testPlot$data$estimate)))
  }

  # Structural snapshot column ordering must not depend on the system locale.
  withr::local_collate("C")
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 136, 0.928678047705762, 0.763339165139693, 0.978948022843315,
     273, 0.80683792902735, 0.63994046270467, 0.90281410036538, 409,
     0.710108610599981, 0.53537645776529, 0.830149615274622, 545,
     0.635157241578976, 0.451745826004409, 0.775552794067485, 682,
     0.575458029657123, 0.383228893135449, 0.735371331723286, 818,
     0.527438974580967, 0.32921732428551, 0.706564300053653, 954,
     0.487655585374727, 0.284317260190482, 0.684564077005306, 1091,
     0.453870591084366, 0.245884063743583, 0.667362366026532, 1227,
     0.425167265983569, 0.215674333450132, 0.652046073301113))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 136, 0.916204713840539, 0.806703678088237, 0.977498076984024,
     273, 0.827444650459192, 0.691685555961959, 0.929864919862331,
     409, 0.743457787704606, 0.583045510293072, 0.870946573479792,
     545, 0.665336821820716, 0.475666195224561, 0.810125852273701,
     682, 0.593098702875689, 0.370802449703617, 0.751140441988808,
     818, 0.527819992097417, 0.279039303003481, 0.69794663706953,
     954, 0.468729816518349, 0.206566125348961, 0.649644172170843,
     1091, 0.415115040260376, 0.144062425796122, 0.605221516857055,
     1227, 0.367353851176434, 0.0971060945454123, 0.562425168903064
    ))

})

