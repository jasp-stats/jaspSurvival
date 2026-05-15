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
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

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
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 136, 0.928678047705762, 0.761197702931884, 0.980919878754199,
     273, 0.80683792902735, 0.637173336322112, 0.90427299181079,
     409, 0.710108610599981, 0.531191710426655, 0.828381277695541,
     545, 0.635157241578976, 0.443526116871654, 0.775631670746246,
     682, 0.575458029657123, 0.372683496901092, 0.734827796810965,
     818, 0.527438974580967, 0.323640336402642, 0.706842866157065,
     954, 0.487655585374727, 0.274909358449137, 0.686983639256401,
     1091, 0.453870591084366, 0.232408474769928, 0.668583003341069,
     1227, 0.425167265983569, 0.206376465015251, 0.65524243469261
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 136, 0.916204713840539, 0.804350622375206, 0.979411496142997,
     273, 0.827444650459192, 0.687114876334917, 0.932103474747157,
     409, 0.743457787704606, 0.583139304624973, 0.875066256836907,
     545, 0.665336821820716, 0.475622750199463, 0.807837577579192,
     682, 0.593098702875689, 0.372475565843616, 0.750685596705045,
     818, 0.527819992097417, 0.274125574683402, 0.696164733826608,
     954, 0.468729816518349, 0.188343724831817, 0.645939666218266,
     1091, 0.415115040260376, 0.138316641479275, 0.604309406356124,
     1227, 0.367353851176434, 0.091951277302095, 0.565295654887753
    ))

})

