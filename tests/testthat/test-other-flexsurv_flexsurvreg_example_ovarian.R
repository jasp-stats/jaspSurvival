context("Other: flexsurv_flexsurvreg_example_ovarian")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_example_ovarian.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", dataset, opts)

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
    list(0, 1, 1, 1, 136, 0.928678047705762, 0.767032204802709, 0.979386642019801, 273, 0.80683792902735,
         0.642037046469453, 0.903695036164804, 409, 0.710108610599981, 0.542205766917912,
         0.83120913097191, 545, 0.635157241578976, 0.46308058974648, 0.777087817007459, 682,
         0.575458029657123, 0.396990720831282, 0.736589578248956, 818, 0.527438974580967,
         0.341652080808536, 0.706877971776755, 954, 0.487655585374727, 0.297382873842918,
         0.68429285339595, 1091, 0.453870591084366, 0.258978549869592, 0.665855111162211, 1227,
         0.425167265983569, 0.226114201193509, 0.649904189988417))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 136, 0.916204713840539, 0.806468771371817, 0.977452452830062, 273,
         0.827444650459192, 0.691029556833121, 0.93051867402636, 409, 0.743457787704606,
         0.583257936944497, 0.873234774533645, 545, 0.665336821820716, 0.476779445277414,
         0.81187347117175, 682, 0.593098702875689, 0.373760065345828, 0.756132901013286, 818,
         0.527819992097417, 0.283080835853873, 0.701558108177778, 954, 0.468729816518349,
         0.206673020202118, 0.649033661835448, 1091, 0.415115040260376, 0.144219501522945,
         0.604311807450317, 1227, 0.367353851176434, 0.0976271531550858, 0.560490228737431))

})

