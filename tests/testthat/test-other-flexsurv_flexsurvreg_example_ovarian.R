context("Other: flexsurv_flexsurvreg_example_ovarian")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_example_ovarian.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  opts[["predictionsOutputTableFormat"]] <- "long"
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
    list(0, 1, 1, 1, 136, 0.928678047705762, 0.771899576899099, 0.979484038118394,
     273, 0.80683792902735, 0.641134701174093, 0.902720278963601,
     409, 0.710108610599981, 0.542299225518397, 0.828342915265842,
     545, 0.635157241578976, 0.455252212545974, 0.776369137021891,
     682, 0.575458029657123, 0.387543267518244, 0.737247361919975,
     818, 0.527438974580967, 0.330890458445671, 0.707134598660413,
     954, 0.487655585374727, 0.286097453563909, 0.684466518359933,
     1091, 0.453870591084366, 0.246378084850874, 0.666708621843998,
     1227, 0.425167265983569, 0.216201325114957, 0.650450127958844
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 136, 0.916204713840539, 0.807062306329637, 0.976285662535547,
     273, 0.827444650459192, 0.69242772944821, 0.927439447942998,
     409, 0.743457787704606, 0.584498780527557, 0.868370174797729,
     545, 0.665336821820716, 0.480222219221574, 0.807995411204584,
     682, 0.593098702875689, 0.375126971401457, 0.752415316659072,
     818, 0.527819992097417, 0.283689088652985, 0.699022652724138,
     954, 0.468729816518349, 0.206906595988978, 0.64726583147863,
     1091, 0.415115040260376, 0.144219501522945, 0.604405499783686,
     1227, 0.367353851176434, 0.0994263824327922, 0.565206069164915
    ))

})

