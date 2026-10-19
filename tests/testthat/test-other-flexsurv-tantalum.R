context("Other: flexsurv-tantalum")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv-tantalum.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 0.428887988974971, 0.316418973846135, "", 0.0665507622509396,
        0.581333365857012, "", "scale", 7.14678157235476e-35, 1.12191748142433e-127,
        "", 7.79193824825503e-33, 4.55260638047155e+58, "", "log(jaspColumn1)",
        19.9148548119624, -32.5249836217968, 0.456678548729934, 26.7555112478587,
        72.3546932457217, 0.744327201505309, "Arrhenius(jaspColumn2) [eV]",
        5.12658373836027, -1.35694653812143, 0.121198997362578, 3.30798439543938,
        11.610114014842, 1.54976055673906, "log(jaspColumn1)<unicode><unicode><unicode>Arrhenius(jaspColumn2) [eV]",
        -1.17629385365836, -2.75187113068009, 0.143393751731309,
        0.803880729161192, 0.399283423363376, -1.46326912810282))

  plotName <- results[["results"]][["failureProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_predicted-failure-probability")

  table <- results[["results"]][["modelEquations"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Model 1", "log(scale) = -78.624 + 19.915 * log(jaspColumn1) + 5.127 * (1 / (k<sub>B</sub> * (jaspColumn2 + 273.15))) <unicode> 1.176 * log(jaspColumn1) * (1 / (k<sub>B</sub> * (jaspColumn2 + 273.15)))",
     "shape = 0.429"))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1086.79130484425, 1115.28145069561, 5, -538.395652422124))

})

test_that("ParametricMixtureSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv-tantalum.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  opts[["mixtureStartTails"]] <- TRUE
  opts[["mixtureStartMembershipProbabilities"]] <- "0.95"
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  # Native finite differences vary slightly across platforms. Compare the
  # exponential scale bound on its log scale and the other unstable cells
  # directly; retain the standard comparison for every remaining cell.
  testthat::expect_equal(log(table[[3]][["upper"]]), log(3.17618559052007e+81), tolerance = 1e-5)
  testthat::expect_equal(table[[4]][["pValue"]], 0.379850532216158, tolerance = 2e-5)
  testthat::expect_equal(table[[4]][["upper"]], 85625.0664958501, tolerance = 1e-5)
  jaspTools::expect_equal_tables(table,
    list("mixing probability", 0.888810512719124, 0.824112283637374,
        1, "", 0.0269350845347928, 0.931683095091096, "", "shape",
        0.370047628240013, 0.214805951992426, "", "", 0.102689471416329,
        0.637483486355568, "", "scale", 3796156374304495104, 4.53714142560946e-45,
        "", "", 2.80619655688109e+20, table[[3]][["upper"]], "", "Eyring(jaspColumn1, from Celsius)",
        26493.9806333044, -32637.1052292414, "", table[[4]][["pValue"]],
        30169.475729638, table[[4]][["upper"]], 0.878171727965332, "log(jaspColumn2)",
        -24.2185421899942, -38.8410191511038, "", 0.00116957043585242,
        7.46058451912882, -9.59606522888467, -3.24619902474106, "Eyring log temperature(jaspColumn1, from Celsius)",
        -1, "", "", "", "", "", "", "mixing probability", 0.111189487280876,
        0.0683169049089037, 2, "", 0.0269350845347928, 0.175887716362626,
        "", "shape", 0.728557837577402, 0.462710165660461, "", "",
        0.168748302827761, 1.14714687959754, "", "scale", 1.31188096825968e+33,
        6.27620108384551e+20, "", "", 1.89880209476542e+34, 2.74215509014167e+45,
        "", "Eyring(jaspColumn1, from Celsius)", -5970.02378880529,
        -12929.6481618267, "", 0.0927098143117976, 3550.89401025635,
        989.600584216163, -1.68127344031153, "log(jaspColumn2)",
        -9.94734302415256, -19.7604650832782, "", 0.046947692218027,
        5.00678692901009, -0.134220965026872, -1.98677178901225,
        "Eyring log temperature(jaspColumn1, from Celsius)", -1,
        "", "", "", "", "", ""))

  plotName <- results[["results"]][["failureProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_predicted-failure-probability")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1084.66655630504, 1135.9488188375, 9, -533.333278152521))

})

