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
    list("shape", 0.428887988973796, 0.328702326177307, "", 0.0582167562802771,
     0.559609386478039, "", "scale", 7.14678157240494e-35, 1.12191753059386e-127,
     "", 7.79193824671191e-33, 4.55260618101143e+58, "", "log(jaspColumn1)",
     19.9148548119611, -32.5249826761791, 0.456678540611866, 26.7555107653911,
     72.3546923001013, 0.744327214927307, "Arrhenius(jaspColumn2) [eV]",
     5.12658373836023, -1.35675240545485, 0.121187855596068, 3.30788534634045,
     11.6099198821753, 1.54980696172914, "log(jaspColumn1)<unicode><unicode><unicode>Arrhenius(jaspColumn2) [eV]",
     -1.17629385365837, -2.75173818500561, 0.143359980262437, 0.803812898489024,
     0.39915047768887, -1.46339260774431))

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
  jaspTools::expect_equal_tables(table,
    list("mixing probability", 0.888810428873975, 0.824261813200341, 1,
     "", 0.0268830269243999, 0.931617275656212, "", "shape", 0.370047102622451,
     0.21624993567556, "", "", 0.101424119372042, 0.633224965970462,
     "", "scale", 3792095783979852800, 1.46567403857388e-42, "",
     "", 2.6913870740239e+20, 9.81117905920575e+78, "", "Eyring(jaspColumn1, from Celsius)",
     26494.4256319224, -29694.8824965345, "", 0.355401081556959,
     28668.5411424246, 82683.7337603793, 0.924163720096491, "log(jaspColumn2)",
     -24.2185838610017, -38.609600659481, "", 0.000972321579248739,
     7.3424904294129, -9.82756706252236, -3.29841544824978, "Eyring log temperature(jaspColumn1, from Celsius)",
     -1, "", "", "", "", "", "", "mixing probability", 0.111189571126025,
     0.0683827243437881, 2, "", 0.0268830269243999, 0.175738186799659,
     "", "shape", 0.728556838666896, 0.464330608013752, "", "", 0.167448049080286,
     1.14314037887543, "", "scale", 1.31183099843563e+33, 6.33876798501648e+20,
     "", "", 1.89806329024974e+34, 2.71488177596099e+45, "", "Eyring(jaspColumn1, from Celsius)",
     -5969.99455607823, -12816.4811831131, "", 0.0874420574753152,
     3493.16960976788, 876.492070956641, -1.70904800596698, "log(jaspColumn2)",
     -9.9473589683562, -19.737663640057, "", 0.0464363391084535,
     4.99514519089406, -0.157054296655389, -1.99140537225821, "Eyring log temperature(jaspColumn1, from Celsius)",
     -1, "", "", "", "", "", ""))

  plotName <- results[["results"]][["failureProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_predicted-failure-probability")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1084.66655630504, 1135.9488188375, 9, -533.333278152521))

})

