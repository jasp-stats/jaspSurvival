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
    list("mixing probability", 0.888810512306414, 0.824112336090354,
        1, "", 0.0269350661654628, 0.931683071526783, "", "shape",
        0.370047630871679, 0.21480654351582, "", "", 0.102688953571696,
        0.637481739952941, "", "scale", 3796160927625131008, 4.54085414510787e-45,
        "", "", 2.80618410333824e+20, 3.17359627240023e+81, "", "Eyring(jaspColumn1, from Celsius)",
        26493.9802449735, -32636.427020772, "", 0.379845070832662,
        30169.1295004187, 85624.3875107189, 0.878181793233571, "log(jaspColumn2)",
        -24.2185422452216, -38.8409363124101, "", 0.00116949480044809,
        7.46054222553484, -9.59614817803297, -3.24621743475024, "Eyring log temperature(jaspColumn1, from Celsius)",
        -1, "", "", "", "", "", "", "mixing probability", 0.111189487693586,
        0.0683169284732168, 2, "", 0.0269350661654628, 0.175887663909646,
        "", "shape", 0.728557834421907, 0.462710950189456, "", "",
        0.168747670233025, 1.1471449246667, "", "scale", 1.31188154181586e+33,
        6.27657208198014e+20, "", "", 1.89879899771162e+34, 2.74199540334796e+45,
        "", "Eyring(jaspColumn1, from Celsius)", -5970.02382796104,
        -12929.6278417948, "", 0.0927088572955676, 3550.88362272483,
        989.580185872749, -1.6812783696301, "log(jaspColumn2)", -9.94734309471771,
        -19.7604461052388, "", 0.0469472630865857, 5.0067772101557,
        -0.134240084196611, -1.98677565970793, "Eyring log temperature(jaspColumn1, from Celsius)",
        -1, "", "", "", "", "", ""))

  plotName <- results[["results"]][["failureProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_predicted-failure-probability")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1084.66655630504, 1135.9488188375, 9, -533.333278152521))

})

