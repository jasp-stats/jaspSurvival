context("Verified: vassiliou2003-2")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/verified/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "verified", "vassiliou2003-2.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 5.87444449225088, 3.78867216105011, "", 1.31456867044104,
     9.10849411762558, "", "scale", 5.97020140664147e-05, 4.65190928586837e-07,
     "", 0.000147877085944578, 0.00766208080285259, "", "Reciprocal(jaspColumn1)",
     5630.32639688547, 3846.59222433453, 6.14785056579467e-10, 910.085178411855,
     7414.06056943642, 6.18659278322792, "Reciprocal(jaspColumn2)",
     0.280598316278434, 0.0914107583958213, 0.00364947956065828,
     0.0965260379144208, 0.469785874161046, 2.90697020556474))

  plotName <- results[["results"]][["meanSurvivalTimePlot"]][["collection"]][["meanSurvivalTimePlot_axis1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1")

  plotName <- results[["results"]][["meanSurvivalTimePlot"]][["collection"]][["meanSurvivalTimePlot_axis2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_jaspcolumn2")

  table <- results[["results"]][["modelEquations"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Model 1", "log(scale) = -9.726 + 5630.326 * (1 / jaspColumn1) + 0.281 * (1 / jaspColumn2)",
     "shape = 5.874"))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(132.48490859125, 134.424535190402, 4, -62.2424542956249))

})

