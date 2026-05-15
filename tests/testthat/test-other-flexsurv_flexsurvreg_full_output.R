context("Other: flexsurv_flexsurvreg_full_output")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["censoringSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 1, "Events", 5, 2, "", 6, 1, "Censored", 8, 2, ""))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(83.5344037495967, 85.2292518219813, 0.999971259110785, 3, -38.7672018747983,
     1, 2, 104.448725742629, 106.143573815014, 2.87408892148436e-05,
     3, -49.2243628713144, 2, 1))

})

