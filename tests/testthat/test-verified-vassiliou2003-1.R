context("Verified: vassiliou2003-1")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/verified/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "verified", "vassiliou2003-1.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 4.2915822230105, 3.26359179265917, "", 0.599578151105757,
     5.64337672937124, "", "scale", 58.9848686965314, 3.51992857757795,
     "", 84.8326414632811, 988.433332798213, "", "Reciprocal(jaspColumn1)",
     1861.61866572779, 713.133113625267, 0.00148821807158453, 585.972783766249,
     3010.10421783031, 3.17697121317227))

  table <- results[["results"]][["modelEquations"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Model 1", "log(scale) = 4.077 + 1861.619 * (1 / jaspColumn1)",
     "shape = 4.292"))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(522.276293326997, 526.479885471983, 3, -258.138146663499))

})

