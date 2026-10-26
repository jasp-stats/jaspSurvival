context("Verified: vassiliou2003-3")

# Source: Vassiliou & Mettas (2003), Understanding Accelerated Life-Testing Analysis, #2003RM-100.
# https://www.tce.edu/sites/default/files/PDF/Mech-Reliability-Engg.pdf
# Printed p. 13, Figures 21-22 (paper clips): lognormal SD = 0.1459, K = 1.1264e-5, n = 1.8740. Here log-scale intercept = -log(K), log-stress slope = -n.
# Published rounded point estimates are the external anchors; the higher-precision
# estimates and confidence limits below are regression expectations from the fixture.

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/verified/.

test_that("ParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "verified", "vassiliou2003-3.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("meanlog", 11.3938554201218, 10.2367214569627, "", 0.590385319468305,
     12.5509893832808, "", "sdlog", 0.145880161227606, 0.0977789935233262,
     "", 0.0297776434406233, 0.217644104044857, "", "log(jaspColumn1)",
     -1.87400146881524, -2.11215566457571, 1.15226107777017e-53,
     0.121509475500059, -1.63584727305478, -15.4226776233129))

  table <- results[["results"]][["customPredictionsResult"]][["collection"]][["customPredictionsResult_tables"]][["collection"]][["customPredictionsResult_tables_meanTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(71.5792218144107, 55.5657083225687, 93.1966217684472, 45, 1))

  table <- results[["results"]][["modelEquations"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("meanlog = 11.394 <unicode> 1.874 * log(jaspColumn1)", "Model 1",
     "sdlog = 0.146"))

})

