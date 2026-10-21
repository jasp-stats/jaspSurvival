context("Verified: SurPyval")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/verified/.

test_that("RecurrentEventAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "verified", "SurPyval.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RecurrentEventAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0567437293703143, 0.00520919361967581, "lambda", 0.0691394540659769,
     0.618109261803914, 1.71730903815359, 1.18266640574051, "beta",
     0.326809037084547, 2.49364513797738))

  table <- results[["results"]][["cumulativeEventsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(32.0000000001989, 22.6296256371682, 45.250417149228))

  table <- results[["results"]][["eventIntensityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.37384723053141, 0.82574652959289, 2.28575736645164))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(19.3646824533097, 18.1372711755495, 2, -7.68234122665484))

})

test_that("RecurrentEventAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "verified", "SurPyval.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RecurrentEventAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-1.17719843728017, -2.08986962524394, "a", 0.465657121846525,
     -0.264527249316398, 0.0419647408403258, 0.00865582033849799,
     "b", 0.0169946594756661, 0.0752736613421536))

  table <- results[["results"]][["cumulativeEventsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(32.0000964800744, 22.6297076701509, 45.250525974967))

  table <- results[["results"]][["eventIntensityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.65101656191601, 0.907460628847199, 3.00382804616413))

  table <- results[["results"]][["instantaneousMeanTimeBetweenEventsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.605687443158958, 0.332908536917416, 1.1019761830002))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(19.7047626993898, 18.4773514216296, 2, -7.85238134969492))

})

