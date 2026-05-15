context("Library: Leukemia")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("NonParametricSurvivalAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Leukemia.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("NonParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["LifeTableContainer"]][["collection"]][["LifeTableContainer_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(11, 1, 0.754133845081525, 0.0866784172041448, 0.909090909090909,
     9, 1, 10, 1, 0.619248987399364, 0.116291299830333, 0.818181818181818,
     13, 1, 8, 1, 0.488426287422128, 0.139664970557228, 0.715909090909091,
     18, 1, 7, 1, 0.37686705950168, 0.152632331027312, 0.613636363636364,
     23, 0.999157600228473, 5, 1, 0.254859951199317, 0.164193267221136,
     0.490909090909091, 31, 0.945584955200429, 4, 1, 0.154877117897191,
     0.162668885827095, 0.368181818181818, 34, 0.875260678143908,
     2, 1, 0.0359178984891853, 0.153492745786294, 0.184090909090909,
     48, 0.943525769474552))

  table <- results[["results"]][["LifeTableContainer"]][["collection"]][["LifeTableContainer_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(12, 2, 0.647036987013362, 0.107582870727984, 0.833333333333333,
     5, 1, 10, 2, 0.446846081150264, 0.136082763487954, 0.666666666666667,
     8, 0.994625360259092, 8, 1, 0.361613705210385, 0.142318760638328,
     0.583333333333333, 12, 0.940998012173809, 6, 1, 0.267518248858267,
     0.148130062553483, 0.486111111111111, 23, 0.883319225339558,
     5, 1, 0.185396526095557, 0.146986183948033, 0.388888888888889,
     27, 0.815735715691272, 4, 1, 0.11483115015247, 0.138715169134987,
     0.291666666666667, 30, 0.740822018515804, 3, 1, 0.0569215525716042,
     0.121874505380446, 0.194444444444444, 33, 0.664223659882563,
     2, 1, 0.0152565271708652, 0.0918663649675205, 0.0972222222222222,
     43, 0.619548629119056, 1, 1, "", "NaN", 0, 45, ""))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 18, 31, 11, 52.6454545454545, 19.8286027955626, "jaspColumn2=Maintained",
     "", 11, 8, 23, 12, 22.7083333333333, 4.18094198103315, "jaspColumn2=Nonmaintained",
     ""))

  plotName <- results[["results"]][["surivalPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_survival-plot")

  table <- results[["results"]][["testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3.3963886989776, 1, 0.0653393220405051, "Log-rank (Mantel-Haenszel)",
     2.77927954475177, 1, 0.0954911154064931, "Peto and Peto", 2.77927954475177,
     1, 0.0954911154064931, "Flemming-Harrington"))

})

