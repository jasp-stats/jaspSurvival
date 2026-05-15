context("Other: coxph_full_output")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("SemiParametricSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "coxph_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SemiParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.5, 0, "H<unicode>", 0.5, 0, "H<unicode>"))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(69.9698807423251, 69.9698807423251, 0, -34.9849403711626, "H<unicode>",
     69.9698807423251, 69.9698807423251, 0, -34.9849403711626, "H<unicode>"
    ))

  plotName <- results[["results"]][["surivalPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_survival-plot")

})

test_that("SemiParametricSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "coxph_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SemiParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.161619857406316, 0.0641310047945991, "H<unicode>", "jaspColumn1",
     0.00115695361003499, 0.0497401245026422, 0.259108710018033,
     3.24928534100736))

  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.5, 0, "H<unicode>", 0.784403669724771, 0.0826697943707723, "H<unicode>"
    ))

  plotName <- results[["results"]][["proportionalHazardsPlots"]][["collection"]][["proportionalHazardsPlots_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_jaspcolumn1")

  table <- results[["results"]][["proportionalHazardsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.69143093228958, 1, 0.405678093672719, "jaspColumn1", 0.69143093228958,
     1, 0.405678093672719, "Global"))

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_residuals-histogram")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsPredicted"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_residuals-vs-predicted")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-4_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsTime"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-5_residuals-vs-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(69.9698807423251, 69.9698807423251, 0, -34.9849403711626, "H<unicode>",
     57.6762945821649, 58.1612012319529, 1, -27.8381472910825, "H<unicode>"
    ))

  plotName <- results[["results"]][["surivalPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-6_survival-plot")

  table <- results[["results"]][["testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(14.2935861601602, 1, 0.000156396864322538, "Likelihood ratio",
     10.56, 1, 0.00115695361003499, "Wald", 12.2594057581327, 1,
     0.000462918900551385, "Score"))

})

test_that("SemiParametricSurvivalAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "coxph_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SemiParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.123879936963637, 0.00658864744672992, "H<unicode>", "jaspColumn1",
     0.0384465599789823, 0.0598435942915716, 0.241171226480544, 2.07006177403158
    ))

  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.5, 0, "H<unicode>", 0.727272727272727, 0.0801742821534724, "H<unicode>"
    ))

  plotName <- results[["results"]][["proportionalHazardsPlots"]][["collection"]][["proportionalHazardsPlots_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_jaspcolumn1")

  table <- results[["results"]][["proportionalHazardsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.12442304908746, 1, 0.288968046598394, "jaspColumn1", 1.12442304908746,
     1, 0.288968046598394, "Global"))

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_residuals-histogram")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsPredicted"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_residuals-vs-predicted")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsTime"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_residuals-vs-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(69.9698807423251, 69.9698807423251, 0, -34.9849403711626, "H<unicode>",
     36.8843005777971, 37.3692072275851, 1, -17.4421502888985, "H<unicode>"
    ))

  plotName <- results[["results"]][["surivalPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-6_survival-plot")

  table <- results[["results"]][["testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.09012841352668, 1, 0.0240624098359963, "Likelihood ratio", 4.29,
     1, 0.0384465599789823, "Wald", 4.70976604255119, 1, 0.0299917329389107,
     "Score"))

})

test_that("SemiParametricSurvivalAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "coxph_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SemiParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.062318329555653, -0.0525937416460908, "H<unicode>", "jaspColumn1",
     0.291043368786259, 0.0586296850902137, 0.0590227630983666, 0.177230400757397,
     1.05583551640566))

  table <- results[["results"]][["frailtyTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(44.913746237973, 7.99009861923082, 3.78526719796018e-07, "jaspColumn3",
     6.10630228739894))

  table <- results[["results"]][["hazardRatioTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.06430108930181, 0.948034712927676, "H<unicode>", "jaspColumn1",
     1.19482630039036))

  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.967889908256881, 0.00811146577439577, "H<unicode>", 0.967889908256881,
     0.0149858635085453, "H<unicode>"))

  plotName <- results[["results"]][["proportionalHazardsPlots"]][["collection"]][["proportionalHazardsPlots_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_jaspcolumn1")

  table <- results[["results"]][["proportionalHazardsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0195685732474866, 0.986724813170825, 0.885250200704232, "jaspColumn1",
     0.0195685732474866, 8.97682343240164, 0.999999999981531, "Global"
    ))

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_residuals-histogram")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsPredicted"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-3_residuals-vs-predicted")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-4_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsPlots"]][["collection"]][["residualsPlots_residualPlotResidualVsTime"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-5_residuals-vs-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(40.4496091964594, 44.4249273993426, 8.19811030560462, -12.0266942926251,
     "H<unicode>", 44.1391919021048, 48.4921132784491, 8.97682343240164,
     -13.0927725186507, "H<unicode>"))

  plotName <- results[["results"]][["surivalPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-6_survival-plot")

  table <- results[["results"]][["testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(43.7843357050237, 8.97682343240164, 1.51557952374564e-06, "Likelihood ratio"
    ))

})

