context("Nonparametric Survival Analysis")

#### single group ----
options <- analysisOptions("NonParametricSurvivalAnalysis")
options$.meta <- list(eventStatus = list(shouldEncode = TRUE), strata = list(
  shouldEncode = TRUE), timeToEvent = list(shouldEncode = TRUE))
options$colorPalette <- "colorblind"
options$eventIndicator <- "1"
options$eventStatus <- "status"
options$lifeTable <- TRUE
options$survivalCurveCensoringPlot <- TRUE
options$survivalCurveCensoringPlotCumulative <- TRUE
options$survivalCurvePlot <- TRUE
options$survivalCurvePlotCumulativeEventsTable <- TRUE
options$survivalCurvePlotRiskTable <- TRUE
options$timeToEvent <- "time"
options$censoringType <- "right"
set.seed(1)
dataset <- survival::leukemia
results <- runAnalysis("NonParametricSurvivalAnalysis", dataset, options)


test_that("titleless-table-0 results match", {
  table <- results[["results"]][["LifeTableContainer"]][["collection"]][["LifeTableContainer_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(23, 2, 0.804854797782958, 0.0587533847558414, 0.91304347826087,
                                      5, 1, 21, 2, 0.684839468071899, 0.0790341964475118, 0.826086956521739,
                                      8, 0.996466605022982, 19, 1, 0.630957910599588, 0.0860061487037983,
                                      0.782608695652174, 9, 0.970708759207681, 18, 1, 0.579799217968031,
                                      0.0915605371517001, 0.739130434782609, 12, 0.942246527231519,
                                      17, 1, 0.530878341628212, 0.0959438755421514, 0.695652173913043,
                                      13, 0.911568450100481, 14, 1, 0.475257724705514, 0.101142675175285,
                                      0.645962732919255, 18, 0.877982262316863, 13, 2, 0.372078087249211,
                                      0.107250641740122, 0.546583850931677, 23, 0.80293335280237,
                                      11, 1, 0.324016545689816, 0.108401780788683, 0.496894409937888,
                                      27, 0.762010637765041, 9, 1, 0.271675995392292, 0.109518069977657,
                                      0.44168391994479, 30, 0.718078477474975, 8, 1, 0.222515297746886,
                                      0.108858801494864, 0.386473429951691, 31, 0.671242442973631,
                                      7, 1, 0.176520182580705, 0.106390972887551, 0.331262939958592,
                                      33, 0.621657726531294, 6, 1, 0.133822195629273, 0.101983376090492,
                                      0.276052449965493, 34, 0.569449296311515, 5, 1, 0.0947332405633126,
                                      0.0953674384864344, 0.220841959972395, 43, 0.514826379784336,
                                      4, 1, 0.0598407178100809, 0.0860352084085751, 0.165631469979296,
                                      45, 0.458446770885507, 2, 1, 0.0148346082548472, 0.0726618005035283,
                                      0.082815734989648, 48, 0.462327406565294))
})

test_that("Kaplan-Meier Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(18, 18, 27, 23, 36.3643892339544, 9.85410105666456, 45))
})

test_that("Cumulative Censoring Plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_censoringPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-censoring-plot-single")
})

test_that("Cumulative Events Table plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_cumulativeEventsTable"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-events-table-single")
})

test_that("Risk Table plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_riskTable"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "risk-table-single")
})

test_that("Survival Curve plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_surivalCurvePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "survival-curve-single")
})

#### multigroup + test ----
options <- analysisOptions("NonParametricSurvivalAnalysis")
options$.meta <- list(eventStatus = list(shouldEncode = TRUE), strata = list(
  shouldEncode = TRUE), timeToEvent = list(shouldEncode = TRUE))
options$colorPalette <- "grandBudapest"
options$eventIndicator <- "1"
options$eventStatus <- "status"
options$strata <- "x"
options$lifeTable <- TRUE
options$lifeTableStepsType <- "quantiles"
options$survivalCurveCensoringPlot <- TRUE
options$survivalCurvePlot <- TRUE
options$survivalCurvePlotCumulativeEventsTable <- TRUE
options$survivalCurvePlotLegend <- "top"
options$survivalCurvePlotRiskTable <- TRUE
options$testsFlemmingHarrington <- TRUE
options$testsFlemmingHarringtonRho <- 0.25
options$testsLogRank <- TRUE
options$testsPetoAndPeto <- TRUE
options$timeToEvent <- "time"
options$censoringType <- "right"
set.seed(1)
dataset <- survival::leukemia
results <- runAnalysis("NonParametricSurvivalAnalysis", dataset, options)


test_that("x=Maintained table results match", {
  table <- results[["results"]][["LifeTableContainer"]][["collection"]][["LifeTableContainer_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11, 0, 1, 0, 1, 5, 1, 7, 3, 0.488426287422128, 0.139664970557228,
                                      0.715909090909091, 22, 1, 3, 3, 0.154877117897191, 0.162668885827095,
                                      0.368181818181818, 40, 0.875260678143908, 1, 1, 0.0359178984891853,
                                      0.153492745786294, 0.184090909090909, 57, 0.943525769474552,
                                      1, 0, 0.0359178984891853, 0.153492745786294, 0.184090909090909,
                                      74, 0.943525769474552, 1, 0, 0.0359178984891853, 0.153492745786294,
                                      0.184090909090909, 92, 0.943525769474552, 1, 0, 0.0359178984891853,
                                      0.153492745786294, 0.184090909090909, 109, 0.943525769474552,
                                      1, 0, 0.0359178984891853, 0.153492745786294, 0.184090909090909,
                                      126, 0.943525769474552, 1, 0, 0.0359178984891853, 0.153492745786294,
                                      0.184090909090909, 144, 0.943525769474552, 1, 0, 0.0359178984891853,
                                      0.153492745786294, 0.184090909090909, 161, 0.943525769474552
                                 ))
})

test_that("x=Nonmaintained table results match", {
  table <- results[["results"]][["LifeTableContainer"]][["collection"]][["LifeTableContainer_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12, 2, 0.647036987013362, 0.107582870727984, 0.833333333333333,
                                      5, 1, 6, 3, 0.361613705210385, 0.142318760638328, 0.583333333333333,
                                      22, 0.940998012173809, 2, 4, 0.0569215525716042, 0.121874505380446,
                                      0.194444444444444, 40, 0.664223659882563))
})

test_that("Kaplan-Meier Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(7, 18, 31, 11, 52.6454545454545, 19.8286027955626, "x=Maintained",
                                      "", 11, 8, 23, 12, 22.7083333333333, 4.18094198103315, "x=Nonmaintained",
                                      ""))
})

test_that("Censoring Plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_censoringPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "censoring-plot-factor")
})

test_that("Cumulative Events Table plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_cumulativeEventsTable"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-events-table-factor")
})

test_that("Risk Table plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_riskTable"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "risk-table-factor")
})

test_that("Survival Curve plot matches", {
  plotName <- results[["results"]][["surivalPlots"]][["collection"]][["surivalPlots_surivalCurvePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "survival-curve-factor")
})

test_that("Tests Table results match", {
  table <- results[["results"]][["testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.3963886989776, 1, 0.0653393220405051, "Log-rank (Mantel-Haenszel)",
                                      2.77927954475177, 1, 0.0954911154064931, "Peto and Peto", 3.19256000777338,
                                      1, 0.0739740829120213, "Flemming-Harrington"))
})
