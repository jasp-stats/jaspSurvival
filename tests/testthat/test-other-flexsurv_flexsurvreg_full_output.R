context("Other: flexsurv_flexsurvreg_full_output")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["censoringSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 1, "Events", 5, 2, "", 6, 1, "Censored", 8, 2, ""))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 0.00294583404147664, -0.195092282202688, 0.101399586243753,
     "scale", -0.0284318665535529, 1.84744099223018, -0.195092282202688,
     "scale(jaspColumn1)", 0.000449368523120733, -0.0284318665535529,
     0.00294583404147664))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 0.00753960491518431, -0.472986932576419, 0.116539009633363,
     "scale", -0.379053598831464, 22.8507010819253, -0.472986932576419,
     "scale(jaspColumn1)", 0.00628895179079035, -0.379053598831464,
     0.00753960491518431))

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 1.73216181581721, 0.927978224300617, "", 0.551577512747128,
     1, 3.2332488819298, "", "scale", 46376.8295087911, 3231.04344408301,
     "", 63035.6656423456, "", 665670.503201136, "", "jaspColumn1",
     -0.0745258521170902, -0.116073784398787, 0.000438678956616039,
     0.0211983141575158, "", -0.0329779198353937, -3.51564995043096,
     "shape", 1.99218807760198, 1.02034905849015, "", 0.680089387992485,
     2, 3.88966236947607, "", "scale", 12246469.3772606, 1044.87092896917,
     "", 58541071.2613659, "", 143535443517.547, "", "jaspColumn1",
     -0.159479574128679, -0.314910424765391, 0.0443235669184763,
     0.0793029116160961, "", -0.00404872349196797, -2.01101789176061
    ))

  plotName <- results[["results"]][["cumulativeHazardPlot"]][["collection"]][["cumulativeHazardPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_subgroup-1")

  plotName <- results[["results"]][["cumulativeHazardPlot"]][["collection"]][["cumulativeHazardPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_subgroup-2")

  plotName <- results[["results"]][["failureProbabilityPlot"]][["collection"]][["failureProbabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_subgroup-1")

  plotName <- results[["results"]][["failureProbabilityPlot"]][["collection"]][["failureProbabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_subgroup-2")

  plotName <- results[["results"]][["hazardPlot"]][["collection"]][["hazardPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_subgroup-1")

  plotName <- results[["results"]][["hazardPlot"]][["collection"]][["hazardPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_subgroup-2")

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 123, 0.04589999700458,
     0.00408826446946746, 0.180491726703416, 0.0448625259766594,
     0.00407991889050855, 0.165140412284832, 0.000646392050060632,
     9.69468082832524e-05, 0.00165057785824674, 120.962320563707,
     112.634535424581, 122.876564610606, 246, 0.152491389279767,
     0.0307453764696454, 0.402829397636041, 0.141433713201337, 0.0302775441469985,
     0.331573875317287, 0.00107373886890784, 0.000337011826244639,
     0.00260253837277186, 232.887788012382, 205.763615995662, 244.047509894123,
     369, 0.30779664106479, 0.0930929908040921, 0.745447691065574,
     0.264935215197203, 0.0888912283037621, 0.525478184354333, 0.00144486067395454,
     0.000569291620886698, 0.00434569019301614, 331.072114221693,
     278.980255946224, 359.874205207095, 492, 0.506614930762481,
     0.187570532878118, 1.37778345962911, 0.397468257743826, 0.171029353294657,
     0.747863194035565, 0.00178361593168628, 0.00073262514685074,
     0.00699710286624925, 413.349433348712, 329.373788535848, 466.338265165169,
     614, 0.743562729610374, 0.303724316131358, 2.40625093811026,
     0.524582887932561, 0.26193568724437, 0.90984734407642, 0.00209767258614968,
     0.000817919096842538, 0.0104107428577564, 479.007552945456,
     360.28947013801, 560.28660559761, 737, 1.02017931840542, 0.439882815112907,
     3.92216002710425, 0.639469715339462, 0.355888102840515, 0.980201715931183,
     0.00239771460058114, 0.000852377347548663, 0.0141775279251266,
     530.250196899194, 372.274368322873, 643.146380075617, 860, 1.33286139825671,
     0.577693144572279, 5.96210615018442, 0.736278431789546, 0.43880853758179,
     0.997425501926139, 0.00268457165097327, 0.000870979694544984,
     0.019117587051529, 568.44391469234, 377.001010691909, 713.442626400309,
     983, 1.68014111584834, 0.724901086701753, 8.55152890248938,
     0.813652322426719, 0.515627522081259, 0.999806749957389, 0.00296060659822688,
     0.000885881958156619, 0.0244956126597056, 595.925825111859,
     378.334887857047, 774.88475056023, 1106, 2.06079708851964, 0.864555704835048,
     11.8027749442449, 0.872647581723173, 0.578761326089712, 0.999992516237255,
     0.0032275172033282, 0.000886873991940027, 0.0305443126609233,
     615.040871203671, 379.184687828394, 824.311627497722))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.00869539911641592,
     0.000180456421440328, 0.0724890542807005, 0.00865770347220153,
     0.000180440140147592, 0.0699240729616308, 0.000127374047424372,
     4.88729143875147e-06, 0.00059919507560939, 135.60580881558,
     131.377335620416, 135.994685909755, 273, 0.0348476041675894,
     0.00239551047908804, 0.16108419315216, 0.0342474182878267, 0.00239264353293714,
     0.148779598828499, 0.000254296635735036, 2.98821237044727e-05,
     0.000769320535648162, 269.853570611605, 253.538302809291, 272.860950493821,
     409, 0.0779691465831053, 0.0097589509997633, 0.269555201115191,
     0.075007034703139, 0.00971148693468851, 0.236280879482374, 0.00037977800548573,
     7.70729444326036e-05, 0.00110099511987244, 398.587310611234,
     363.916896329907, 408.113145093528, 545, 0.138132296400909,
     0.025107946332557, 0.425544488468531, 0.12901654385316, 0.0247953631261312,
     0.346586087516169, 0.00050492754866362, 0.0001412914863281,
     0.00163929742530345, 520.850185828311, 462.074671878733, 541.7702883063,
     682, 0.215928610519776, 0.0516401457438025, 0.663133698327089,
     0.194207175048875, 0.0503294516058211, 0.484765784799407, 0.000630748392215043,
     0.000208126923462534, 0.00240298983309309, 635.81649794337,
     545.914094076291, 673.692516192902, 818, 0.31019239715386, 0.0884798465525075,
     1.02377351144825, 0.266694143308586, 0.084678442455249, 0.640763204847647,
     0.000755454273071759, 0.000261369281495437, 0.00347771544960727,
     740.545691676849, 611.692644043047, 800.00529514953, 954, 0.421404814595403,
     0.135311967076303, 1.54806040969275, 0.343875561108497, 0.126556614985477,
     0.787339938964108, 0.000879997534047205, 0.000308263212922956,
     0.004763959156705, 835.062999182697, 659.256188248613, 921.081017303544,
     1091, 0.550550288311108, 0.188971021453972, 2.29015267298838,
     0.423367591017112, 0.172189504390661, 0.898748992536609, 0.00100531596745529,
     0.000345260523215708, 0.00643429480446505, 919.510389727533,
     689.008157322751, 1036.43899652747, 1227, 0.695725715127604,
     0.247629496608537, 3.27054850022794, 0.501287606501468, 0.219350877083463,
     0.962014396147918, 0.00112959777910214, 0.000370868478649694,
     0.00828399276643979, 992.608544523504, 707.042426169893, 1143.86829599821
    ))

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-7_subgroup-1")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-8_subgroup-2")

  plotName <- results[["results"]][["residualHistogram"]][["collection"]][["residualHistogram_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-9_subgroup-1")

  plotName <- results[["results"]][["residualHistogram"]][["collection"]][["residualHistogram_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-10_subgroup-2")

  plotName <- results[["results"]][["residualVsPredictedPlot"]][["collection"]][["residualVsPredictedPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-11_subgroup-1")

  plotName <- results[["results"]][["residualVsPredictedPlot"]][["collection"]][["residualVsPredictedPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-12_subgroup-2")

  plotName <- results[["results"]][["residualsVsPredictorsPlot"]][["collection"]][["residualsVsPredictorsPlot_table1"]][["collection"]][["residualsVsPredictorsPlot_table1_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-13_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsVsPredictorsPlot"]][["collection"]][["residualsVsPredictorsPlot_table2"]][["collection"]][["residualsVsPredictorsPlot_table2_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-14_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsVsTimePlot"]][["collection"]][["residualsVsTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-15_subgroup-1")

  plotName <- results[["results"]][["residualsVsTimePlot"]][["collection"]][["residualsVsTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-16_subgroup-2")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["collection"]][["restrictedMeanSurvivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-17_subgroup-1")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["collection"]][["restrictedMeanSurvivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-18_subgroup-2")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(104.448725742629, 106.143573815014, 3, -49.2243628713144, 1, 83.5344037495967,
     85.2292518219813, 3, -38.7672018747983, 2))

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-19_subgroup-1")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-20_subgroup-2")

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 198.718086748457, 72.6579282912515, 386.887536950718,
     20, 306.469681424375, 149.389911068186, 530.641209686018, 30,
     401.771801827806, 224.84487845173, 663.234624390241, 40, 494.356631593181,
     291.97301569715, 799.805906759382, 50, 589.610008817539, 354.135196269329,
     957.415963570959, 60, 692.691306791741, 409.367042304821, 1158.90653778219,
     70, 810.960120381918, 462.727703479135, 1426.08287391954, 80,
     958.901933768744, 526.017897586853, 1822.98108544457, 90, 1179.15349846453,
     605.455061598488, 2537.7001595619, 100, "<unicode>", "<unicode>",
     "<unicode>"))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 475.726369440225, 190.365152955342, 869.670169353663,
     20, 693.345077123755, 354.940590326897, 1167.93972654785, 30,
     877.390496431623, 492.473895714154, 1447.70104257921, 40, 1050.74860461096,
     600.605619599755, 1740.54875537942, 50, 1224.71518779887, 695.708453650989,
     2082.31133745698, 60, 1408.88863141754, 782.229259592265, 2491.08714954434,
     70, 1615.84930419667, 870.415441864297, 3060.74582138739, 80,
     1869.29043658753, 967.976315989896, 3806.54281607258, 90, 2237.44555055393,
     1092.93873630834, 5145.50236717121, 100, "<unicode>", "<unicode>",
     "<unicode>"))

})

test_that("ParametricSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["censoringSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(12, "Events", 14, "Censored"))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", -0.0284026206806305, 0.0628930366612109, "scale", 0.0867662670344112,
     -0.0284026206806305))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("meanlog", 0.102084672857729, 0.0351482107495265, "sdlog", 0.0351482107495265,
     0.0518049514089254))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", -0.0338516032642821, 0.0643153445670604, "scale", 0.0856896561723237,
     -0.0338516032642821))

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", "Log-logistic", 1.37597572950465, 0.841670933843557,
     0.345073855053439, 2.24946488236194, "scale", "", 837.752879915103,
     470.310884553795, 246.769445957376, 1492.26801006722, "meanlog",
     "Log-normal", 6.77210985901319, 6.14588780014988, 0.319506921455121,
     7.39833191787649, "sdlog", "", 1.26577094495712, 0.810243738553545,
     0.288098341260455, 1.9774001437615, "shape", "Weibull", 1.10805973956938,
     0.674053646122464, 0.281009159375663, 1.82151137897932, "scale",
     "", 1225.41895892538, 690.421182604192, 358.714386981839, 2174.97907470001
    ))

  plotName <- results[["results"]][["cumulativeHazardPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_predicted-cumulative-hazard")

  plotName <- results[["results"]][["failureProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_predicted-failure-probability")

  plotName <- results[["results"]][["hazardPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_predicted-hazard")

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 136, 0.0787671006676681,
     0.0193988285412729, 0.196828290616758, 0.0757448422105838, 0.0192118820652699,
     0.178668348416154, 0.000766346062624432, 0.000279688015867371,
     0.0014434723997693, 131.538812172018, 122.557361266923, 135.131600821266,
     273, 0.193738252321238, 0.0755764039676131, 0.379462984855148,
     0.176126476969262, 0.0727911146100375, 0.315771247961946, 0.000887713397922583,
     0.000444050518209319, 0.00171305519571072, 251.316575857629,
     226.274292777002, 266.004329120183, 409, 0.316887096930438,
     0.149484190602884, 0.585420064903437, 0.27158700945416, 0.138847947504687,
     0.443128109490974, 0.0009136849228794, 0.000492755733370139,
     0.00187130216301819, 356.769099171478, 312.725146424374, 387.258935906312,
     545, 0.440478434719357, 0.227640068689964, 0.817938675292323,
     0.356271634603091, 0.203589138518515, 0.558659517935492, 0.000899488297843676,
     0.000478190281135496, 0.00189268644351973, 449.940831440177,
     383.669713047803, 498.853331790406, 682, 0.561612496447821,
     0.309176384057076, 1.0591555781687, 0.429711266523461, 0.265948714644433,
     0.653251495369774, 0.000866968142860685, 0.000449257191783188,
     0.00187235207945141, 532.968503200198, 441.306442819413, 602.836404979266,
     818, 0.676865956486439, 0.38332790818181, 1.28970410109335,
     0.491792757280374, 0.31841063608339, 0.724647693669399, 0.000827255376484067,
     0.00042157500611112, 0.00178698010809545, 606.192139910744,
     487.732562268998, 697.596301887705, 954, 0.786535317179999,
     0.453707888691182, 1.50078096430846, 0.544580049321246, 0.36473172478916,
     0.777044018752637, 0.00078546009500889, 0.00039447869687036,
     0.00167411415390108, 671.622170838795, 524.607169760363, 785.560775480267,
     1091, 0.891283517746075, 0.523034089785999, 1.70737096757638,
     0.589870993077716, 0.407280547010618, 0.818658079296516, 0.000743948826777034,
     0.000368460689123101, 0.00156641766206686, 730.830049115756,
     554.601628977526, 865.581409445798, 1227, 0.989760907390754,
     0.585739711934087, 1.89728556542174, 0.628334457115874, 0.443306083597575,
     0.850024834620961, 0.000704623441730175, 0.000346249656563405,
     0.00146931807279342, 783.925488773282, 579.406966346111, 941.489998960029
    ))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.0735522299564721,
     0.014884008320687, 0.186757972291212, 0.0709123816816271, 0.0147737889343706,
     0.17035549072785, 0.000847901116002147, 0.000301458695878105,
     0.00159589758718278, 132.427171663022, 124.325138921401, 135.500819763856,
     273, 0.197442794844509, 0.0803385238759249, 0.387718684979928,
     0.179172905173709, 0.0771960976342957, 0.321396782302828, 0.000922438401640329,
     0.000504438863080962, 0.00179217822312522, 252.235762732066,
     228.266821018854, 266.500931371686, 409, 0.320941503233119,
     0.159467332621823, 0.599883854188777, 0.274534312853878, 0.147402180252409,
     0.451124618005171, 0.000887690215003281, 0.000504762747720946,
     0.00181384198761894, 357.207719261908, 313.401523289125, 386.530380524119,
     545, 0.438213449041613, 0.239474688723448, 0.831657579723804,
     0.35481194661295, 0.212958805887048, 0.564672898094226, 0.000836303717742812,
     0.000467672247497044, 0.00176851182069679, 450.25108319624,
     383.270868002259, 497.727989359205, 682, 0.549250597444693,
     0.316711209369305, 1.05592555213408, 0.422617659908137, 0.271458879011156,
     0.652129691886266, 0.000785295280875926, 0.000426620740839076,
     0.00169602354616557, 533.862470384273, 441.271488075578, 600.18174933326,
     818, 0.652858893321358, 0.384594537874387, 1.26929326342714,
     0.479444565044411, 0.319273410893348, 0.718969829521353, 0.000739192683174463,
     0.000391720145525005, 0.00162713311261578, 608.41655803868,
     486.103783932095, 694.691023653046, 954, 0.75053776576014, 0.447769492612454,
     1.48127110950348, 0.52788740152721, 0.360948026741825, 0.77265147854522,
     0.000698068693517688, 0.000363787293125333, 0.00156729013538236,
     675.832945450114, 522.599471486373, 782.102955332277, 1091,
     0.8436026806958, 0.506799100776687, 1.67786437058328, 0.569841993449342,
     0.397579215736055, 0.813227561255247, 0.000661268714467024,
     0.000337808765696844, 0.00149389842356237, 737.567980225243,
     552.255187735841, 865.87731812467, 1227, 0.931281594403621,
     0.562596988875625, 1.86923325988139, 0.605951623569797, 0.430272434166457,
     0.84575811949068, 0.000628743052409768, 0.000315157475687027,
     0.00143442746549618, 793.557504849479, 576.080941856587, 943.496471607421
    ))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 136, 0.0875154525519651,
     0.0240000483861446, 0.214354407663232, 0.0837952861594605, 0.0237143374644532,
     0.192937693670363, 0.00071303198208108, 0.000286584046682314,
     0.00125425128839376, 130.512485859943, 120.331213609028, 134.799164555353,
     273, 0.189413061630606, 0.0753277718974706, 0.367551408539906,
     0.172555349540808, 0.0725605520570018, 0.30757227055179, 0.000768794826891753,
     0.000404763624325189, 0.00138178388276461, 249.92427515515,
     223.164198593437, 265.349260045602, 409, 0.296443289805187,
     0.141137186591568, 0.537000585219525, 0.256542212295394, 0.131629825202271,
     0.415501219472443, 0.000803121942539428, 0.000446931023142328,
     0.00160785852954236, 356.68514148403, 311.50419882846, 387.32252175484,
     545, 0.407461867575075, 0.213198899710753, 0.733506325759038,
     0.334663178179284, 0.192004588795416, 0.519777780778426, 0.000828425854806772,
     0.000456188191940152, 0.00184811243588958, 452.412297409991,
     386.168962309711, 500.750712961837, 682, 0.522394447164483,
     0.284466830846765, 0.980490722433613, 0.406901297124311, 0.247584683340928,
     0.624873028598543, 0.00084874524204922, 0.00045000142104494,
     0.00207828722022079, 538.540320164375, 449.896185809415, 606.372983569042,
     818, 0.638999977461617, 0.358072132555743, 1.25987639883147,
     0.472180007902583, 0.300977347275862, 0.716310911347015, 0.000865586978742005,
     0.000439026019710293, 0.00230513331872241, 614.691067935066,
     499.71814565417, 703.060322622403, 954, 0.757728760718732, 0.434998204932623,
     1.57548800714998, 0.531270183481651, 0.35273416852137, 0.793093404011022,
     0.000880093011809464, 0.000428149017975991, 0.00252592781880229,
     682.388087549097, 537.857282775002, 792.915361332954, 1091,
     0.879199591719079, 0.503509950363115, 1.93641882393812, 0.584884959739624,
     0.395594500216314, 0.855780498477055, 0.000892947452456232,
     0.000416958847630573, 0.00274591843333462, 742.866000480166,
     569.17607051361, 877.624645425308, 1227, 1.0014297233001, 0.570564894831506,
     2.30833778582385, 0.632646148823566, 0.434793930835085, 0.900573617567208,
     0.000904355304316987, 0.000406366880854969, 0.00297546261727128,
     796.01461216043, 587.965847671388, 954.641380744004))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-4_probability-plot")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-5_predicted-restricted-mean-survival-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(198.709408968454, 201.225602044497, 2, "Log-logistic", -97.354704484227,
     198.243484085311, 200.759677161354, 2, "Log-normal", -97.1217420426555,
     199.907802094262, 202.423995170304, 2, "Weibull", -97.9539010471308
    ))

  plotName <- results[["results"]][["survivalTimePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-6_predicted-survival-time")

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 169.673297958982, 65.4864895397585, 330.039933820102,
     20, 305.889530851268, 156.295407254479, 536.574541935314, 30,
     452.570480494805, 257.021315571558, 767.545665320735, 40, 623.936472768971,
     360.541772542722, 1066.38232800809, 50, 837.752879915103, 470.807661290757,
     1495.98410487176, 60, 1124.84190047649, 601.725555616689, 2191.90270188018,
     70, 1550.76373306258, 765.603738280288, 3447.85215781514, 80,
     2294.39002326397, 1027.39061363391, 6266.97937080868, 90, 4136.36026557173,
     1541.03437800466, 15691.159508315, 100, "<unicode>", "<unicode>",
     "<unicode>"))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 172.424441958627, 76.7887689961651, 316.670112742475,
     20, 300.909680488339, 161.691339609186, 516.297914672484, 30,
     449.591290400454, 253.533873800937, 761.074604042198, 40, 633.607993785386,
     354.444826402003, 1100.57959682562, 50, 873.152179932877, 464.836872215622,
     1610.2864269486, 60, 1203.25932879529, 596.665239690652, 2459.55394782025,
     70, 1695.7506642143, 776.047359982119, 3951.71234813659, 80,
     2533.63310905872, 1038.97264871697, 7063.59925536919, 90, 4421.61633618319,
     1550.52880475636, 16741.5805174246, 100, "<unicode>", "<unicode>",
     "<unicode>"))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 160.794978492893, 50.6954692073589, 337.594635470775,
     20, 316.516257430048, 143.946241606259, 563.98904702513, 30,
     483.303879617768, 262.558476399946, 815.130153957384, 40, 668.354658896766,
     391.091217262141, 1104.53203160746, 50, 880.304676917499, 517.095549733856,
     1474.60199621137, 60, 1132.45371871949, 647.900305590952, 1968.60277797037,
     70, 1448.90331582141, 793.242495255443, 2717.54012253372, 80,
     1882.79755170067, 966.920158247777, 3921.58731127352, 90, 2601.21611951073,
     1224.90870985404, 6372.1793589818, 100, "<unicode>", "<unicode>",
     "<unicode>"))

})

