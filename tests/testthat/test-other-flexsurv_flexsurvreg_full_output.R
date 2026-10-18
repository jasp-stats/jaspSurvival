context("Other: flexsurv_flexsurvreg_full_output")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  opts[["predictionsOutputTableFormat"]] <- "long"
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
    list("shape", 1.732161815801, 0.927978225961469, "", 0.551577511151959,
        1, 3.23324887608256, "", "scale", 46376.8295057228, 3231.04349553521,
        "", 63035.665259807, "", 665670.492512701, "", "jaspColumn1",
        -0.0745258522629337, -0.116073784357059, 0.000438678919027338,
        0.0211983140618143, "", -0.0329779201688086, -3.51564997318259,
        "shape", 1.99218807259738, 1.02034904868738, "", 0.680089393495839,
        2, 3.88966238730259, "", "scale", 12246469.6195708, 1044.8708067171,
        "", 58541073.2743625, "", 143535465991.515, "", "jaspColumn1",
        -0.159479574239656, -0.315083240916624, 0.0445600009210647,
        0.0793910846854076, "", -0.00387590756268771, -2.00878442298155))

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
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.00869539898611844,
        0.000158580225822316, 0.0775073715612134, 0.00865770334303212,
        0.000158567652638257, 0.0745797973481693, 0.000127374045195739,
        4.11721552506631e-06, 0.000686785911055016, 135.605808820813,
        131.169770681422, 135.995417291476, 273, 0.0348476035238867,
        0.00183420205925201, 0.184921752199685, 0.0342474176661691,
        0.00183252093863417, 0.168830681517225, 0.000254296630398863,
        2.27443917577046e-05, 0.000972288979239625, 269.85357066387,
        251.890708438001, 272.885506341395, 409, 0.0779691449851267,
        0.00731121659917753, 0.327045718595427, 0.0750070332250201,
        0.00728455467149917, 0.278949215882085, 0.00037977799674813,
        5.63231393069979e-05, 0.00148045087787509, 398.587310802382,
        357.682678093081, 408.311791877041, 545, 0.138132293371435,
        0.0187255367309647, 0.548876583718136, 0.129016541214538,
        0.0185513030895898, 0.422401665621814, 0.000504927536321273,
        0.000100255866467532, 0.00226211273567652, 520.850186296468,
        447.702653836396, 542.59756523132, 682, 0.215928605541772,
        0.0371745298262417, 0.90582079410419, 0.194207171037634,
        0.0364920401767725, 0.595790008578788, 0.000630748376089299,
        0.000146664286008224, 0.00333964281903596, 635.816498865403,
        520.550529440037, 675.687860517581, 818, 0.31019238972043,
        0.0632042030801334, 1.42240028600057, 0.266694137857608,
        0.0612482406533825, 0.758865453127882, 0.000755454253070315,
        0.000191295789226203, 0.00481069891365065, 740.545693242183,
        571.921542754864, 805.013182195448, 954, 0.421404804172523,
        0.0952996085879106, 2.16944104863952, 0.343875554269791,
        0.0908994806458582, 0.885758528774, 0.000879997510071002,
        0.000231128481731816, 0.00671874881394803, 835.06300158502,
        599.974017165024, 930.38225706562, 1091, 0.550550274324268,
        0.134903275442168, 3.20465575235467, 0.423367582951848, 0.126199574424874,
        0.959427053637813, 0.00100531593938957, 0.000263629810397266,
        0.00916983970270377, 919.510393153346, 616.962240943408,
        1051.31894640467, 1227, 0.695725697043517, 0.179709901471931,
        4.61699951017921, 0.50128759748271, 0.16448744265601, 0.990117592296487,
        0.00112959774690268, 0.000295512245043848, 0.0117125022700092,
        992.608549114436, 625.24975622173, 1166.81190593709))

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
    list(0, 0, 0, 0, 10, 475.726374514959, 175.283909750473, 991.616430148745,
        20, 693.345085175997, 314.809114747915, 1346.20738050372,
        30, 877.390507140197, 429.279873963357, 1666.56923327737,
        40, 1050.74861791131, 524.719527992108, 1993.43325077613,
        50, 1224.71520377264, 606.238382471045, 2371.07318416605,
        60, 1408.88865028929, 685.168605310593, 2831.83332082917,
        70, 1615.84932639695, 765.043953793446, 3391.01137119611,
        80, 1869.29046295404, 857.723254750599, 4199.82100602314,
        90, 2237.44558312377, 975.558161233543, 5600.93337765441,
        100, "<unicode>", "<unicode>", "<unicode>"))

})

test_that("ParametricSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  opts[["predictionsOutputTableFormat"]] <- "long"
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

test_that("ParametricSurvivalAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  opts[["predictionsOutputTableFormat"]] <- "long"
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 1.3792322177338, 0.075787741796219, "", 2.04168520052764,
        25.1001212775258, "", "scale", 181400750885.405, 1140151.69060426,
        "", 1108536539302.09, 28861275822297828, "", "[scale] log(jaspColumn1)",
        -4.73616807436202, -7.62877322470244, 0.001331416214734,
        1.47584607327324, -1.8435629240216, -3.20912062587786, "[scale] jaspColumn2 contrast 1 (Helmert)",
        0.313574802993169, -0.0557938697786514, 0.0961309076413167,
        0.188456867414582, 0.682943475764989, 1.66390754178962, "[shape] jaspColumn1",
        0.00424115248421963, -0.0422825448843662, 0.85819484993401,
        0.0237370164633426, 0.0507648498528055, 0.178672517279891))

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_jaspcolumn2-1")

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_jaspcolumn2-2")

  plotName <- results[["results"]][["medianSurvivalTimePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_median-survival-time")

  table <- results[["results"]][["medianSurvivalTimeTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3123.83324631979, 682.743421169468, 12154.5516916819, 38.8932,
        1, 5848.64761900758, 1655.66517354333, 17992.741294444, 38.8932,
        2, 1981.67977380203, 561.642831796923, 6243.88895897063,
        42.8499666666667, 1, 3710.23219768121, 1337.10325758944,
        9264.6049378233, 42.8499666666667, 2, 1309.07842551897, 465.472613285802,
        3388.30751493712, 46.8067333333333, 1, 2450.94338038873,
        1088.89942415602, 5098.35549921188, 46.8067333333333, 2,
        894.586960179107, 386.680336975264, 1955.53909932612, 50.7635,
        1, 1674.90499078681, 886.418042635109, 3060.94111025321,
        50.7635, 2, 629.198812045714, 322.281741478563, 1200.30242590501,
        54.7202666666667, 1, 1178.02771267927, 686.503915191447,
        1973.38521621409, 54.7202666666667, 2, 453.638921750475,
        261.646284745643, 776.667088806978, 58.6770333333333, 1,
        849.332851780998, 513.811442319998, 1387.94332626741, 58.6770333333333,
        2, 334.181053248964, 204.572315084266, 535.279624238574,
        62.6338, 1, 625.675913944707, 366.829786847068, 1055.48063186863,
        62.6338, 2, 250.872784049159, 153.969452044038, 399.588143207012,
        66.5905666666667, 1, 469.700651541344, 256.538307943114,
        852.128201555864, 66.5905666666667, 2, 191.503095884292,
        112.100882345082, 316.616508579789, 70.5473333333333, 1,
        358.544786952301, 177.748951408135, 711.399035220429, 70.5473333333333,
        2, 148.372888160419, 80.7072126583763, 263.077525505335,
        74.5041, 1, 277.793553829115, 123.002873691749, 611.954279584804,
        74.5041, 2))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(187.67303741196, 193.963520102067, 5, -88.8365187059798))

})

