context("Other: flexsurv_flexsurvreg_full_output")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", dataset, opts)

  table <- results[["results"]][["censoringSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 1, "Events", 5, 2, "", 6, 1, "Censored", 8, 2, ""))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00294583405248142, "shape", -0.195092282202688, 0.101399586243753, -0.0284318662588786,
         "scale", 1.84744099223018, -0.195092282202688, 0.000449368519066222, "scale(age)",
         -0.0284318665535529, 0.00294583404147664))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00753978163404189, "shape", -0.472986932576419, 0.116539009633363, -0.379060269714016,
         "scale", 22.8507010819253, -0.472986932576419, 0.00630294432752521, "scale(age)",
         -0.379053598831464, 0.00753960491518431))

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 1.732161815801, 0.927978225961469, "", 0.551577511151959, 1, 3.23324887608256, "",
         "scale", 46376.8295057228, 3231.04349553521, "", 63035.665259807, "", 665670.492512701, "",
         "age", -0.0745258522629337, -0.116073784357059, 0.000438678919027338, 0.0211983140618143, "",
         -0.0329779201688086, -3.51564997318259, "shape", 1.99218807259738, 1.02034904868738, "",
         0.680089393495839, 2, 3.88966238730259, "", "scale", 12246469.6195708, 1044.8708067171, "",
         58541073.2743625, "", 143535465991.515, "", "age", -0.159479574239656, -0.315083240916624,
         0.0445600009210647, 0.0793910846854076, "", -0.00387590756268771, -2.00878442298155))

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
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 123, 0.04589999700458, 0.0035290180028644,
         0.184468409853579, 0.0448625259766594, 0.00352279833518057, 0.168453791594474,
         0.000646392050060632, 9.69468082832524e-05, 0.00165057785824674, 120.962320563707,
         112.288131588984, 122.876564610606, 246, 0.152491389279767, 0.029424082798575,
         0.406251838571646, 0.141433713201337, 0.0289954092053671, 0.333857611437922,
         0.00107373886890784, 0.000337011826244639, 0.00260253837277186, 232.887788012382,
         205.057812299036, 244.173800581963, 369, 0.30779664106479, 0.0888027549653951,
         0.756595174290882, 0.264935215197203, 0.0849739597957325, 0.530738536238193,
         0.00144486067395454, 0.000569291620886698, 0.00434569019301614, 331.072114221693,
         278.108336559881, 360.23369382787, 492, 0.506614930762481, 0.183121695165447, 1.37254026642147,
         0.397468257743826, 0.167333181113132, 0.746537719620072, 0.00178361593168628,
         0.00073262514685074, 0.00685236816409416, 413.349433348712, 328.581741161678, 467.396528868039,
         614, 0.743562729610374, 0.300275094110332, 2.34347402970844, 0.524582887932561,
         0.259385545435076, 0.904006425406823, 0.00209767258614968, 0.000817919096842538,
         0.0101913379732392, 479.007552945456, 358.236804446671, 562.30630607204, 737, 1.02017931840542,
         0.43430000673274, 3.81081085622019, 0.639469715339462, 0.352282093127573, 0.977869690795931,
         0.00239771460058114, 0.000852377347548663, 0.0141433860069918, 530.250196899194,
         371.780265679422, 644.559331573268, 860, 1.33286139825671, 0.570760424193494, 5.8722533674421,
         0.736278431789546, 0.434904436604529, 0.997183479203529, 0.00268457165097327,
         0.000870979694544984, 0.019211827751245, 568.44391469234, 377.354287630796, 715.628184032435,
         983, 1.68014111584834, 0.721820091894621, 8.49151754229085, 0.813652322426719,
         0.514132870389381, 0.999806749957389, 0.00296060659822688, 0.000885881958156619,
         0.0247889236460128, 595.925825111859, 379.590486745342, 776.31387348025, 1106, 2.06079708851964,
         0.855953309258977, 11.9263282302482, 0.872647581723173, 0.575122041600297, 0.999992516237255,
         0.0032275172033282, 0.000886873991940027, 0.0315766657635466, 615.040871203671,
         379.977754899601, 827.332246660878))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.00869539898611844, 0.000129275455130046,
         0.0824339045851802, 0.00865770334303212, 0.000129267099406002, 0.079127698705876,
         0.000127374045195739, 4.11721552506631e-06, 0.000686785911055016, 135.605808820813,
         130.810077646441, 135.995417291476, 273, 0.0348476035238867, 0.00165868770052838,
         0.186589248935086, 0.0342474176661691, 0.00165731283829048, 0.170215498728168,
         0.000254296630398863, 2.27443917577046e-05, 0.000972288979239625, 269.85357066387,
         250.968093616587, 272.885506341395, 409, 0.0779691449851267, 0.00693388731731103,
         0.331779316178934, 0.0750070332250201, 0.0069099033865033, 0.28235432175351,
         0.00037977799674813, 5.63231393069979e-05, 0.00148045087787509, 398.587310802382,
         357.359031830779, 408.358877481313, 545, 0.138132293371435, 0.0176077034599216,
         0.54738106319547, 0.129016541214538, 0.0174535936801529, 0.42153721419333, 0.000504927536321273,
         0.000100255866467532, 0.00226211273567652, 520.850186296468, 447.177488746151, 542.716334896379,
         682, 0.215928605541772, 0.0360250117918175, 0.902933446419771, 0.194207171037634,
         0.0353838335408945, 0.594621242782551, 0.000630748376089299, 0.000146664286008224,
         0.00333964281903596, 635.816498865403, 520.476371469485, 676.079320729806, 818,
         0.31019238972043, 0.0617101278190473, 1.42478972014604, 0.266694137857608, 0.0598446278033476,
         0.759440944993698, 0.000755454253070315, 0.000191295789226203, 0.00481069891365065,
         740.545693242183, 571.921542754864, 805.509645557222, 954, 0.421404804172523,
         0.0943365037066702, 2.14013802160743, 0.343875554269791, 0.0900234996544441, 0.882361386966424,
         0.000879997510071002, 0.000231128481731816, 0.00686851967828022, 835.06300158502,
         605.645887637109, 930.38225706562, 1091, 0.550550274324268, 0.133734064032463, 3.23219956171199,
         0.423367582951848, 0.125177319508233, 0.960529385046231, 0.00100531593938957,
         0.000263629810397266, 0.00914518109794835, 919.510393153346, 620.315156311168, 1052.0090050016,
         1227, 0.695725697043517, 0.179303547006118, 4.73505536388788, 0.50128759748271,
         0.164147856396367, 0.991218037119816, 0.00112959774690268, 0.000295512245043848,
         0.0121056554900555, 992.608549114436, 627.547148273872, 1167.91592820146))

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
    list(0, 0, 0, 0, 10, 198.718086748457, 70.0825321016936, 394.967603750557, 20, 306.469681424375,
         146.928646961919, 535.489929711903, 30, 401.771801827806, 222.907462036435, 667.660781596217,
         40, 494.356631593181, 289.404698366072, 808.549805612947, 50, 589.610008817539, 351.76548962275,
         960.015664220261, 60, 692.691306791741, 408.664545335101, 1161.67320974412, 70,
         810.960120381918, 464.921956427013, 1430.0696931916, 80, 958.901933768744, 527.726166721104,
         1816.21213034296, 90, 1179.15349846453, 610.59194505137, 2530.04669931135, 100, "<unicode>",
         "<unicode>", "<unicode>"))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 475.726374514959, 169.316693209592, 995.669672299374, 20, 693.345085175997,
         313.156454986095, 1348.24041589537, 30, 877.390507140197, 428.480238408714, 1667.79589879305,
         40, 1050.74861791131, 525.717953041793, 1996.1815920334, 50, 1224.71520377264, 610.498872045422,
         2364.76692947528, 60, 1408.88865028929, 686.673953929784, 2817.12915841983, 70,
         1615.84932639695, 767.748884051495, 3386.98918613442, 80, 1869.29046295404, 857.723254750599,
         4176.35608561548, 90, 2237.44558312377, 977.470370286145, 5551.42466384883, 100, "<unicode>",
         "<unicode>", "<unicode>"))

})

test_that("ParametricSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", dataset, opts)

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
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 136, 0.0787671006676681, 0.0184753877493133,
         0.198315029300027, 0.0757448422105838, 0.0183057640008665, 0.17988854668414,
         0.000766346062624432, 0.000279688015867371, 0.00145032950833849, 131.538812172018,
         122.511064132216, 135.183915464627, 273, 0.193738252321238, 0.0714646883240811,
         0.381024459220123, 0.176126476969262, 0.0689708467670744, 0.316838820174053,
         0.000887713397922583, 0.000444050518209319, 0.00171305519571072, 251.316575857629,
         226.15713716068, 266.402025205239, 409, 0.316887096930438, 0.144764716467315, 0.590826358346414,
         0.27158700945416, 0.134774157435398, 0.446130599026739, 0.0009136849228794,
         0.000492755733370139, 0.00187130216301819, 356.769099171478, 312.461046910801, 388.1896118283,
         545, 0.440478434719357, 0.223165468604234, 0.821502319874592, 0.356271634603091,
         0.200017533465569, 0.560229517816285, 0.000899488297843676, 0.000478190281135496,
         0.00189268644351973, 449.940831440177, 382.720466410739, 500.303024357504, 682,
         0.561612496447821, 0.308546566457172, 1.05808506858847, 0.429711266523461, 0.265486252299398,
         0.652880112814384, 0.000866968142860685, 0.00045321875346591, 0.00187235207945141,
         532.968503200198, 440.333225373355, 604.823079922992, 818, 0.676865956486439, 0.386038583642707,
         1.29310519404656, 0.491792757280374, 0.320255698132597, 0.725582658536028, 0.000827255376484067,
         0.00042157500611112, 0.00178698010809545, 606.192139910744, 486.564898597409, 700.052093019034,
         954, 0.786535317179999, 0.459835614348921, 1.51866549975809, 0.544580049321246,
         0.36861257104783, 0.780996046854034, 0.00078546009500889, 0.00039447869687036,
         0.00167411415390108, 671.622170838795, 523.522877325334, 786.897429223196, 1091,
         0.891283517746075, 0.530694681252327, 1.72629173406162, 0.589870993077716, 0.411803780793253,
         0.8220569507764, 0.000743948826777034, 0.000368460689123101, 0.00156641766206686,
         730.830049115756, 553.072043121786, 867.543882076942, 1227, 0.989760907390754,
         0.590274920280411, 1.92776616568254, 0.628334457115874, 0.445825090127769, 0.854527201745593,
         0.000704623441730175, 0.000346249656563405, 0.00146931807279342, 783.925488773282,
         578.67845735034, 942.247337219385))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.0735522299564721, 0.014884008320687,
         0.187034123757107, 0.0709123816816271, 0.0147737889343706, 0.170584566656469,
         0.000847901116002147, 0.000301458695878105, 0.00159589758718278, 132.427171663022,
         124.410255490613, 135.500819763856, 273, 0.197442794844509, 0.0771007541216782,
         0.383760392641184, 0.179172905173709, 0.074203428909379, 0.318705348174627,
         0.000922438401640329, 0.000504438863080962, 0.00179217822312522, 252.235762732066,
         228.474677137736, 266.562840919377, 409, 0.320941503233119, 0.15530322083609, 0.60351895842777,
         0.274534312853878, 0.143844465975793, 0.45311621218635, 0.000887690215003281,
         0.000504762747720946, 0.00181384198761894, 357.207719261908, 314.51122686203, 387.283890399357,
         545, 0.438213449041613, 0.237270847023318, 0.832834392243354, 0.35481194661295,
         0.211222378985182, 0.56518489831694, 0.000836303717742812, 0.000467672247497044,
         0.00176851182069679, 450.25108319624, 383.958465960063, 498.523418576591, 682,
         0.549250597444693, 0.31312367320425, 1.06417701811453, 0.422617659908137, 0.268840516707114,
         0.65498832367621, 0.000785295280875926, 0.000426620740839076, 0.00169602354616557,
         533.862470384273, 440.707988270894, 601.807845702453, 818, 0.652858893321358, 0.38318441323635,
         1.27697454267171, 0.479444565044411, 0.318312823552971, 0.721120224664335, 0.000739192683174463,
         0.000391720145525005, 0.00162713311261578, 608.41655803868, 484.693718239518, 696.434664190234,
         954, 0.75053776576014, 0.445313758584808, 1.48034435748918, 0.52788740152721, 0.359376756543537,
         0.77244068461654, 0.000698068693517688, 0.000363787293125333, 0.00156729013538236,
         675.832945450114, 521.215801600446, 784.436961886818, 1091, 0.8436026806958, 0.502645455568815,
         1.68230751230001, 0.569841993449342, 0.39507176950133, 0.814055576774951, 0.000661268714467024,
         0.000337808765696844, 0.00149389842356237, 737.567980225243, 550.948429773233, 867.110007595165,
         1227, 0.931281594403621, 0.556293981058621, 1.87675319865169, 0.605951623569797,
         0.4266700962164, 0.846913659014578, 0.000628743052409768, 0.000315157475687027,
         0.00146062741531551, 793.557504849479, 575.428144522386, 945.37504032426))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 136, 0.0875154525519651, 0.0228056299120798,
         0.215090103343245, 0.0837952861594605, 0.022547547169938, 0.193531228628183,
         0.00071303198208108, 0.000286584046682314, 0.00125425128839376, 130.512485859943,
         120.202393236911, 134.855029678429, 273, 0.189413061630606, 0.0720131343167419,
         0.369572682193174, 0.172555349540808, 0.0694813259736405, 0.308970443166879,
         0.000768794826891753, 0.000404763624325189, 0.00138178388276461, 249.92427515515,
         222.984837533539, 265.674315360847, 409, 0.296443289805187, 0.135550830984617,
         0.539125761583356, 0.256542212295394, 0.126765225466355, 0.416742063055503,
         0.000803121942539428, 0.000446931023142328, 0.00160785852954236, 356.68514148403,
         310.784936525651, 388.144281624504, 545, 0.407461867575075, 0.208410785128097,
         0.740701274522819, 0.334663178179284, 0.18812652882825, 0.523220554722586, 0.000828425854806772,
         0.000456188191940152, 0.00185728793505045, 452.412297409991, 386.291319425635, 502.084896891397,
         682, 0.522394447164483, 0.279538123362372, 0.984141224517961, 0.406901297124311,
         0.243867098986714, 0.626239934654172, 0.00084874524204922, 0.00045000142104494,
         0.00207828722022079, 538.540320164375, 448.88657705497, 608.840513696737, 818,
         0.638999977461617, 0.354451549005841, 1.26202278906532, 0.472180007902583, 0.298441891822222,
         0.716919164146127, 0.000865586978742005, 0.000439026019710293, 0.00230513331872241,
         614.691067935066, 496.929444389409, 706.004635465452, 954, 0.757728760718732, 0.432270696711332,
         1.57661746046906, 0.531270183481651, 0.350966338164552, 0.793326979797882, 0.000880093011809464,
         0.000428149017975991, 0.00260138257011856, 682.388087549097, 536.069335462222, 796.216633448339,
         1091, 0.879199591719079, 0.503664976871037, 1.93687331487063, 0.584884959739624,
         0.395688192549683, 0.855780498477055, 0.000892947452456232, 0.000416958847630573,
         0.00282302298642678, 742.866000480166, 565.588870590888, 881.154988030365, 1227,
         1.0014297233001, 0.578943503054427, 2.32659961603427, 0.632646148823566, 0.439509771262568,
         0.902372846844914, 0.000904355304316987, 0.000406366880854969, 0.00307504617568264,
         796.01461216043, 587.387962027542, 958.39543536206))

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
    list(0, 0, 0, 0, 10, 169.673297958982, 65.4815670637923, 338.323832550897, 20, 305.889530851268,
         156.467150129851, 544.963624960565, 30, 452.570480494805, 255.433832552716, 767.40749054871, 40,
         623.936472768971, 358.669901684324, 1049.6786043645, 50, 837.752879915103, 469.326352624902,
         1478.66947877777, 60, 1124.84190047649, 599.687917630095, 2182.74692217635, 70,
         1550.76373306258, 764.128100528935, 3471.09662192728, 80, 2294.39002326397, 1014.67891630845,
         6237.42869907853, 90, 4136.36026557173, 1521.5881119629, 15768.2775869778, 100, "<unicode>",
         "<unicode>", "<unicode>"))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 172.424441958627, 78.079442918414, 323.398604933791, 20, 300.909680488339,
         161.691339609186, 521.805421150368, 30, 449.591290400454, 255.075878554767, 765.886547245803,
         40, 633.607993785386, 353.612257041399, 1112.05288308705, 50, 873.152179932877,
         462.259756587964, 1639.78909870412, 60, 1203.25932879529, 593.435148299612, 2518.03448506981,
         70, 1695.7506642143, 770.192528461462, 4076.26088492157, 80, 2533.63310905872, 1040.94525414763,
         7456.584497726, 90, 4421.61633618319, 1549.25521988017, 17373.9236776236, 100, "<unicode>",
         "<unicode>", "<unicode>"))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 10, 160.794978492893, 49.9721044947362, 345.057734530121, 20, 316.516257430048,
         142.920065689408, 575.161970233245, 30, 483.303879617768, 262.490702616411, 821.346683082848,
         40, 668.354658896766, 387.95752248922, 1103.9944385055, 50, 880.304676917499, 515.410249681893,
         1452.23216271312, 60, 1132.45371871949, 644.790556867686, 1957.64973506705, 70,
         1448.90331582141, 790.028995684793, 2688.79520980906, 80, 1882.79755170067, 966.67645369431,
         3943.61353798275, 90, 2601.21611951073, 1218.92941214365, 6355.41789509446, 100, "<unicode>",
         "<unicode>", "<unicode>"))

})

test_that("ParametricSurvivalAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", dataset, opts)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 1.3792322177338, 0.075787741796219, "", 2.04168520052764, 25.1001212775258, "",
         "scale", 181400750885.405, 1140151.69060426, "", 1108536539302.09, 28861275822297828, "",
         "[scale] log(age)", -4.73616807436202, -7.62877322470244, 0.001331416214734, 1.47584607327324,
         -1.8435629240216, -3.20912062587786, "[scale] rx contrast 1 (Helmert)", 0.313574802993169,
         -0.0557938697786514, 0.0961309076413167, 0.188456867414582, 0.682943475764989, 1.66390754178962,
         "[shape] age", 0.00424115248421963, -0.0422825448843662, 0.85819484993401, 0.0237370164633426,
         0.0507648498528055, 0.178672517279891))

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
    list(3123.83324631979, 705.449455460682, 12154.5516916819, 38.8932, 1, 5848.64761900758,
         1716.87152117626, 18366.4600126079, 38.8932, 2, 1981.67977380203, 578.699988718598,
         6241.15527309941, 42.8499666666667, 1, 3710.23219768121, 1358.02960456864, 9515.4971970232,
         42.8499666666667, 2, 1309.07842551897, 479.714734052865, 3373.02219638204, 46.8067333333333, 1,
         2450.94338038873, 1097.10263934902, 5216.33565822338, 46.8067333333333, 2, 894.586960179107,
         400.156916414608, 1965.05465006294, 50.7635, 1, 1674.90499078681, 876.34888139277,
         3112.23700611058, 50.7635, 2, 629.198812045714, 328.985155276274, 1196.94779871836,
         54.7202666666667, 1, 1178.02771267927, 682.837424443097, 1999.41109417834, 54.7202666666667, 2,
         453.638921750475, 265.339098994579, 770.205909624059, 58.6770333333333, 1, 849.332851780998,
         514.519510977437, 1397.03011036713, 58.6770333333333, 2, 334.181053248964, 205.77718887179,
         536.298623645302, 62.6338, 1, 625.675913944707, 368.744064739076, 1053.74244896142, 62.6338, 2,
         250.872784049159, 153.969452044038, 399.737397370718, 66.5905666666667, 1, 469.700651541344,
         257.677893869897, 837.190736866586, 66.5905666666667, 2, 191.503095884292, 112.773389581682,
         317.775365486417, 70.5473333333333, 1, 358.544786952301, 177.016010066665, 699.442750157027,
         70.5473333333333, 2, 148.372888160419, 80.1385815819326, 264.36658563753, 74.5041, 1,
         277.793553829115, 121.847946535148, 600.541671082664, 74.5041, 2))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(187.67303741196, 193.963520102067, 5, -88.8365187059798))

})

