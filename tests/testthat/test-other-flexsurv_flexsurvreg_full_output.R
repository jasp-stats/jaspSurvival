context("Other: flexsurv_flexsurvreg_full_output")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  # GUI defaults from qml_components/SurvivalExport.qml.
  opts[c("exportResidualsCoxSnell", "exportResidualsResponse", "exportFittedMean", "exportFittedMedian")] <- rep(list(FALSE), 4)
  opts[["exportColumnPrefix"]] <- ""
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)
  expect_identical(results[["status"]], "complete")

  table <- results[["results"]][["censoringSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 1, "Events", 5, 2, "", 6, 1, "Censored", 8, 2, ""))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 0.00294583404147664, -0.195092282202688, 0.101399586243753,
     "scale", -0.0284318665535529, 1.84744099223018, -0.195092282202688,
     "jaspColumn1", 0.000449368523120733, -0.0284318665535529, 0.00294583404147664
    ))

  table <- results[["results"]][["coefficientsCovarianceMatrixTable"]][["collection"]][["coefficientsCovarianceMatrixTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("shape", 0.00753960491518431, -0.472986932576419, 0.116539009633363,
     "scale", -0.379053598831464, 22.8507010819253, -0.472986932576419,
     "jaspColumn1", 0.00628895179079035, -0.379053598831464, 0.00753960491518431
    ))

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
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_subgroup-1-")

  plotName <- results[["results"]][["cumulativeHazardPlot"]][["collection"]][["cumulativeHazardPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_subgroup-2-")

  plotName <- results[["results"]][["hazardPlot"]][["collection"]][["hazardPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_subgroup-1-")

  plotName <- results[["results"]][["hazardPlot"]][["collection"]][["hazardPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_subgroup-2-")

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 0, 0, 0, 123, 0.04589999700458,
     0.00518290779653138, 0.172165997192324, 0.000646392050060632,
     0.000107174895488599, 0.00157850659788787, 120.962320563707,
     113.360195599485, 122.893560184502, 0.0448625259766594, 0.177871009139788,
     0.00378437860822389, 246, 0.152491389279767, 0.0364775278882595,
     0.399533243184474, 0.00107373886890784, 0.000349990579111217,
     0.00255356417522903, 232.887788012382, 207.906306731773, 244.254285144193,
     0.141433713201337, 0.361961446846558, 0.029279914395244, 369,
     0.30779664106479, 0.104731011291696, 0.735061812929682, 0.00144486067395454,
     0.000623684011992211, 0.00413285956819355, 331.072114221693,
     279.987118855627, 360.144652793727, 0.264935215197203, 0.544524132317749,
     0.0816518244757919, 492, 0.506614930762481, 0.193915048242253,
     1.27201896454453, 0.00178361593168628, 0.000762100494741992,
     0.00676607126497844, 413.349433348712, 328.04321387505, 466.141575349322,
     0.397468257743826, 0.730504762546907, 0.165682398686094, 614,
     0.743562729610374, 0.307177967775138, 2.21830952647087, 0.00209767258614968,
     0.000838967272475044, 0.00990780026755102, 479.007552945456,
     353.761895029687, 560.670710125836, 0.524582887932561, 0.887058184322678,
     0.246908607904536, 737, 1.02017931840542, 0.441400386020114,
     3.4985759202225, 0.00239771460058114, 0.000889466169569609,
     0.0147070424076869, 530.250196899194, 364.189144905456, 644.723282580467,
     0.639469715339462, 0.961776079977611, 0.35034717781829, 860,
     1.33286139825671, 0.583874225476029, 5.22202121633127, 0.00268457165097327,
     0.000921084234116285, 0.0197574864280285, 568.44391469234, 368.387032389084,
     718.142127389873, 0.736278431789546, 0.993314070863662, 0.433239057875749,
     983, 1.68014111584834, 0.712048905984369, 7.49684709778472,
     0.00296060659822688, 0.000916899554505154, 0.0253274910552036,
     595.925825111859, 369.176363783698, 782.063398809863, 0.813652322426719,
     0.999309068142921, 0.50729065801857, 1106, 2.06079708851964,
     0.859772382279156, 10.6598894550945, 0.0032275172033282, 0.000948436337725259,
     0.0323396301259641, 615.040871203671, 369.176434167847, 838.721974700201,
     0.872647581723173, 0.999963800337782, 0.580953893721555))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.00869539911641592,
     0.000163591921207248, 0.075833980659704, 0.000127374047424372,
     4.34206091843378e-06, 0.000566202171464824, 135.60580881558,
     131.137848194669, 135.996255019032, 0.00865770347220152, 0.0738984002687497,
     0.000270264507821527, 273, 0.0348476041675894, 0.00233104283606026,
     0.167004352036461, 0.000254296635735036, 3.09866401590254e-05,
     0.000750644715090301, 269.853570611605, 252.446447166309, 272.883869525714,
     0.0342474182878266, 0.153013404816842, 0.00335416453666559,
     409, 0.0779691465831053, 0.00978718727580946, 0.276159793778596,
     0.00037977800548573, 7.99058740345776e-05, 0.00104247943865914,
     398.587310611234, 362.314419007479, 408.158347680888, 0.0750070347031391,
     0.240762378922097, 0.0131636443626882, 545, 0.138132296400909,
     0.0271489431524225, 0.40542297887141, 0.00050492754866362, 0.000139116780246265,
     0.00150421277405772, 520.850185828311, 455.992846042998, 542.136232067324,
     0.12901654385316, 0.34207944195578, 0.0295193619982062, 682,
     0.215928610519776, 0.0528622251515454, 0.642410818854988, 0.000630748392215043,
     0.000214647627499562, 0.00224551505201704, 635.81649794337,
     543.716521197863, 673.787372045649, 0.194207175048875, 0.463257490839974,
     0.0554257124063335, 818, 0.31019239715386, 0.0933776256977882,
     0.950963389568438, 0.000755454273071759, 0.000265285268849026,
     0.00299209804779529, 740.545691676849, 611.311300488523, 801.043427524644,
     0.266694143308586, 0.613282813080783, 0.0880661469006182, 954,
     0.421404814595403, 0.14063933020653, 1.35160548204994, 0.000879997534047205,
     0.000300508986626228, 0.00428183597246934, 835.062999182697,
     662.096090285491, 922.413759263119, 0.343875561108497, 0.748213615704298,
     0.125911520822415, 1091, 0.550550288311108, 0.197487975509884,
     1.98346871640157, 0.00100531596745529, 0.000342148896234624,
     0.00582309140786014, 919.510389727533, 693.533392833943, 1037.00261928984,
     0.423367591017112, 0.854032435030784, 0.164573650042433, 1227,
     0.695725715127604, 0.268278463485721, 2.84855495032016, 0.00112959777910214,
     0.00036728730288059, 0.00783466615910583, 992.608544523504,
     711.900131593995, 1146.37926295165, 0.501287606501468, 0.927809016715587,
     0.21652105858515))

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_subgroup-1-")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_subgroup-2-")

  plotName <- results[["results"]][["residualHistogram"]][["collection"]][["residualHistogram_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-7_subgroup-1-")

  plotName <- results[["results"]][["residualHistogram"]][["collection"]][["residualHistogram_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-8_subgroup-2-")

  plotName <- results[["results"]][["residualVsPredictedPlot"]][["collection"]][["residualVsPredictedPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-9_subgroup-1-")

  plotName <- results[["results"]][["residualVsPredictedPlot"]][["collection"]][["residualVsPredictedPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-10_subgroup-2-")

  plotName <- results[["results"]][["residualsVsPredictorsPlot"]][["collection"]][["residualsVsPredictorsPlot_table1"]][["collection"]][["residualsVsPredictorsPlot_table1_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-11_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsVsPredictorsPlot"]][["collection"]][["residualsVsPredictorsPlot_table2"]][["collection"]][["residualsVsPredictorsPlot_table2_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-12_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsVsTimePlot"]][["collection"]][["residualsVsTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-13_subgroup-1-")

  plotName <- results[["results"]][["residualsVsTimePlot"]][["collection"]][["residualsVsTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-14_subgroup-2-")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["collection"]][["restrictedMeanSurvivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-15_subgroup-1-")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["collection"]][["restrictedMeanSurvivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-16_subgroup-2-")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(104.448725742629, 106.143573815014, 3, -49.2243628713144, 1, 83.5344037495967,
     85.2292518219813, 3, -38.7672018747983, 2))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-17_subgroup-1-")

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-18_subgroup-2-")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-19_subgroup-1-")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-20_subgroup-2-")

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 198.718086748457, 63.4725227988521, 401.196573803081,
     0.2, 306.469681424375, 136.18448198518, 553.605761401134, 0.3,
     401.771801827806, 217.237849271519, 676.136494691836, 0.4, 494.356631593181,
     287.111296446282, 805.752824878585, 0.5, 589.610008817539, 346.901671445617,
     972.789477292467, 0.6, 692.691306791741, 395.152878171957, 1198.58529169667,
     0.7, 810.960120381918, 450.47792620339, 1460.33829373806, 0.8,
     958.901933768744, 506.373627989931, 1895.8464592472, 0.9, 1179.15349846453,
     574.359209474288, 2684.66174884533))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 475.726369440225, 187.024827596037, 870.148235394596,
     0.2, 693.345077123755, 355.890593689999, 1171.32527299015, 0.3,
     877.390496431623, 490.351748846787, 1455.66803221102, 0.4, 1050.74860461096,
     596.93775814311, 1754.5784455397, 0.5, 1224.71518779887, 718.76654463732,
     2090.94489927072, 0.6, 1408.88863141754, 801.674121799586, 2464.27385300016,
     0.7, 1615.84930419667, 872.162007522935, 2904.09421489461, 0.8,
     1869.29043658753, 970.981396216079, 3616.15517040957, 0.9, 2237.44555055393,
     1096.01510661062, 4797.56582520057))

})

test_that("ParametricSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "flexsurv_flexsurvreg_full_output.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  # GUI defaults from qml_components/SurvivalExport.qml.
  opts[c("exportResidualsCoxSnell", "exportResidualsResponse", "exportFittedMean", "exportFittedMedian")] <- rep(list(FALSE), 4)
  opts[["exportColumnPrefix"]] <- ""
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)
  expect_identical(results[["status"]], "complete")

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

  plotName <- results[["results"]][["hazardPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_predicted-hazard")

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 0, 0, 0, 136, 0.0787671006676681,
     0.0198355155676322, 0.191272082109924, 0.000766346062624432,
     0.000292233004984781, 0.00147330171798636, 131.538812172018,
     123.11836819092, 135.087931900818, 0.0757448422105838, 0.184459392483454,
     0.0199060917463678, 273, 0.193738252321238, 0.0777853676876944,
     0.365656569869113, 0.000887713397922583, 0.000450578207201718,
     0.00166021016841059, 251.316575857629, 227.937868876546, 265.83645234961,
     0.176126476969262, 0.319594542098561, 0.0716457516347682, 409,
     0.316887096930438, 0.16085280366424, 0.579813534570023, 0.0009136849228794,
     0.000504515567127737, 0.00180401584474648, 356.769099171478,
     313.813043712536, 386.981454318891, 0.27158700945416, 0.446008445152784,
     0.137550260482084, 545, 0.440478434719357, 0.248929833729064,
     0.817046176569816, 0.000899488297843676, 0.000486351480523289,
     0.00188916914166246, 449.940831440177, 386.524378262462, 498.002147687403,
     0.356271634603091, 0.547506447124283, 0.192882899338385, 682,
     0.561612496447821, 0.325179069389642, 1.04990323883438, 0.000866968142860685,
     0.000460058665604493, 0.00179977937574146, 532.968503200198,
     446.032543473951, 601.693184180569, 0.429711266523461, 0.64237049919039,
     0.260423756878666, 818, 0.676865956486439, 0.40129135331535,
     1.27134679342061, 0.000827255376484067, 0.000438481898413588,
     0.00174197983887224, 606.192139910744, 491.150231614746, 696.515962128421,
     0.491792757280374, 0.704737661487135, 0.315626484074999, 954,
     0.786535317179999, 0.47763902611202, 1.50828769155137, 0.00078546009500889,
     0.000411272915508252, 0.00161130226769085, 671.622170838795,
     528.7932390848, 783.607504069692, 0.544580049321246, 0.757108241109543,
     0.360275669073197, 1091, 0.891283517746075, 0.545779163281654,
     1.71217081517802, 0.000743948826777034, 0.00038495615931931,
     0.00151755746825601, 730.830049115756, 565.739321130454, 863.754077684936,
     0.589870993077716, 0.80024911805187, 0.393894646104124, 1227,
     0.989760907390754, 0.607638690106926, 1.93342897958307, 0.000704623441730175,
     0.000361507992617402, 0.00142186677311166, 783.925488773282,
     588.911289055881, 934.547974466646, 0.628334457115874, 0.834370974855691,
     0.429001343722895))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.0735522299564721,
     0.0151006963007285, 0.182530876901187, 0.000847901116002147,
     0.000284263531021557, 0.00155586850416169, 132.427171663022,
     124.252768761601, 135.470490386725, 0.0709123816816271, 0.174412996606942,
     0.0138725484155415, 273, 0.197442794844509, 0.0788527941454962,
     0.373112465737653, 0.000922438401640329, 0.000471090013087454,
     0.00174866273839828, 252.235762732066, 228.9316115082, 266.503554871511,
     0.179172905173709, 0.31620061253502, 0.075655954903935, 409,
     0.320941503233119, 0.159388795430671, 0.569726350561466, 0.000887690215003281,
     0.000495013323572624, 0.00178020762629554, 357.207719261908,
     315.189031525075, 386.472615903106, 0.274534312853878, 0.44421333192908,
     0.151234194139668, 545, 0.438213449041613, 0.241114841829257,
     0.785818157771664, 0.000836303717742812, 0.00045373381717889,
     0.00172836110914599, 450.25108319624, 386.778087719519, 495.833362803114,
     0.35481194661295, 0.553139893220755, 0.216242333344407, 682,
     0.549250597444693, 0.309892740562347, 0.992866893123935, 0.000785295280875926,
     0.000413793802839709, 0.00167690020854825, 533.862470384273,
     443.820243865954, 596.389959328887, 0.422617659908137, 0.63824410950474,
     0.268305656627637, 818, 0.652858893321358, 0.372912094752961,
     1.19853706278664, 0.000739192683174463, 0.000374968417050247,
     0.00162512133503113, 608.41655803868, 491.508554750627, 690.324370635586,
     0.479444565044411, 0.707257642187049, 0.318694392367018, 954,
     0.75053776576014, 0.431263951407142, 1.40634375019177, 0.000698068693517688,
     0.000344725985671841, 0.0015631631610838, 675.832945450114,
     526.992425106566, 778.00152940632, 0.52788740152721, 0.757812121970525,
     0.357532195401626, 1091, 0.8436026806958, 0.49314014685479,
     1.58613563821198, 0.000661268714467024, 0.000321607476111367,
     0.00149409053236093, 737.567980225243, 553.85891708704, 860.592790107381,
     0.569841993449342, 0.79768578530116, 0.393198154072953, 1227,
     0.931281594403621, 0.545333657351487, 1.76170509558429, 0.000628743052409768,
     0.000300742878138749, 0.00143453826595191, 793.557504849479,
     572.501219028489, 940.377690687424, 0.605951623569797, 0.832318184972551,
     0.428733446562548))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 0, 0, 0, 136, 0.0875154525519651,
     0.022532675732504, 0.215256937533358, 0.00071303198208108, 0.000299761769082286,
     0.00126978332065619, 130.512485859943, 120.629789704555, 134.923315951467,
     0.0837952861594605, 0.188051220278761, 0.023154994305248, 273,
     0.189413061630606, 0.0753140545977767, 0.370386740920402, 0.000768794826891753,
     0.00040744605342883, 0.0013982418637348, 249.92427515515, 223.624010121193,
     266.107113662121, 0.172555349540808, 0.305088237911072, 0.0688208232621794,
     409, 0.296443289805187, 0.136799048767115, 0.534113902400601,
     0.000803121942539428, 0.000456596661112121, 0.00157773444357789,
     356.68514148403, 311.049853437784, 387.906042989536, 0.256542212295394,
     0.407388740786562, 0.124973857609148, 545, 0.407461867575075,
     0.208452942379619, 0.741677225960351, 0.000828425854806772,
     0.000463131131268881, 0.00186785090981454, 452.412297409991,
     384.661049808546, 501.136437100673, 0.334663178179284, 0.513443233322925,
     0.187964491645051, 682, 0.522394447164483, 0.289986235772435,
     0.974801132589732, 0.00084874524204922, 0.000456499616227843,
     0.00209108857447024, 538.540320164375, 443.707710145719, 606.252721031308,
     0.406901297124311, 0.613829966174656, 0.250272248885136, 818,
     0.638999977461617, 0.364200998373631, 1.27888650773873, 0.000865586978742005,
     0.000451759866563011, 0.00239558782381669, 614.691067935066,
     493.300038363666, 704.781881851426, 0.472180007902583, 0.705237700211657,
     0.305150032075995, 954, 0.757728760718732, 0.437655306203371,
     1.61222561927993, 0.000880093011809464, 0.000437608500386702,
     0.00270620484909158, 682.388087549097, 533.28120361378, 795.400859516585,
     0.531270183481651, 0.780782551547548, 0.348316860313732, 1091,
     0.879199591719079, 0.500047857607422, 1.95690080175481, 0.000892947452456232,
     0.000424046497626564, 0.0030708166206864, 742.866000480166,
     560.307496465695, 880.903694818635, 0.584884959739624, 0.835904124069427,
     0.38877617408261, 1227, 1.0014297233001, 0.560410367887337,
     2.35652505170549, 0.000904355304316987, 0.000411467624300331,
     0.00336762982740017, 796.01461216043, 574.914171833538, 958.122015667454,
     0.632646148823566, 0.883722383124209, 0.426719609661876))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_probability-plot")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-4_predicted-restricted-mean-survival-time")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(198.709408968454, 201.225602044497, 2, "Log-logistic", -97.354704484227,
     198.243484085311, 200.759677161354, 2, "Log-normal", -97.1217420426555,
     199.907802094262, 202.423995170304, 2, "Weibull", -97.9539010471308
    ))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-5_predicted-failure-probability")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-6_log-logistic-distribution")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-7_log-normal-distribution")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-8_weibull-distribution")

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 169.673297958982, 61.8072021795583, 341.836275407744,
     0.2, 305.889530851268, 154.964412586194, 538.806866247412, 0.3,
     452.570480494805, 252.095834405866, 758.445388683309, 0.4, 623.936472768971,
     358.222830393139, 1066.92483301375, 0.5, 837.752879915103, 466.572883814226,
     1504.18972393348, 0.6, 1124.84190047649, 601.254287218707, 2241.72187807747,
     0.7, 1550.76373306258, 762.176625738325, 3633.53919936021, 0.8,
     2294.39002326397, 1018.76070514133, 7018.4515196819, 0.9, 4136.36026557173,
     1518.29128082694, 18172.957097776))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 172.424441958627, 76.6653945432377, 342.119845682322,
     0.2, 300.909680488339, 163.281407319541, 536.275430342072, 0.3,
     449.591290400454, 259.695773135883, 782.521859985656, 0.4, 633.607993785386,
     350.088358356474, 1146.24925091308, 0.5, 873.152179932877, 462.758821534212,
     1700.97074873094, 0.6, 1203.25932879529, 591.494774149103, 2531.81825032935,
     0.7, 1695.7506642143, 766.869735717386, 4159.17859893258, 0.8,
     2533.63310905872, 1021.47110762768, 7389.4067201387, 0.9, 4421.61633618319,
     1488.62195858835, 16813.7526880168))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 160.794978492893, 48.9053770560189, 339.885959470321,
     0.2, 316.516257430048, 140.96151783192, 549.713392030051, 0.3,
     483.303879617768, 263.455308536148, 798.13698327488, 0.4, 668.354658896766,
     394.489888179549, 1096.93301039446, 0.5, 880.304676917499, 530.385318217048,
     1478.29291313031, 0.6, 1132.45371871949, 677.454953853096, 1942.14894113387,
     0.7, 1448.90331582141, 837.176060893678, 2614.04378058709, 0.8,
     1882.79755170067, 1009.75120261128, 3720.73540749251, 0.9, 2601.21611951073,
     1278.93402964551, 6038.63630324337))

})

