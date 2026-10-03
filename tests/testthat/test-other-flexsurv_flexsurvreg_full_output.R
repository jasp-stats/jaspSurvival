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
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_subgroup-1")

  plotName <- results[["results"]][["cumulativeHazardPlot"]][["collection"]][["cumulativeHazardPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_subgroup-2")

  plotName <- results[["results"]][["hazardPlot"]][["collection"]][["hazardPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_subgroup-1")

  plotName <- results[["results"]][["hazardPlot"]][["collection"]][["hazardPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_subgroup-2")

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 0, 0, 0, 123, 0.04589999700458,
     0.00386366617703009, 0.189432794796785, 0.000646392050060632,
     0.000104244738970185, 0.00161700208845907, 120.962320563707,
     112.463028134651, 122.870937579384, 0.0448625259766594, 0.00418303480238569,
     0.169622667814664, 246, 0.152491389279767, 0.0308924801181884,
     0.405680613402277, 0.00107373886890784, 0.000340382484763066,
     0.00263243965246739, 232.887788012382, 205.083119308748, 243.96638126639,
     0.141433713201337, 0.0308775861487202, 0.329570251938411, 369,
     0.30779664106479, 0.0914636061997442, 0.73913986020607, 0.00144486067395454,
     0.000569917876893865, 0.00437470806797264, 331.072114221693,
     278.18494376282, 359.857920466171, 0.264935215197203, 0.0890871010499564,
     0.535081793084739, 492, 0.506614930762481, 0.187274291729507,
     1.35695106071609, 0.00178361593168628, 0.000725532655681159,
     0.00689375753089543, 413.349433348712, 327.809562161521, 466.175355292253,
     0.397468257743826, 0.172057662711504, 0.748967185815297, 614,
     0.743562729610374, 0.308809313797536, 2.33628395333164, 0.00209767258614968,
     0.000803411494401664, 0.010093896744193, 479.007552945456, 357.175486631406,
     560.839557965246, 0.524582887932561, 0.266493597302587, 0.906639755275755,
     737, 1.02017931840542, 0.436351885635744, 3.76326225588468,
     0.00239771460058114, 0.000847704999198129, 0.0138576444033621,
     530.250196899194, 369.180396148946, 643.597747660868, 0.639469715339462,
     0.357486065387727, 0.976336767602984, 860, 1.33286139825671,
     0.576407737791226, 5.73905145345665, 0.00268457165097327, 0.00087189752548933,
     0.0190143689765989, 568.44391469234, 374.156583962044, 714.996060586696,
     0.736278431789546, 0.440563423207259, 0.996269375905493, 983,
     1.68014111584834, 0.711297459096726, 8.40072274589348, 0.00296060659822688,
     0.000884391558416839, 0.0248397959006783, 595.925825111859,
     375.956437753229, 773.896141797311, 0.813652322426719, 0.512473522184246,
     0.999648127796691, 1106, 2.06079708851964, 0.85028174527664,
     11.6052349858209, 0.0032275172033282, 0.000888764525281616,
     0.0310728323807804, 615.040871203671, 376.580085654846, 826.233859908366,
     0.872647581723173, 0.573015284260012, 0.999984966358021))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.00869539911641592,
     0.000194236611753215, 0.0742925554778886, 0.000127374047424372,
     5.52197367491308e-06, 0.000622484032184393, 135.60580881558,
     131.101254773357, 135.995103131618, 0.00865770347220152, 0.000180230370257828,
     0.0729985041566993, 273, 0.0348476041675894, 0.00249412029329615,
     0.16218729011154, 0.000254296635735036, 3.28493851897482e-05,
     0.0007527296920745, 269.853570611605, 252.622215936348, 272.862088529607,
     0.0342474182878266, 0.00225999431485069, 0.153134964019796,
     409, 0.0779691465831053, 0.0103122190866363, 0.271127043180658,
     0.00037977800548573, 7.81878132296839e-05, 0.00106224483026595,
     398.587310611234, 362.259222559198, 408.101544461185, 0.0750070347031391,
     0.00960508129966797, 0.23733368726672, 545, 0.138132296400909,
     0.0261550581495062, 0.413428703406209, 0.00050492754866362,
     0.00013879683039026, 0.00156161653567754, 520.850185828311,
     460.107966771503, 541.755949981631, 0.12901654385316, 0.0244227401895821,
     0.335300384925951, 682, 0.215928610519776, 0.0525899708085245,
     0.633119575273381, 0.000630748392215043, 0.000203668733271149,
     0.00233893196519825, 635.81649794337, 546.374755628738, 673.54903350934,
     0.194207175048875, 0.0483652733207341, 0.464635308517705, 818,
     0.31019239715386, 0.0908071527565663, 0.972694274780098, 0.000755454273071759,
     0.000262981121284672, 0.00334518105631028, 740.545691676849,
     614.845620134404, 800.330613251469, 0.266694143308586, 0.0797048544523423,
     0.616138295024081, 954, 0.421404814595403, 0.137662822697459,
     1.46997062787281, 0.000879997534047205, 0.000311154891394086,
     0.00458245638853332, 835.062999182697, 663.026998381792, 921.542444859101,
     0.343875561108497, 0.120425257214457, 0.770512812750181, 1091,
     0.550550288311108, 0.193224770717393, 2.22201225716212, 0.00100531596745529,
     0.000347834995874018, 0.00607676334156464, 919.510389727533,
     693.711268175208, 1037.05804600508, 0.423367591017112, 0.163531714091284,
     0.890490454407098, 1227, 0.695725715127604, 0.255200879205573,
     3.21802818566398, 0.00112959777910214, 0.000370831875673255,
     0.00789468637281803, 992.608544523504, 709.630525197841, 1145.06148644843,
     0.501287606501468, 0.215601854743023, 0.959934544567447))

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_subgroup-1")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_subgroup-2")

  plotName <- results[["results"]][["residualHistogram"]][["collection"]][["residualHistogram_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-7_subgroup-1")

  plotName <- results[["results"]][["residualHistogram"]][["collection"]][["residualHistogram_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-8_subgroup-2")

  plotName <- results[["results"]][["residualVsPredictedPlot"]][["collection"]][["residualVsPredictedPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-9_subgroup-1")

  plotName <- results[["results"]][["residualVsPredictedPlot"]][["collection"]][["residualVsPredictedPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-10_subgroup-2")

  plotName <- results[["results"]][["residualsVsPredictorsPlot"]][["collection"]][["residualsVsPredictorsPlot_table1"]][["collection"]][["residualsVsPredictorsPlot_table1_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-11_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsVsPredictorsPlot"]][["collection"]][["residualsVsPredictorsPlot_table2"]][["collection"]][["residualsVsPredictorsPlot_table2_residualPlotResidualVsPredictors1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-12_residuals-vs-jaspcolumn1")

  plotName <- results[["results"]][["residualsVsTimePlot"]][["collection"]][["residualsVsTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-13_subgroup-1")

  plotName <- results[["results"]][["residualsVsTimePlot"]][["collection"]][["residualsVsTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-14_subgroup-2")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["collection"]][["restrictedMeanSurvivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-15_subgroup-1")

  plotName <- results[["results"]][["restrictedMeanSurvivalTimePlot"]][["collection"]][["restrictedMeanSurvivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-16_subgroup-2")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(104.448725742629, 106.143573815014, 3, -49.2243628713144, 1, 83.5344037495967,
     85.2292518219813, 3, -38.7672018747983, 2))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-17_subgroup-1")

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-18_subgroup-2")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-19_subgroup-1")

  plotName <- results[["results"]][["survivalTimePlot"]][["collection"]][["survivalTimePlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-20_subgroup-2")

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 198.718086748457, 71.9625611931727, 390.351715555494,
     0.2, 306.469681424375, 148.43366402691, 533.76555770831, 0.3,
     401.771801827806, 221.925568899761, 668.220610495705, 0.4, 494.356631593181,
     289.119518582417, 804.265702373883, 0.5, 589.610008817539, 350.91058735453,
     968.191123069013, 0.6, 692.691306791741, 407.90550184131, 1170.1902356379,
     0.7, 810.960120381918, 462.490021708553, 1451.97154670774, 0.8,
     958.901933768744, 524.056303322121, 1861.02604407391, 0.9, 1179.15349846453,
     610.282502437729, 2609.54066293283))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 475.726369440225, 184.405693908027, 876.2871400881,
     0.2, 693.345077123755, 351.384474735918, 1175.34616003414, 0.3,
     877.390496431623, 492.299099313725, 1442.63157438422, 0.4, 1050.74860461096,
     605.843994786315, 1747.07640247955, 0.5, 1224.71518779887, 701.865433395016,
     2075.47883863222, 0.6, 1408.88863141754, 790.610234190409, 2500.49359763505,
     0.7, 1615.84930419667, 883.103940217737, 3052.68155783556, 0.8,
     1869.29043658753, 982.855563352642, 3822.07958783242, 0.9, 2237.44555055393,
     1110.93950691076, 5242.24425606718))

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

  plotName <- results[["results"]][["hazardPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_predicted-hazard")

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 0, 0, 0, 136, 0.0787671006676681,
     0.0183620428581762, 0.195419202720973, 0.000766346062624432,
     0.000265087090800644, 0.00144595905343907, 131.538812172018,
     122.534931638151, 135.161294217719, 0.0757448422105838, 0.0196354276866982,
     0.177948466799427, 273, 0.193738252321238, 0.0720206763410791,
     0.374168846345617, 0.000887713397922583, 0.000451679178286414,
     0.00174848050884091, 251.316575857629, 225.607594680647, 266.130359911803,
     0.176126476969262, 0.0737226308575926, 0.31438901962369, 409,
     0.316887096930438, 0.144696216814908, 0.57694958444138, 0.0009136849228794,
     0.000491217772931061, 0.00188739192746561, 356.769099171478,
     311.844838684151, 387.730735581348, 0.27158700945416, 0.139043437388684,
     0.442121337403061, 545, 0.440478434719357, 0.226901434656353,
     0.812218554847958, 0.000899488297843676, 0.000475757096347559,
     0.00191941652733696, 449.940831440177, 382.677094020361, 499.966027478866,
     0.356271634603091, 0.20481792195699, 0.556016122309656, 682,
     0.561612496447821, 0.308770388046431, 1.04676861415103, 0.000866968142860685,
     0.000445510630607534, 0.0018680395417021, 532.968503200198,
     439.207614832439, 603.782230648967, 0.429711266523461, 0.265097025024604,
     0.649157317554236, 818, 0.676865956486439, 0.387030770992264,
     1.27561631217994, 0.000827255376484067, 0.000416994235731428,
     0.00178138767114203, 606.192139910744, 483.690961621287, 698.880827043627,
     0.491792757280374, 0.319032371913194, 0.723515136008643, 954,
     0.786535317179999, 0.458498386722835, 1.49952615780353, 0.00078546009500889,
     0.000387767643102286, 0.00168859981329864, 671.622170838795,
     521.438535502547, 786.81807905148, 0.544580049321246, 0.365386689563198,
     0.776155225108765, 1091, 0.891283517746075, 0.527506331588933,
     1.71961366065001, 0.000743948826777034, 0.000362667525138325,
     0.00158314227850621, 730.830049115756, 552.095294903438, 868.235772331938,
     0.589870993077716, 0.407877140923672, 0.817734829120917, 1227,
     0.989760907390754, 0.585477543145961, 1.91825042586965, 0.000704623441730175,
     0.000341526981333469, 0.00147586274608895, 783.925488773282,
     578.113976309713, 943.577581967775, 0.628334457115874, 0.444512666240576,
     0.850074439913071))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0.0735522299564721,
     0.015288345934772, 0.187635906837756, 0.000847901116002147,
     0.000292377035595353, 0.00159680269155483, 132.427171663022,
     124.326728752506, 135.466003623171, 0.0709123816816271, 0.0145842574494242,
     0.174822117267193, 273, 0.197442794844509, 0.0786238031587982,
     0.388803673309776, 0.000922438401640329, 0.000497763334334167,
     0.00177785682167472, 252.235762732066, 228.191749095286, 266.38051921133,
     0.179172905173709, 0.0739219823197285, 0.322891338539728, 409,
     0.320941503233119, 0.158925462562404, 0.595020132745327, 0.000887690215003281,
     0.000500854486747798, 0.00180297297235668, 357.207719261908,
     312.985209267357, 386.598047666432, 0.274534312853878, 0.144961870881861,
     0.452101868303848, 545, 0.438213449041613, 0.241457106621796,
     0.827530467600059, 0.000836303717742812, 0.000465651045800898,
     0.00176876704546367, 450.25108319624, 382.863980020037, 497.223832201418,
     0.35481194661295, 0.211893564626612, 0.563540820775831, 682,
     0.549250597444693, 0.316058646523442, 1.04841973860409, 0.000785295280875926,
     0.000424199363442832, 0.00171687087969362, 533.862470384273,
     438.75107778947, 599.903570735339, 0.422617659908137, 0.269501374551571,
     0.651545135693941, 818, 0.652858893321358, 0.389091357669622,
     1.26748535600862, 0.000739192683174463, 0.000390470884185881,
     0.0016445034368779, 608.41655803868, 483.061357761112, 694.429273357216,
     0.479444565044411, 0.319454284826989, 0.718577599214975, 954,
     0.75053776576014, 0.449328887549562, 1.47417453266653, 0.000698068693517688,
     0.000361359610609471, 0.00157859290579119, 675.832945450114,
     519.296694295709, 783.057892037732, 0.52788740152721, 0.36047835196548,
     0.77079190987974, 1091, 0.8436026806958, 0.507457467685191,
     1.69317076951959, 0.000661268714467024, 0.000337009103681548,
     0.00151157493723924, 737.567980225243, 549.882035208189, 865.868662337807,
     0.569841993449342, 0.395894996795193, 0.812311001417778, 1227,
     0.931281594403621, 0.562214970370638, 1.88771603966352, 0.000628743052409768,
     0.000315607791036924, 0.0014550039581301, 793.557504849479,
     574.159186467849, 942.568233112229, 0.605951623569797, 0.427116754439271,
     0.845436402810628))

  table <- results[["results"]][["lifeTimeTable"]][["collection"]][["lifeTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0, 0, "<unicode>", 0, 0, 0, 0, 0, 0, 136, 0.0875154525519651,
     0.0224602578191678, 0.21176277719776, 0.00071303198208108, 0.000289665411418109,
     0.00125986968015631, 130.512485859943, 120.471509821397, 134.871844700843,
     0.0837952861594605, 0.0234070349644658, 0.191368982164715, 273,
     0.189413061630606, 0.0723229618874379, 0.363868808664252, 0.000768794826891753,
     0.000410309739774809, 0.00138867349100741, 249.92427515515,
     223.248444603923, 265.556273946768, 0.172555349540808, 0.0718740912367264,
     0.304875252945215, 409, 0.296443289805187, 0.138663709791334,
     0.5293140747856, 0.000803121942539428, 0.000455915902363928,
     0.00161001391975669, 356.68514148403, 311.265312975658, 387.653922534965,
     0.256542212295394, 0.131459271328476, 0.412133895014955, 545,
     0.407461867575075, 0.210030664107596, 0.726984185926295, 0.000828425854806772,
     0.000462707583572417, 0.00188732426607908, 452.412297409991,
     386.145511998752, 501.175124654976, 0.334663178179284, 0.192320977087081,
     0.519647582386566, 682, 0.522394447164483, 0.284644230581194,
     0.968939131749558, 0.00084874524204922, 0.000452771433729534,
     0.00216043493561115, 538.540320164375, 449.354823565624, 606.57733978982,
     0.406901297124311, 0.251063515080677, 0.626398346324427, 818,
     0.638999977461617, 0.360004233910864, 1.25366029357498, 0.000865586978742005,
     0.000438932686612195, 0.00240282122010745, 614.691067935066,
     498.821849043683, 703.035264947446, 0.472180007902583, 0.30567406733257,
     0.71951823143186, 954, 0.757728760718732, 0.431934655530522,
     1.5712093782516, 0.000880093011809464, 0.000425587509707872,
     0.00265879656281991, 682.388087549097, 534.694849604929, 791.980611646875,
     0.531270183481651, 0.355577955659994, 0.798359197081482, 1091,
     0.879199591719079, 0.503543317604322, 1.93785269716424, 0.000892947452456232,
     0.000413317245118812, 0.00290059326418829, 742.866000480166,
     564.515311845005, 875.307317279014, 0.584884959739624, 0.400731521133512,
     0.859611682592047, 1227, 1.0014297233001, 0.571994675557357,
     2.35191884442127, 0.000904355304316987, 0.000401798620825883,
     0.0031377242305879, 796.01461216043, 585.391970616884, 950.28559471105,
     0.632646148823566, 0.441236227036216, 0.903989242134728))

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
    list(0, 0, 0, 0, 0.1, 169.673297958982, 65.7979872557709, 332.5492411373,
     0.2, 305.889530851268, 157.078503521545, 538.381652022188, 0.3,
     452.570480494805, 257.837220667283, 766.254620595488, 0.4, 623.936472768971,
     363.714257882789, 1064.25181035927, 0.5, 837.752879915103, 473.950018138419,
     1499.41638773067, 0.6, 1124.84190047649, 602.019417332679, 2186.01168129483,
     0.7, 1550.76373306258, 764.058209359428, 3468.06249890526, 0.8,
     2294.39002326397, 1020.01033141574, 6193.02109859067, 0.9, 4136.36026557173,
     1518.23177340763, 15749.4065686652))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 172.424441958627, 77.5144936829402, 323.767694858204,
     0.2, 300.909680488339, 161.604807029638, 525.404264309142, 0.3,
     449.591290400454, 255.370625769295, 769.342332595615, 0.4, 633.607993785386,
     353.619693315823, 1113.32089530312, 0.5, 873.152179932877, 468.659474094634,
     1642.39203477511, 0.6, 1203.25932879529, 599.170512828735, 2489.27289874305,
     0.7, 1695.7506642143, 773.744326709859, 4018.96067564075, 0.8,
     2533.63310905872, 1045.93225382537, 7185.81037975465, 0.9, 4421.61633618319,
     1549.1003288775, 16348.769116068))

  table <- results[["results"]][["survivalTimeTable"]][["collection"]][["survivalTimeTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, 0, 0, 0.1, 160.794978492893, 50.0847381120066, 343.176103281488,
     0.2, 316.516257430048, 143.150539876038, 565.553952982766, 0.3,
     483.303879617768, 262.162146987455, 807.993803477287, 0.4, 668.354658896766,
     386.934864929595, 1095.85031038815, 0.5, 880.304676917499, 514.880896065137,
     1459.70224902919, 0.6, 1132.45371871949, 644.63146219266, 1972.37167377096,
     0.7, 1448.90331582141, 790.349519636671, 2717.46805292634, 0.8,
     1882.79755170067, 964.966288740705, 3985.793301069, 0.9, 2601.21611951073,
     1220.35496273612, 6448.76821143219))

})

