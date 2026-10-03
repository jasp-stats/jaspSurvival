context("Other: parametric_mixture")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ParametricMixtureSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["mixtureClassificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 29, 0.646586793108261, 0.379712648571903, 0.12719298245614,
     "Component 2", 199, 0.659178573976143, 0.620287351428097, 0.87280701754386
    ))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_mixture-components")

  table <- results[["results"]][["mixtureComponentsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 403.762327209258, 194.167770787689, "Mean", 150.817028027356,
     839.603896218665, "", 281.272526459097, 145.60603228303, "Median",
     94.4891394842526, 543.344481682601, "Component 2", 379.235410765163,
     281.717604790717, "Mean", 57.5155860204829, 510.509440420179,
     "", 337.586562338091, 234.150083214382, "Median", 63.0161107006711,
     486.716406446487))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2317.08811883001, 2334.23484697478, 5, -1153.54405941501))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.837510703478608, 0.702251687976086, 0.964083561977706,
     227, 0.647920773432955, 0.467207442183441, 0.850936561583383,
     341, 0.470336727384008, 0.262703745022653, 0.677181246834331,
     454, 0.326105530081854, 0.105709908196189, 0.515362223457239,
     568, 0.217234048516009, 0.0397334228397795, 0.385337077812005,
     681, 0.141939013302123, 0.0109433174638086, 0.295145533065843,
     795, 0.0916474779022491, 0.002708020291503, 0.228493619973953,
     908, 0.0598044075375614, 0.000825922522472359, 0.175675044165969,
     1022, 0.039633433062926, 0.000158888612760356, 0.14109961461
    ))

})

test_that("ParametricMixtureSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("meanlog", 1, 5.66330496231285, 5.5104357266533, 1, 0.077995941183288,
     5.8161741979724, "sdlog", "", 1.09763927737438, 0.982843254396318,
     "", 0.0618650889168497, 1.22584346776128, "mixing probability",
     2, 0.0419319818976908, 0.02032714157593, 1, 0.0152989090216891,
     0.0845186156946802, "meanlog", "", 2.55281443000953, 2.12581173438851,
     "", 0.217862521448943, 2.97981712563054, "sdlog", "", 0.464759151135476,
     0.235321980245863, "", 0.161379818076435, 0.917895847801773,
     "mixing probability", "", 0.958068018102309, 0.915481384305639,
     2, 0.0152989090216046, 0.979672858423988, "meanlog", "", 5.74737596884194,
     5.62665601938274, "", 0.0615929427333476, 5.86809591830114,
     "sdlog", "", 0.822274281225095, 0.718001390843184, "", 0.0568900532123362,
     0.941690367438187, "mixing probability", 3, 0.0467714549754397,
     0.0253486250613667, 1, 0.0144393878361561, 0.0847254637181388,
     "meanlog", "", 2.6379151275315, 2.30090841148649, "", 0.171945361600151,
     2.97492184357651, "sdlog", "", 0.511298581597061, 0.314452583228209,
     "", 0.126814925835611, 0.831369349424108, "mixing probability",
     "", 0.034977111861682, 0.0153654573619545, 2, 0.0145124806658921,
     0.0776461298848838, "meanlog", "", 4.08121297858849, 4.01662185326487,
     "", 0.0329552613380178, 4.14580410391211, "sdlog", "", 0.074809493029287,
     0.0409104758838338, "", 0.0230371145017827, 0.13679773032194,
     "mixing probability", "", 0.918251433162878, 0.868655147905893,
     3, 0.0202873896565957, 0.950193779113536, "meanlog", "", 5.80998002631198,
     5.69867740038328, "", 0.0567880975398691, 5.92128265224067,
     "sdlog", "", 0.730315270780268, 0.63659179900518, "", 0.0511780387748292,
     0.837837363862296))

  table <- results[["results"]][["mixtureClassificationTable"]][["collection"]][["mixtureClassificationTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 9, 0.962328133834908, 0.0419319818976908, 0.0394736842105263,
     "Component 2", 219, 0.995892501282354, 0.958068018102309, 0.960526315789474
    ))

  table <- results[["results"]][["mixtureClassificationTable"]][["collection"]][["mixtureClassificationTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 11, 0.96299849727195, 0.0467714549754397, 0.0482456140350877,
     "Component 2", 10, 0.781684417418316, 0.034977111861682, 0.043859649122807,
     "Component 3", 207, 0.999184084619584, 0.918251433162878, 0.907894736842105
    ))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_2-components")

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_3-components")

  table <- results[["results"]][["mixtureComponentsTable"]][["collection"]][["mixtureComponentsTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 14.3079448126458, 8.50368671871836, "Mean", 3.79835583673811,
     24.0739448116183, "", 12.8431992501414, 8.37969681579889, "Median",
     2.79805177242122, 19.6842165778415, "Component 2", 439.414634756668,
     376.466352738374, "Mean", 34.6639032259585, 512.888389184998,
     "", 313.367294944131, 277.731833187959, "Median", 19.3012138625806,
     353.575103053255))

  table <- results[["results"]][["mixtureComponentsTable"]][["collection"]][["mixtureComponentsTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 15.9367577184132, 10.8851280656995, "Mean", 3.09984404850892,
     23.3327752363088, "", 13.9840182997305, 9.98324723298146, "Median",
     2.40448708345234, 19.588092255309, "Component 2", 59.3831911299222,
     55.6361591097157, "Mean", 1.97476598920305, 63.3825814937871,
     "", 59.2172555668837, 55.5132567864598, "Median", 1.95152013346889,
     63.1683954404371, "Component 3", 435.572049586294, 380.33168271656,
     "Mean", 30.1386955990602, 498.835671605606, "", 333.61246213678,
     298.472380322963, "Median", 18.9452170510474, 372.889695095189
    ))

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_1-component")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-4_2-components")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-5_3-components")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2342.5381106112, 4.11672991406406e-08, 2349.3968018691, 6.34807913840848e-05,
     1, 2, -1169.2690553056, 2313.01678814594, 0.105931738015936,
     2330.16351629071, 0.953009500321554, 2, 5, -1151.50839407297,
     2308.75081379351, 0.894068220816765, 2336.18557882515, 0.0469270188870621,
     3, 8, -1146.37540689676))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-6_1-component")

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-7_2-components")

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-8_3-components")

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.800843168790079, 0.755393217251182, 0.843166993230825,
     227, 0.585955189797808, 0.531726056801115, 0.639948820744406,
     341, 0.438969708520623, 0.382839399098367, 0.495826574020597,
     454, 0.339313770061718, 0.280585736334241, 0.395815638707972,
     568, 0.268144943605338, 0.210840635365966, 0.321492692026101,
     681, 0.216598071206132, 0.16239730622488, 0.267926610698257,
     795, 0.177549112582412, 0.12818152256303, 0.227243829448239,
     908, 0.147820745978874, 0.101810052551378, 0.193225881754145,
     1022, 0.124336744478246, 0.0827482628867492, 0.166935842923031
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.853256753765902, 0.808594502964876, 0.889038858348488,
     227, 0.625152143187991, 0.568624631678363, 0.67619408435501,
     341, 0.439822288936137, 0.379372262264166, 0.498020452678339,
     454, 0.312377432026856, 0.251876091057452, 0.369654851944467,
     568, 0.224906337670458, 0.168549711302577, 0.28242145654686,
     681, 0.165360082692482, 0.114045823806663, 0.219077543053392,
     795, 0.12337851476492, 0.0794617478041046, 0.173494153147548,
     908, 0.093761486662801, 0.0561150639297111, 0.13898867542592,
     1022, 0.0721109123749698, 0.0403207554581007, 0.112840152496229
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.853294873673174, 0.802126096299809, 0.887351665782474,
     227, 0.64367254169095, 0.584675261630825, 0.691818147509647,
     341, 0.448140998465913, 0.390115717753789, 0.497741473290304,
     454, 0.309037909904176, 0.251772337016469, 0.359234972274371,
     568, 0.214052744856342, 0.16134431582515, 0.260572776108395,
     681, 0.150834548358567, 0.104565780840641, 0.194971056432119,
     795, 0.107633017325534, 0.068035130769656, 0.148009999492631,
     908, 0.0782231090840424, 0.046423298627256, 0.113747344936568,
     1022, 0.0575230308219395, 0.0318476944456022, 0.0883689496633302
    ))

})

test_that("ParametricMixtureSurvivalAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("mixing probability", 0.655691945006062, 0.0256586181370358, 1,
     "", 0.493114616985993, 0.992791006752066, "", "shape", 1.34192760465771,
     1.06120580810848, "", "", 0.160692809669193, 1.69690900896229,
     "", "scale", 246.481011744193, 100.711653216279, "", "", 112.556294774281,
     603.235943510666, "", "jaspColumn2 (2)", 0.731221613397204,
     -0.252427777665525, "", 0.145118943986272, 0.501871156215946,
     1.71487100445993, 1.45699071233847, "mixing probability", 0.344308054993938,
     0.00720899324793435, 2, "", 0.493114616985993, 0.974341381862964,
     "", "shape", 1.89917115461318, 0.838026380521219, "", "", 0.792747264504092,
     4.30398273652367, "", "scale", 603.719557309404, 214.82546578949,
     "", "", 318.278134316468, 1696.62056841537, "", "jaspColumn2 (2)",
     -0.109518617076801, -1.18554213519553, "", 0.841881926670181,
     0.549001678911585, 0.966504901041928, -0.199486852743192))

  table <- results[["results"]][["mixtureClassificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 188, 0.726037140989046, 0.655691945006062, 0.824561403508772,
     "Component 2", 40, 0.674930566363172, 0.344308054993938, 0.175438596491228
    ))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_jaspcolumn2-1")

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_jaspcolumn2-2")

  table <- results[["results"]][["mixtureComponentsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 226.264342863169, 94.5016690764998, "Mean", 100.791820536375,
     541.742313671292, "", 187.571620093285, 73.9545875208835, "Median",
     89.0703577011027, 475.739421229069, "Component 2", 535.726083725898,
     195.020009246649, "Mean", 276.210364002291, 1471.65635922673,
     "", 497.762998369773, 154.00822890797, "Median", 297.931364240102,
     1608.79716819628))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2308.96077758689, 2332.96619698957, 7, -1147.48038879345))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 1, 1, 1, 114, 0.789693970235256, 0.531989113637994,
     0.919792179739499, 0.90080983124853, 0.705418287052194, 0.942906225832476,
     227, 0.562380667261367, 0.321417643366797, 0.792688964575633,
     0.752873293474043, 0.507015139718168, 0.838024742141765, 341,
     0.385316601504714, 0.196781852355722, 0.650000516162147, 0.594433203786424,
     0.363014692922902, 0.71664759335144, 454, 0.260151166103601,
     0.119718840364167, 0.512925630341282, 0.448199211702786, 0.23033617348336,
     0.607307847455033, 568, 0.171870992836269, 0.0623900301067598,
     0.394977976437593, 0.322794182123546, 0.143157859531556, 0.500647658147566,
     681, 0.111082269993614, 0.0283964899065143, 0.300016340477532,
     0.2246162281039, 0.0841041533974055, 0.411638586242057, 795,
     0.0690706322608892, 0.0123136990154399, 0.222573132752334, 0.151072563758134,
     0.0491578124508745, 0.33322597964996, 908, 0.0413584896548977,
     0.00499219203624484, 0.16244556449806, 0.0996471383253963, 0.0279410680951379,
     0.277685418445678, 1022, 0.023509448194203, 0.00172034586371316,
     0.116713605019737, 0.0644848665302737, 0.0134145863961176, 0.231486418859469
    ))

})

