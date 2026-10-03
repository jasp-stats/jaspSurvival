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
    list(0, 1, 1, 1, 114, 0.837510703478608, 0.693178240969771, 0.964192449060135,
     227, 0.647920773432955, 0.470098117709105, 0.843438556165969,
     341, 0.470336727384008, 0.250904155473358, 0.678041467484831,
     454, 0.326105530081854, 0.0994359634430418, 0.515573892032043,
     568, 0.217234048516009, 0.0330615667534956, 0.388371736964099,
     681, 0.141939013302123, 0.00980299877721949, 0.295459762409128,
     795, 0.0916474779022491, 0.0026277940101676, 0.229572598981349,
     908, 0.0598044075375614, 0.000697699434125319, 0.18510871858919,
     1022, 0.039633433062926, 0.000144626482750973, 0.147912378739373
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
    list(0, 1, 1, 1, 114, 0.800843168790079, 0.757036186077945, 0.841366848997511,
     227, 0.585955189797808, 0.530933333222224, 0.638543877287045,
     341, 0.438969708520623, 0.380922689425097, 0.494939875590877,
     454, 0.339313770061718, 0.281742753283774, 0.395643281551402,
     568, 0.268144943605338, 0.213403629156306, 0.322652814971132,
     681, 0.216598071206132, 0.165712337486659, 0.269106877749925,
     795, 0.177549112582412, 0.130587933496063, 0.227080882957631,
     908, 0.147820745978874, 0.104781991851695, 0.194268958819098,
     1022, 0.124336744478246, 0.0849361531065554, 0.168306498556842
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.853256753765902, 0.806291868098778, 0.889761626998848,
     227, 0.625152143187991, 0.568070921548917, 0.677186853395178,
     341, 0.439822288936137, 0.379807485790561, 0.494789567862438,
     454, 0.312377432026856, 0.253343588001033, 0.367727207553959,
     568, 0.224906337670458, 0.17058472910074, 0.278498849159668,
     681, 0.165360082692482, 0.117102180092034, 0.21575349185885,
     795, 0.12337851476492, 0.081566658504393, 0.169608112575294,
     908, 0.093761486662801, 0.0579075234743802, 0.135718185375931,
     1022, 0.0721109123749698, 0.0416630973870258, 0.109749232586348
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.853294873673174, 0.79747787334471, 0.887355684454728,
     227, 0.64367254169095, 0.582051908134309, 0.694378311044195,
     341, 0.448140998465913, 0.386435015028645, 0.503497137788211,
     454, 0.309037909904176, 0.249362051403086, 0.364158209403286,
     568, 0.214052744856342, 0.160103158147051, 0.26588655707751,
     681, 0.150834548358567, 0.103883877430994, 0.19810879591378,
     795, 0.107633017325534, 0.0681352512572418, 0.149839696451766,
     908, 0.0782231090840424, 0.0453695396438526, 0.115502668438779,
     1022, 0.0575230308219395, 0.0306824864275962, 0.0901867645527847
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
    list(0, 1, 1, 1, 1, 1, 1, 114, 0.789693970235256, 0.552705089020884,
     0.932212127803108, 0.90080983124853, 0.708493516057809, 0.943785420802411,
     227, 0.562380667261367, 0.335140843253013, 0.80912916437891,
     0.752873293474043, 0.517074864346998, 0.841554435557279, 341,
     0.385316601504714, 0.206055324720256, 0.667909876463435, 0.594433203786424,
     0.360678084096817, 0.724020647704619, 454, 0.260151166103601,
     0.118708489828183, 0.53143236297966, 0.448199211702786, 0.238111520842074,
     0.611355816691495, 568, 0.171870992836269, 0.062060196727296,
     0.411528497938918, 0.322794182123546, 0.14737346657931, 0.511036684203508,
     681, 0.111082269993614, 0.0300184347113388, 0.308180703956782,
     0.2246162281039, 0.0883859892455577, 0.42640255412536, 795,
     0.0690706322608892, 0.0123557665160708, 0.225369204120071, 0.151072563758134,
     0.0504660058870758, 0.354945407599891, 908, 0.0413584896548977,
     0.00461719877177426, 0.163838488370058, 0.0996471383253963,
     0.027138446791387, 0.296444425702438, 1022, 0.023509448194203,
     0.00140878959862499, 0.117687076424568, 0.0644848665302737,
     0.0138330043194995, 0.246394098904181))

})

