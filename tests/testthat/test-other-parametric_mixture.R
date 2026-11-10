context("Other: parametric_mixture")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.
# Saved examples retain the softened starts; sharper starts have a separate regression test.

test_that("ParametricMixtureSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", dataset, opts)

  plotName <- results[["results"]][["densityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_density-plot")

  table <- results[["results"]][["mixtureClassificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 29, 0.64658679289263, 0.379712648358674, 0.12719298245614,
     "Component 2", 199, 0.659178574188333, 0.620287351641326, 0.87280701754386
    ))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_mixture-components")

  table <- results[["results"]][["mixtureComponentsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 403.762327233183, 194.167769332777, "Mean", 150.817029592108,
     839.603902609372, "", 281.272526447276, 145.606033982904, "Median",
     94.4891377988584, 543.344475293672, "Component 2", 379.235410762752,
     281.717604428765, "Mean", 57.515586267485, 510.509441069592,
     "", 337.58656232715, 234.150082525412, "Median", 63.016111199854,
     486.716407847066))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2317.08811883001, 2334.23484697478, 5, -1153.54405941501))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.837510703480614, 0.693178241999883, 0.964192445632236,
     227, 0.647920773433917, 0.470098117233437, 0.843438547539175,
     341, 0.470336727384092, 0.250904154302862, 0.678041447686086,
     454, 0.326105530081986, 0.0994359626832619, 0.515573877676234,
     568, 0.217234048516388, 0.033061567271267, 0.388371730838835,
     681, 0.141939013302487, 0.00980299924939749, 0.29545974842458,
     795, 0.0916474779023893, 0.00262779419067274, 0.229572594575408,
     908, 0.0598044075375195, 0.000697699477494885, 0.185108716130106,
     1022, 0.0396334330629067, 0.000144626511614093, 0.147912376107185
    ))

})

test_that("ParametricMixtureSurvivalAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", dataset, opts)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("meanlog", 1, 5.66330496231285, 5.5104357266533, 1, 0.077995941183288, 5.8161741979724, "sdlog",
         "", 1.09763927737438, 0.982843254396318, "", 0.0618650889168497, 1.22584346776128,
         "mixing probability", 2, 0.0419319818976908, 0.0203271415738075, 1, 0.0152989090238738,
         0.0845186157029274, "meanlog", "", 2.55281443000953, 2.1258117343574, "", 0.217862521464814,
         2.97981712566165, "sdlog", "", 0.464759151135476, 0.235321980234484, "", 0.161379818087902,
         0.91789584784616, "mixing probability", "", 0.958068018102309, 0.915481384297391, 2,
         0.0152989090237894, 0.979672858426111, "meanlog", "", 5.74737596884194, 5.6266560193655, "",
         0.0615929427421434, 5.86809591831838, "sdlog", "", 0.822274281225095, 0.718001390844649, "",
         0.0568900532114804, 0.941690367436266, "mixing probability", 3, 0.024822869410622,
         0.00898992103431514, 1, 0.0127429209352505, 0.0666648298705659, "meanlog", "", 2.47713576041158,
         2.34376714615668, "", 0.068046461724248, 2.61050437466649, "sdlog", "", 0.0875795098846912,
         0.0208327772777691, "", 0.0641674459046985, 0.368178013395634, "mixing probability", "",
         0.122032232597472, 0.00528869028210207, 2, 0.178400584844997, 0.784186553550579, "meanlog", "",
         4.68440079240088, 2.39465288901569, "", 1.16826019327214, 6.97414869578606, "sdlog", "",
         1.25378608054861, 0.69938705339938, "", 0.373404060778383, 2.24765318164931,
         "mixing probability", "", 0.853144897991907, 0.263965586503987, 3, 0.178024496596676,
         0.989485440529733, "meanlog", "", 5.82826645326626, 5.60354754576, "", 0.114654610635102,
         6.05298536077252, "sdlog", "", 0.737767435572176, 0.546555002944111, "", 0.112923290149935,
         0.995875595427316))

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_1-component")

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_2-components")

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_3-components")

  table <- results[["results"]][["mixtureClassificationTable"]][["collection"]][["mixtureClassificationTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 9, 0.962328133834908, 0.0419319818976908, 0.0394736842105263,
     "Component 2", 219, 0.995892501282354, 0.958068018102309, 0.960526315789474
    ))

  table <- results[["results"]][["mixtureClassificationTable"]][["collection"]][["mixtureClassificationTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 6, 0.903794959811028, 0.024822869410622, 0.0263157894736842, "Component 2", 13,
         0.700814033660999, 0.122032232597472, 0.0570175438596491, "Component 3", 209, 0.913221623907488,
         0.853144897991907, 0.916666666666667))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-4_1-component")

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-5_2-components")

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-6_3-components")

  table <- results[["results"]][["mixtureComponentsTable"]][["collection"]][["mixtureComponentsTable_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 14.3079448126458, 8.50368671832856, "Mean", 3.79835583707273,
     24.0739448127218, "", 12.8431992501414, 8.37969681553823, "Median",
     2.79805177262505, 19.6842165784538, "Component 2", 439.414634756668,
     376.466352743144, "Mean", 34.6639032231182, 512.8883891785,
     "", 313.367294944131, 277.731833183171, "Median", 19.3012138653369,
     353.57510305935))

  table <- results[["results"]][["mixtureComponentsTable"]][["collection"]][["mixtureComponentsTable_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 11.9528631723007, 10.3717478053755, "Mean", 0.865289112347331, 13.775010798247,
         "", 11.9071107111166, 10.4204179526768, "Median", 0.810236753337174, 13.6059115988113,
         "Component 2", 237.552953395952, 16.4055613259537, "Mean", 323.9469982458, 3439.77292492055, "",
         108.245391410458, 10.9643915284382, "Median", 126.458781936838, 1068.64705909242, "Component 3",
         446.043663637946, 380.331682711619, "Mean", 36.3134772749663, 523.209619046691, "",
         339.769162520223, 271.387461129622, "Median", 38.9561010567566, 425.381052312344))

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-7_1-component")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-8_2-components")

  plotName <- results[["results"]][["probabilityPlot"]][["collection"]][["probabilityPlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-9_3-components")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2342.5381106112, 4.11672991406406e-08, 2349.3968018691, 6.34807913840848e-05, 1, 2,
         -1169.2690553056, 2313.01678814594, 0.15671527439833, 2330.16351629071, 0.969499147055834, 2, 5,
         -1151.50839407297, 2309.65104022291, 0.843284664698821, 2337.08580525455, 0.0304362737630769, 3,
         8, -1146.82552011146))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-10_1-component")

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-11_2-components")

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["collection"]][["survivalProbabilityPlot_table3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-12_3-components")

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
    list(0, 1, 1, 1, 114, 0.853256753765902, 0.806291868115377, 0.88976162699609,
     227, 0.625152143187991, 0.568070921576394, 0.677186853403971,
     341, 0.439822288936137, 0.379807485780646, 0.494789567823326,
     454, 0.312377432026856, 0.253343588033634, 0.367727207578119,
     568, 0.224906337670458, 0.170584729151939, 0.27849884913719,
     681, 0.165360082692482, 0.117102180114839, 0.215753491863617,
     795, 0.12337851476492, 0.0815666585316651, 0.169608112557453,
     908, 0.093761486662801, 0.0579075234822319, 0.135718185387342,
     1022, 0.0721109123749698, 0.0416630973917502, 0.109749232576366
    ))

  table <- results[["results"]][["survivalProbabilityTable"]][["collection"]][["survivalProbabilityTable_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 114, 0.852937377531562, 0.761555112101355, 0.933549208601753, 227,
         0.637617730766408, 0.540807704156315, 0.82494535201896, 341, 0.44687493177921,
         0.371772778878938, 0.711042766291698, 454, 0.311652261436807, 0.251026960405899,
         0.61013781256109, 568, 0.218719375289416, 0.165763390190972, 0.530169925229065, 681,
         0.156270974224592, 0.109812667307416, 0.472441051083198, 795, 0.113132794516898,
         0.0733216162275949, 0.426612977336454, 908, 0.083432104367875, 0.0497869686713181,
         0.387164814933167, 1022, 0.062284883052136, 0.0342948465243255, 0.356632658879363))

})

test_that("ParametricMixtureSurvivalAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", dataset, opts)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  # These structural references use C collation; keep ordering local to this test.
  withr::local_collate("C")

  # The native numerical covariance differs slightly across platforms.
  testthat::expect_equal(table[[4]][["z"]], 71.2508404587343, tolerance = 1e-5)
  jaspTools::expect_equal_tables(table,
    list("mixing probability", 0.0452392919141223, 0.020704275921776, 1, "", 0.0177842300785239,
         0.0959987072368488, "", "shape", 16.9266341005184, 9.12094017780696, "", "", 5.33989394196343,
         31.4124351643014, "", "scale", 12.331037205885, 11.6644103434889, "", "", 0.349661032715778,
         13.0357621255838, "", "sex (2)", 3.35422767086837, 3.26195977074925, "", 0, 0.0470763242829536,
         3.44649557098749, 71.2508404587343, "mixing probability", 0.954760708085878, 0.904001292764074,
         2, "", 0.0177842300782897, 0.979295724078008, "", "shape", 1.42229396855244, 1.25574236172884,
         "", "", 0.0903783687834261, 1.61093564622251, "", "scale", 376.711167060456, 329.086090779955,
         "", "", 25.978050455435, 431.228506351367, "", "sex (2)", 0.351310647203602, 0.109894750094068,
         "", 0.00434235485811584, 0.123173639420822, 0.592726544313135, 2.85215772510668))

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_jaspcolumn1-1", tolerance = 1e-5)

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_jaspcolumn1-2", tolerance = 1e-5)

  table <- results[["results"]][["mixtureClassificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 6, 0.929908135868084, 0.0452392919141223, 0.0263157894736842, "Component 2", 222,
         0.978668494812203, 0.954760708085878, 0.973684210526316))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_jaspcolumn1-1", tolerance = 1e-5)

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_jaspcolumn1-2", tolerance = 1e-5)

  table <- results[["results"]][["mixtureComponentsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 11.95093558179, 11.20263353288, "Mean", 0.394270311753149, 12.749221945081, "",
         12.0669028584416, 11.3401615173231, "Median", 0.382428496136813, 12.8402178727908,
         "Component 2", 342.516269696808, 299.609227505941, "Mean", 23.389416921275, 391.568030075742,
         "", 291.135082670224, 252.290950627444, "Median", 21.2717261496274, 335.959875495344))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2290.67909136352, 2314.6845107662, 7, -1137.99218862975))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-6_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 1, 114, 0.795351790383576, 0.732345648939922, 0.841902059169772, 1, 227,
         0.586937941979342, 0.515686301846004, 0.652256538586866, 1, 341, 0.400831151230015,
         0.330361542990152, 0.471174884515812, 1, 454, 0.259167357232877, 0.195637727284674,
         0.326140455058523, 1, 568, 0.158882665500387, 0.106390025045765, 0.217008423561103, 1, 681,
         0.0937081486079273, 0.0540731062841998, 0.141908726427713, 1, 795, 0.0529083598164005,
         0.0252692680172993, 0.0909960618554452, 1, 908, 0.0289806579449851, 0.0111594757138767,
         0.057629435334542, 1, 1022, 0.0152776977181491, 0.004596094925611, 0.0356043165629757, 1, 0, 1,
         1, 1, 2, 114, 0.899832103635723, 0.860092578218274, 0.931674664256669, 2, 227,
         0.755920398137214, 0.687186503953029, 0.816093665061017, 2, 341, 0.589785747256151,
         0.498240232852019, 0.671820335213482, 2, 454, 0.432805921999487, 0.333283184153118,
         0.526550457109675, 2, 568, 0.321632709363732, 0.222628664057938, 0.421391811418163, 2, 681,
         0.233473137905639, 0.143611382139679, 0.331654973713421, 2, 795, 0.16504929802379,
         0.0883011930731028, 0.25800571619783, 2, 908, 0.114552354978377, 0.0521873031211332,
         0.197909559331285, 2, 1022, 0.0776786746291624, 0.0296239325487406, 0.150181359256487, 2))

})

