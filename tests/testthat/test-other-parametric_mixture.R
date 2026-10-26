context("Other: parametric_mixture")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.
# Saved examples retain the softened starts; sharper starts have a separate regression test.

test_that("ParametricMixtureSurvivalAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "parametric_mixture.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

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

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("meanlog", 1, 5.66330496231285, 5.5104357266533, 1, 0.077995941183288,
     5.8161741979724, "sdlog", "", 1.09763927737438, 0.982843254396318,
     "", 0.0618650889168497, 1.22584346776128, "mixing probability",
     2, 0.0419319818976908, 0.0203271415738075, 1, 0.0152989090238738,
     0.0845186157029274, "meanlog", "", 2.55281443000953, 2.1258117343574,
     "", 0.217862521464814, 2.97981712566165, "sdlog", "", 0.464759151135476,
     0.235321980234484, "", 0.161379818087902, 0.91789584784616,
     "mixing probability", "", 0.958068018102309, 0.915481384297391,
     2, 0.0152989090237894, 0.979672858426111, "meanlog", "", 5.74737596884194,
     5.6266560193655, "", 0.0615929427421434, 5.86809591831838, "sdlog",
     "", 0.822274281225095, 0.718001390844649, "", 0.0568900532114804,
     0.941690367436266, "mixing probability", 3, 0.0467714549745484,
     0.0253486251082547, 1, 0.0144393877922691, 0.0847254635678667,
     "meanlog", "", 2.63791512752655, 2.30090841093042, "", 0.171945361881343,
     2.97492184412269, "sdlog", "", 0.511298581587074, 0.314452583187614,
     "", 0.126814925861716, 0.831369349498958, "mixing probability",
     "", 0.0349771118641286, 0.0153654573686496, 2, 0.0145124806604978,
     0.0776461298635736, "meanlog", "", 4.08121297858824, 4.01662185327525,
     "", 0.0329552613325933, 4.14580410390123, "sdlog", "", 0.0748094930289235,
     0.0409104759146439, "", 0.0230371144727401, 0.136797730217587,
     "mixing probability", "", 0.918251433161323, 0.868655148253932,
     3, 0.0202873895393218, 0.95019377896721, "meanlog", "", 5.80998002631512,
     5.69867740039331, "", 0.0567880975363563, 5.92128265223693,
     "sdlog", "", 0.730315270774973, 0.636591798998152, "", 0.0511780387758697,
     0.837837363859396))

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
    list("Component 1", 11, 0.96299849727299, 0.0467714549745484, 0.0482456140350877,
     "Component 2", 10, 0.781684417437957, 0.0349771118641286, 0.043859649122807,
     "Component 3", 207, 0.999184084619548, 0.918251433161323, 0.907894736842105
    ))

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
    list("Component 1", 15.936757718253, 10.88512805695, "Mean", 3.09984405493184,
     23.3327752545946, "", 13.9840182996614, 9.98324722743145, "Median",
     2.40448708737166, 19.5880922660049, "Component 2", 59.3831911299057,
     55.6361591105348, "Mean", 1.97476598874806, 63.3825814928189,
     "", 59.2172555668689, 55.513256787035, "Median", 1.95152013314779,
     63.168395439751, "Component 3", 435.57204958598, 380.331682711619,
     "Mean", 30.138695601765, 498.835671611366, "", 333.61246213783,
     298.472380325982, "Median", 18.9452170499208, 372.889695093763
    ))

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
    list(2342.5381106112, 4.11672991406406e-08, 2349.3968018691, 6.34807913840848e-05,
     1, 2, -1169.2690553056, 2313.01678814594, 0.105931738015936,
     2330.16351629071, 0.953009500321554, 2, 5, -1151.50839407297,
     2308.75081379351, 0.894068220816765, 2336.18557882515, 0.0469270188870621,
     3, 8, -1146.37540689676))

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
    list(0, 1, 1, 1, 114, 0.853294873673588, 0.797477873390684, 0.887355684397888,
     227, 0.643672541692452, 0.582051908265637, 0.694378310936078,
     341, 0.448140998466652, 0.386435015013531, 0.503497137787394,
     454, 0.309037909904071, 0.24936205134372, 0.364158209406098,
     568, 0.214052744855705, 0.160103158172554, 0.265886557090959,
     681, 0.15083454835768, 0.103883877411438, 0.198108795914851,
     795, 0.107633017324572, 0.0681352512410208, 0.149839696446349,
     908, 0.0782231090831037, 0.0453695396040253, 0.115502668428995,
     1022, 0.057523030821072, 0.0306824864321537, 0.0901867645283517
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
  # These structural references use C collation; keep ordering local to this test.
  withr::local_collate("C")

  # The native numerical covariance differs slightly across platforms.
  testthat::expect_equal(table[[4]][["z"]], 76.7851604118328, tolerance = 1e-5)
  jaspTools::expect_equal_tables(table,
    list("mixing probability", 0.0494420798510104, 0.022430246874223,
        1, "", 0.0196245295728078, 0.105473467196689, "", "shape",
        16.4665915461521, 9.0819144531651, "", "", 4.99928655309317,
        29.8558898067258, "", "scale", 12.3184731646619, 11.6482099776925,
        "", "", 0.351632883160154, 13.0273047446004, "", "jaspColumn1 (2)",
        4.07653037447639, 3.9724757280707, "", 0, 0.0530900808517186,
        4.18058502088208, table[[4]][["z"]], "mixing probability",
        0.95055792014899, 0.894526532802347, 2, "", 0.0196245295730529,
        0.977569753126001, "", "shape", 1.4313262304408, 1.26482205888837,
        "", "", 0.0903139637789481, 1.6197494055001, "", "scale",
        377.171294005735, 329.762991063903, "", "", 25.84926640688,
        431.39524105782, "", "jaspColumn1 (2)", 0.286327130026688,
        0.0425519456788598, "", 0.0213304560900401, 0.12437737951855,
        0.530102314374516, 2.30208363558571))

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_jaspcolumn1-1", tolerance = 1e-5)

  plotName <- results[["results"]][["densityPlot"]][["collection"]][["densityPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_jaspcolumn1-2", tolerance = 1e-5)

  table <- results[["results"]][["mixtureClassificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 6, 0.9379816341186, 0.0494420798510104, 0.0263157894736842,
        "Component 2", 222, 0.97457249938828, 0.95055792014899, 0.973684210526316))

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_jaspcolumn1-1", tolerance = 1e-5)

  plotName <- results[["results"]][["mixtureComponentPlot"]][["collection"]][["mixtureComponentPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_jaspcolumn1-2", tolerance = 1e-5)

  table <- results[["results"]][["mixtureComponentsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 11.9292494574304, 11.1843964558912, "Mean",
        0.39241675895493, 12.7237078172998, "", 12.0473178219962,
        11.3219204738332, "Median", 0.381718511438219, 12.8191915002075,
        "Component 2", 342.61641597311, 299.948711896238, "Mean",
        23.2493984553474, 391.353600927838, "", 291.965074414256,
        253.247714537069, "Median", 21.1925951555549, 336.601674110053))

  plotName <- results[["results"]][["probabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_probability-plot")

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2289.98437725949, 2313.98979666217, 7, -1137.99218862975))

  plotName <- results[["results"]][["survivalProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-6_predicted-survival-probability")

  table <- results[["results"]][["survivalProbabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 1, 1, 1, 1, 114, 0.793655846498437, 0.728774902929971,
        0.840343393127909, 1, 227, 0.586148873496409, 0.514221667786646,
        0.651531297923887, 1, 341, 0.399983294107099, 0.329523450746404,
        0.469926816156937, 1, 454, 0.258047252118154, 0.194394616231003,
        0.324887709079898, 1, 568, 0.157627197599082, 0.105464352941985,
        0.215287185815823, 1, 681, 0.0925177327190453, 0.0533320163906341,
        0.140543015500363, 1, 795, 0.0519173418737562, 0.0247390826048521,
        0.0896630727662604, 1, 908, 0.0282328158958783, 0.010841968907,
        0.0565297265315335, 1, 1022, 0.0147585423506136, 0.00438528099108106,
        0.0346353696693368, 1, 0, 1, 1, 1, 2, 114, 0.892728354713212,
        0.85086370768027, 0.926742173595164, 2, 227, 0.739058311676962,
        0.667418966777631, 0.802501231342259, 2, 341, 0.584554322995495,
        0.493186779667153, 0.670915892241984, 2, 454, 0.449459393485462,
        0.352345307775868, 0.5484845880805, 2, 568, 0.336992718707572,
        0.242933999450726, 0.439087345797564, 2, 681, 0.237398110552619,
        0.149324876840603, 0.336387379136573, 2, 795, 0.138570973398052,
        0.0686619514131509, 0.2333641625405, 2, 908, 0.0921002731353567,
        0.0368924696746921, 0.172780065685677, 2, 1022, 0.0598785499273145,
        0.0196297198546932, 0.128093947994948, 2))

})

