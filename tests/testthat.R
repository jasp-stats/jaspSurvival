library(jaspTools)
library(testthat)

if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
  source(".github/scripts/ci-snapshot-diagnostics.R")

jaspTools::runTestsTravis(module = getwd())
