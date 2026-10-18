# Capture the actual platform-specific sampling state without changing analyses or snapshots.
stopifnot(getRversion() == "4.5.2")
output <- Sys.getenv("MIXTURE_DIAGNOSTIC_OUTPUT", file.path(getwd(), ".work", "mixture-plot-diagnostics"))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
diagnostic <- new.env(parent = emptyenv())
diagnostic$phase <- diagnostic$case <- ""
diagnostic$events <- list()
diagnostic$record <- function(kind, value) {
  if (kind == "quantileSolver" && length(diagnostic$events) > 400L)
    return(invisible(NULL))
  diagnostic$events[[length(diagnostic$events) + 1L]] <- list(
    phase = diagnostic$phase, case = diagnostic$case, kind = kind, value = value)
  if (kind == "grid")
    cat("GRID", diagnostic$phase, diagnostic$case, "scouts", value$scoutCount,
      "anchors", length(value$anchors), "selected", length(value$selected),
      "first times", paste(head(value$selected, 6L), collapse = ","), "\n")
}
assign(".mixturePlotDiagnostic", diagnostic, envir = .GlobalEnv)
checkpoint <- function() saveRDS(diagnostic$events, file.path(output, "events.rds"))
cat("R:", R.version.string, "\n")
cat("Commit:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n")
print(Sys.info())
print(utils::sessionInfo())
for (package in c("jaspTools", "flexsurv", "rstpm2", "ggplot2", "jaspGraphs"))
  cat(package, as.character(utils::packageVersion(package)), find.package(package), "\n")
jaspTools::setPkgOption("module.dirs", getwd())
# Resolve the module exactly as runAnalysis does, before installing any tracing.
jaspTools:::reinstallChangedModules()
jaspTools::setPkgOption("reinstall.modules", FALSE)

sourceIdentity <- function() {
  ns <- asNamespace("jaspSurvival")
  files <- c(.sapAdaptivePlotTimes = "parametricsurvival-plotgrid.R",
    .sapModelPredictionPlotRecords = "parametricsurvival-scenarios.R",
    .sapmMixtureDistribution = "parametricmixturesurvival-distributions.R")
  for (name in names(files)) {
    expressions <- parse(file.path("R", files[[name]]), keep.source = FALSE)
    assignment <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
      identical(x[[2L]], as.name(name)), as.list(expressions))
    stopifnot(length(assignment) == 1L)
    expected <- eval(assignment[[1L]][[3L]], ns)
    actual <- get(name, ns)
    same <- identical(deparse(formals(actual)), deparse(formals(expected))) &&
      identical(deparse(body(actual)), deparse(body(expected)))
    cat("SOURCE", diagnostic$phase, name, "matches checkout:", same, "\n")
    diagnostic$record("source", list(name = name, matches = same,
      path = getNamespaceInfo(ns, "path"), body = deparse(body(actual))))
  }
}

runPhase <- function(phase) {
  diagnostic$phase <- phase
  sourceIdentity()
  ns <- asNamespace("jaspSurvival")
  trace(".sapAdaptivePlotTimes", where = ns, print = FALSE,
    tracer = quote(.diagnosticInputAnchors <- anchors), exit = quote({
      state <- get(".mixturePlotDiagnostic", envir = .GlobalEnv)
      field <- function(name) get0(name, envir = parent.frame(), inherits = FALSE)
      state$record("grid", list(times = times, anchors = anchors,
        inputAnchors = .diagnosticInputAnchors, scoutCount = scoutCount,
        selected = returnValue(), minimum = minimum, maximum = maximum,
        scouts = field("scouts"), raw = field("raw"), transformedX = field("x"),
        retained = field("retained"),
        selectionAttempt = field("selected")))
    }))
  trace(".sapModelPredictionValues", where = ns, print = FALSE, exit = quote({
    if (type == "quantile" || (type == "survival" && length(at) <= 140L)) {
      state <- get(".mixturePlotDiagnostic", envir = .GlobalEnv)
      value <- returnValue()
      state$record("predictions", list(type = type, at = at,
        confidenceIntervals = options[["predictionsDisplayConfidenceIntervals"]],
        family = attr(fit, "family"), components = attr(fit, "components"),
        values = value, warnings = attr(value, "predictionWarnings")))
    }
  }))
  trace("qgeneric", where = asNamespace("flexsurv"), print = FALSE, exit = quote({
    arguments <- list(...)
    if (length(p) <= 50L && !is.null(arguments[["targetLog"]])) {
      state <- get(".mixturePlotDiagnostic", envir = .GlobalEnv)
      state$record("quantileSolver", list(p = p, arguments = arguments, roots = returnValue()))
    }
  }))
  on.exit({
    untrace(".sapAdaptivePlotTimes", where = ns)
    untrace(".sapModelPredictionValues", where = ns)
    untrace("qgeneric", where = asNamespace("flexsurv"))
    checkpoint()
  }, add = TRUE)
  fixture <- file.path("tests", "testthat", "jaspfiles", "other", "parametric_mixture.jasp")
  options <- jaspTools::analysisOptions(fixture)
  dataset <- jaspTools::extractDatasetFromJASPFile(fixture)
  for (i in seq_along(options)) {
    diagnostic$case <- paste0("analysis-", i)
    cat("CASE", phase, diagnostic$case, "\n")
    opts <- options[[i]]
    opts[["predictionsOutputTableFormat"]] <- "long"
    opts[["mixtureStartTails"]] <- TRUE
    opts[["mixtureStartMembershipProbabilities"]] <- "c(0.95)"
    encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
    set.seed(1)
    results <- jaspTools::runAnalysis("ParametricMixtureSurvivalAnalysis", encoded$dataset,
      encoded$options, encodedDataset = TRUE, view = FALSE)
    diagnostic$record("status", list(status = results$status, error = results$results$errorMessage))
    stopifnot(results$status == "complete")
    for (key in names(results$state$figures)) {
      figure <- results$state$figures[[key]]$obj
      plots <- if (inherits(figure, "jaspGraphsPlot")) figure$subplots else list(figure)
      for (j in seq_along(plots)) {
        if (!inherits(plots[[j]], "ggplot")) next
        built <- ggplot2::ggplot_build(plots[[j]])
        diagnostic$record("plotLayers", list(key = key, subplot = j,
          title = plots[[j]]$labels$title, layers = built$data))
      }
    }
    checkpoint()
  }
}

tryCatch({
  runPhase("automatic-install")
  if (identical(Sys.getenv("MIXTURE_DIAGNOSTIC_FRESH_INSTALL"), "true")) {
    pkgload::unload("jaspSurvival", quiet = TRUE)
    utils::install.packages(".", repos = NULL, type = "source", lib = .libPaths()[1L])
    runPhase("explicit-source-install")
  }
}, finally = {
  checkpoint()
  capture.output(str(diagnostic$events, max.level = 4L), file = file.path(output, "summary.txt"))
  cat("Diagnostics saved:", output, "\n")
})
