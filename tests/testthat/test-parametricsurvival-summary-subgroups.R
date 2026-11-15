# Capture the table payload without initializing the Desktop output bridge.
.subgroupSummaryTable <- function(fits, criterion = "aic") {
  summary <- jaspSurvival:::.sapSummaryTableFun
  environment(summary) <- new.env(parent = environment(summary))
  environment(summary)$createJaspTable <- function() {
    table <- new.env(parent = emptyenv())
    table$columns <- character(0)
    table$addColumnInfo <- function(name, ...) table$columns <- c(table$columns, name)
    table$addFootnote <- function(...) NULL
    table$setData <- function(data) table$data <- data
    return(table)
  }
  options <- list(
    analysisType = "parametric", censoringType = "right", eventStatus = "event", timeToEvent = "time",
    subgroup = "group", distribution = "weibull", modelTerms = list(list(components = list())),
    modelSummaryAicWeights = TRUE, modelSummaryBicWeights = TRUE, modelSummaryRankModels = TRUE,
    modelSummaryRankModelsBy = criterion, outputFormattingAlwaysDisplayModelInformation = FALSE
  )
  return(summary(fits, options))
}

.subgroupSummaryFit <- function(group, scale = 1, distribution = "weibull") {
  data <- data.frame(time = (1:20) * scale, event = 1)
  fit <- flexsurv::flexsurvreg(survival::Surv(time, event) ~ 1, data = data, dist = distribution)
  attr(fit, "subgroup") <- attr(fit, "subgroupLabel") <- group
  attr(fit, "modelTitle") <- "Model 1"
  attr(fit, "distribution") <- distribution
  attr(fit, "components") <- 1L
  return(fit)
}

test_that("Single-model subgroup summaries do not compare different datasets", {
  fits <- list(.subgroupSummaryFit("A"), .subgroupSummaryFit("B", .5))
  comparisonColumns <- c("aicWeight", "bicWeight", "rank")
  for (includeFullDataset in c(FALSE, TRUE)) {
    if (includeFullDataset)
      fits <- c(list(.subgroupSummaryFit("fullDataset", 2)), fits)
    for (criterion in c("aic", "bic", "logLik")) {
      table <- .subgroupSummaryTable(fits, criterion)
      expect_false(any(comparisonColumns %in% table$columns))
      expect_false(any(comparisonColumns %in% names(table$data)))
      expect_equal(table$data$subgroup, vapply(fits, attr, character(1), which = "subgroup"))
      expect_equal(table$data$aic, vapply(fits, AIC, numeric(1)))
      expect_equal(table$data$bic, vapply(fits, BIC, numeric(1)))
    }
  }
})

test_that("Competing fits within a subgroup retain weights and ranks", {
  for (group in c("A", "B", "fullDataset")) {
    fits <- list(.subgroupSummaryFit(group), .subgroupSummaryFit(group, distribution = "exp"))
    for (criterion in c("aic", "bic", "logLik")) {
      table <- .subgroupSummaryTable(fits, criterion)
      expect_true(all(c("aicWeight", "bicWeight", "rank") %in% table$columns))
      expect_equal(sum(table$data$aicWeight), 1)
      expect_equal(sum(table$data$bicWeight), 1)
      expect_equal(table$data$rank, 1:2)
      expect_equal(table$data[[criterion]], sort(table$data[[criterion]], decreasing = criterion == "logLik"))
      expect_equal(table$data$aicWeight[1] / table$data$aicWeight[2], exp(-diff(rev(table$data$aic)) / 2))
      expect_equal(table$data$bicWeight[1] / table$data$bicWeight[2], exp(-diff(rev(table$data$bic)) / 2))
    }
  }
})
