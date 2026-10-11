# Covariate support includes every fitting row, regardless of censoring status.
.sapPredictionContourObserved <- function(fit, settings, axes, records, type) {
  labels <- .sapPredictionFitLabels(fit, settings)
  observed <- .saSafeRbind(lapply(seq_along(fit), function(i) {
    training <- attr(fit[[i]], "dataset")
    scenarios <- .sapModelPredictionScenarios(fit[[i]], settings, timeRole = "panels")
    roles <- data.frame(Line = scenarios[["lines"]], Panel = scenarios[["panels"]])
    evaluations <- if (type %in% c("mean", "median")) "" else
      .sapModelPredictionEvaluationLabels(unique(records[[i]][["at"]]), type)
    .saSafeRbind(lapply(which(!duplicated(roles)), function(j) {
      rows <- scenarios[["observedRows"]][[j]]
      if (!length(rows)) return(NULL)
      .saSafeRbind(lapply(evaluations, function(evaluation) {
        data.frame(x = training[[axes[1L]]][rows], y = training[[axes[2L]]][rows],
          ObservedRow = rownames(training)[rows], Subgroup = attr(fit[[i]], "subgroup"),
          Panel = .sapPredictionSeriesLabel(if (length(fit) > 1L) labels[i], scenarios[["panels"]][j], evaluation))
      }))
    }))
  }))
  if (is.null(observed)) return(NULL)
  return(observed[!duplicated(observed[c("Subgroup", "ObservedRow", "Panel")]), , drop = FALSE])
}
.sapAddContourObservedLayer <- function(plot, observed) {
  if (is.null(observed)) return(plot)
  keep <- is.finite(observed[["x"]]) & is.finite(observed[["y"]])
  for (position in c("x", "y")) {
    scale <- plot$scales$get_scales(position)
    values <- suppressWarnings(scale$get_transformation()$transform(observed[[position]]))
    bounds <- scale$get_limits()
    tolerance <- 64 * .Machine$double.eps * max(abs(bounds))
    keep <- keep & is.finite(values) & values >= bounds[1L] - tolerance & values <= bounds[2L] + tolerance
  }
  observed <- observed[keep, , drop = FALSE]
  if (!nrow(observed)) return(plot)
  return(plot + ggplot2::geom_point(data = observed, ggplot2::aes(x = x, y = y),
    inherit.aes = FALSE, shape = 21, fill = "grey30", colour = "white", alpha = .65,
    size = 1.5, stroke = .25, show.legend = FALSE))
}
