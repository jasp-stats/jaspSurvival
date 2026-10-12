# Reserve canvas room for endpoint tick labels without extending data axes.
.sapPlotCanvasPadding <- function(plot) {
  if (inherits(plot, "patchwork"))
    plot$patches$plots <- lapply(plot$patches$plots, .sapPlotCanvasPadding)
  margin <- ggplot2::calc_element("plot.margin", ggplot2::theme_get() + plot$theme)
  if (all(grid::unitType(margin) == "points")) {
    margin <- as.numeric(margin)
  } else {
    if (grDevices::dev.cur() == 1L) {
      grDevices::pdf(file = NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
    }
    margin <- grid::convertUnit(margin, "pt", valueOnly = TRUE)
  }
  return(plot + ggplot2::theme(plot.margin = ggplot2::margin(
    t = max(margin[1L], 0) + 8, r = max(margin[2L], 0) + 10, b = margin[3L], l = margin[4L], unit = "pt")))
}

# A single colour category uses gray, including its confidence bands.
.sapPlotPalette <- function(palette, seriesCount) {
  return(if (seriesCount == 1L) function(n) rep("grey40", n) else palette)
}

# Colours describe predictor lines; line types identify percentiles or merged fits.
.sapPredictionPlotEncoding <- function(fit, data, options, modelIndex, percentileLines = FALSE) {
  subgroups <- vapply(fit, function(model) attr(model, "subgroupLabel"), character(1))
  distributions <- vapply(fit, function(model) attr(model, "distribution"), character(1))
  field <- if (length(unique(subgroups)) > 1L) "subgroup" else if (
    length(unique(distributions)) > 1L && length(unique(data[["Line"]])) > 1L) "distribution" else NULL
  percentileLines <- percentileLines && length(unique(data[["at"]])) > 1L
  colourTitle <- NULL
  if (percentileLines) {
    # Keep percentile keys independent of predictor and merged-model keys.
    labels <- .sapPredictionFitLabels(fit, options)
    otherLines <- length(setdiff(options[["predictionsSeparateLines"]],
      options[["predictionsLifetimeTimeId"]])) > 0L || any(nzchar(labels))
    field <- if (otherLines) "percentile" else NULL
    if (!otherLines) colourTitle <- gettext("Lifetime percentile")
  }
  labels <- .sapPredictionFitLabels(fit, options, exclude = field)
  lines <- if (percentileLines) data[["ConditionLine"]] else data[["Line"]]
  colours <- vapply(seq_len(nrow(data)), function(i)
    .sapPredictionSeriesLabel(labels[modelIndex[i]], lines[i]), character(1))
  percentileLabels <- if (percentileLines) .sapModelPredictionPercentileLabels(data[["at"]])
  if (percentileLines && is.null(field)) colours <- percentileLabels
  colours[!nzchar(colours)] <- gettext("Prediction")
  rowOrder <- if (is.factor(data[["Series"]])) order(data[["Series"]]) else seq_len(nrow(data))
  data[["Colour"]] <- factor(colours, levels = unique(colours[rowOrder]))
  data[["Type"]] <- ""
  typeLabels <- typeTitle <- NULL
  if (!is.null(field)) {
    types <- if (field == "percentile") percentileLabels else if (field == "subgroup") subgroups else distributions
    levels <- unique(types)
    data[["Type"]] <- factor(if (field == "percentile") types else types[modelIndex], levels = levels)
    typeTitle <- if (field == "percentile") gettext("Lifetime percentile") else if (field == "subgroup") gettext("Subgroup") else gettext("Distribution")
    typeLabels <- levels
    if (field == "subgroup") typeLabels <- vapply(match(levels, types), function(i) {
      if (subgroups[i] == gettext("Full dataset")) return(subgroups[i])
      variables <- options[["subgroup"]]
      values <- vapply(attr(fit[[i]], "dataset")[1L, variables, drop = FALSE], as.character, character(1))
      if (length(values) == 1L) return(unname(values))
      return(paste(paste0(jaspBase::decodeColNames(variables), " = ", values), collapse = ", "))
    }, character(1))
  }
  return(list(data = data, field = field, title = typeTitle, labels = typeLabels, colourTitle = colourTitle))
}
.sapPredictionLayerEncoding <- function(layer, data) {
  index <- match(as.character(layer[["Series"]]), as.character(data[["Series"]]))
  layer[["Colour"]] <- as.character(data[["Colour"]])[index]
  layer[["Type"]] <- data[["Type"]][index]
  return(layer)
}
.sapPredictionTypeScale <- function(encoding, points) {
  levels <- levels(encoding[["data"]][["Type"]])
  if (points) {
    shapes <- unique(c(16, 17, 15, 18, 3, 7, 8, 0:25, 65:90, 97:122, 48:57))
    return(ggplot2::scale_shape_manual(name = encoding[["title"]], limits = levels,
      labels = encoding[["labels"]], values = rep_len(shapes, length(levels))))
  }
  lines <- scales::linetype_pal()(min(length(levels), 13L))
  if (length(levels) > length(lines)) {
    segments <- as.character(as.hexmode(1:15))
    lines <- unique(c(lines, as.vector(outer(segments, segments, paste0))))
  }
  return(ggplot2::scale_linetype_manual(name = encoding[["title"]], limits = levels,
    labels = encoding[["labels"]], values = rep_len(lines, length(levels))))
}
