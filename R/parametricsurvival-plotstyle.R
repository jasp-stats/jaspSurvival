# Reserve canvas room for endpoint tick labels without extending data axes.
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

# Keep empirical colours identical to a standalone Kaplan-Meier plot.
.sapKaplanMeierPalette <- function(palette, colours, empirical, singleColour) {

  empiricalValues <- if (length(empirical) == 1L) singleColour else palette(length(empirical))
  fitted <- setdiff(colours, empirical)
  candidates <- palette(length(colours) + length(empirical))
  candidates <- candidates[!candidates %in% empiricalValues]
  values <- stats::setNames(character(length(colours)), colours)
  values[empirical] <- empiricalValues
  values[fitted] <- rep_len(candidates, length(fitted))
  return(scales::manual_pal(unname(values)))
}

.sapPredictionPlotSize <- function(plot, options, width, height) {
  position <- options[["plotLegend"]]
  if (position == "none" || identical(plot$theme$legend.position, "none")) return(c(width = round(.8 * width), height = height))
  keys <- unlist(lapply(c("colour", "fill", "linetype", "shape"), function(aesthetic) {
    scale <- plot$scales$get_scales(aesthetic)
    if (is.null(scale)) return(character(0))
    limits <- scale$limits
    if (is.character(limits)) return(limits)
    return(character(0))
  }), use.names = FALSE)
  text <- unique(c(keys, unlist(plot$labels[c("colour", "fill", "linetype", "shape")], use.names = FALSE)))
  text <- text[nzchar(text)]
  if (!length(text) && length(plot$layers)) {
    for (layer in plot$layers) if (is.data.frame(layer$data)) for (field in c("Line", "Range"))
      if (field %in% names(layer$data)) text <- unique(c(text, as.character(layer$data[[field]])))
  }
  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  longest <- if (length(lines)) max(nchar(lines)) else 12L
  if (position %in% c("left", "right")) width <- width + min(260, max(140, 60 + 7 * longest))
  else height <- height + min(180, max(60, 30 + 18 * ceiling(max(1L, length(lines)) / 3L)))
  return(c(width = round(.8 * width), height = height))
}

.sapPredictionContourColourbarWidth <- function(plot, options) {
  if (!options[["plotLegend"]] %in% c("top", "bottom")) return(NULL)
  scale <- plot$scales$get_scales("fill")
  if (is.null(scale)) return(NULL)
  theme <- ggplot2::theme_get() + plot$theme
  text <- ggplot2::calc_element("legend.text", theme)
  font <- if (inherits(text, "element_blank")) 0 else text$size
  labels <- scale$get_labels()
  if (!length(labels)) return(NULL)
  labelWidth <- max(nchar(labels)) * font * .6
  return(max(160, (length(labels) - 1L) * (labelWidth + 8)))
}

.sapPredictionContourPlotSize <- function(plot, options, panelCount) {
  columns <- min(2L, panelCount)
  rows    <- ceiling(panelCount / columns)
  # Base canvas fits square panels, axis labels and one caption per row.
  width  <- 260 * columns + 100
  height <- 300 * rows + 50
  if (options[["plotLegend"]] != "none") {
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off())
    grob <- ggplot2::ggplotGrob(plot)
    indices <- which(grepl("^guide-box", grob$layout$name) &
      !vapply(grob$grobs, function(value) inherits(value, "zeroGrob"), logical(1)))
    if (length(indices)) {
      # Measure the rendered guide, including its title and every legend key.
      dimensions <- vapply(grob$grobs[indices], function(guide) c(
        width = grid::convertWidth(sum(guide$widths), "inches", valueOnly = TRUE) * 96,
        height = grid::convertHeight(sum(guide$heights), "inches", valueOnly = TRUE) * 96), numeric(2))
      legendWidth  <- max(dimensions["width", ]) + 32
      legendHeight <- max(dimensions["height", ]) + 32
      if (options[["plotLegend"]] %in% c("left", "right")) {
        width  <- width + legendWidth
        height <- max(height, legendHeight)
      } else {
        fill <- plot$scales$get_scales("fill")
        text <- ggplot2::calc_element("legend.text", ggplot2::theme_get() + plot$theme)
        if (!is.null(fill) && !inherits(text, "element_blank")) {
          # Colourbar endpoint labels extend beyond the guide's own width.
          labelWidths <- vapply(fill$get_labels(), function(label) {
            grob <- grid::textGrob(label, gp = grid::gpar(fontsize = text$size,
              fontfamily = text$family, fontface = text$face))
            grid::convertWidth(grid::grobWidth(grob), "inches", valueOnly = TRUE) * 96
          }, numeric(1))
          if (length(labelWidths)) legendWidth <- legendWidth + max(labelWidths)
        }
        width  <- max(width, legendWidth)
        height <- height + legendHeight
      }
    }
  }
  return(c(width = ceiling(width), height = ceiling(height)))
}
