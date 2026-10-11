# Reference grids are averaged on each native parameter's linear predictor.
.sapPredictionCustomValues <- function(options) {
  variables <- .sapOptionsVariables(options)
  values <- stats::setNames(rep("", length(variables)), variables)
  unassigned <- setdiff(variables, .sapPredictionTableVariables(options))
  if (options[["predictionsUnassignedReferenceValues"]] != "custom" || !length(unassigned)) return(values)
  data <- .sapCustomPredictionManual(options[["predictionsUnassignedReferenceValuesCustomValues"]], unassigned)
  if (nrow(data) != 1L) stop(gettext("Custom reference values require exactly one row."))
  values[unassigned] <- vapply(data, function(value) trimws(value[1L]), character(1))
  return(values)
}
.sapPredictionReferenceValue <- function(value, variable, options, weights, referenceLevel) {
  mode <- options[["predictionsUnassignedReferenceValues"]]
  text <- .sapPredictionCustomValues(options)[[variable]]
  if (is.factor(value)) {
    level <- if (mode == "custom" && nzchar(text)) text else referenceLevel
    if (!level %in% levels(value)) stop(gettextf("Reference value for %1$s must be an existing factor level.", jaspBase::decodeColNames(variable)))
    return(factor(level, levels = levels(value), ordered = is.ordered(value)))
  }
  number <- if (mode == "reference") 0 else if (mode == "custom" && nzchar(text)) .sapNumericExpression(text) else .sapWeightedMean(value, weights)
  if (!.sapFiniteNumeric(number, expectedLength = 1L)) stop(gettextf("Reference value for %1$s must be one finite number or expression.", jaspBase::decodeColNames(variable)))
  return(as.numeric(number))
}
.sapPredictionMarginalFactors <- function(fit, options, used) {
  if (options[["predictionsUnassignedReferenceValues"]] == "reference") return(character(0))
  training <- attr(fit, "dataset")
  variables <- setdiff(.sapModelVariables(attr(fit, "modelTerms")), used)
  factors <- variables[vapply(training[variables], is.factor, logical(1))]
  if (options[["predictionsUnassignedReferenceValues"]] == "custom") {
    values <- .sapPredictionCustomValues(options)
    factors <- factors[!nzchar(values[factors])]
  }
  return(factors)
}
.sapPredictionPointCoefficients <- function(fit) {
  return(matrix(fit[["res.t"]][, "est"], nrow = 1L, dimnames = list(NULL, rownames(fit[["res.t"]]))))
}
.sapPredictionConditionDesign <- function(fit, data, options, used) {
  if (fit[["ncovs"]] == 0L) return(matrix(numeric(0), nrow = nrow(data), ncol = 0L))
  factors <- .sapPredictionMarginalFactors(fit, options, used)
  training <- attr(fit, "dataset")
  grid <- if (length(factors)) expand.grid(lapply(training[factors], levels), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) else data.frame(.grid = 1L)
  rows <- rep(seq_len(nrow(data)), each = nrow(grid))
  expanded <- data[rows, , drop = FALSE]
  for (variable in factors) expanded[[variable]] <- factor(rep(grid[[variable]], nrow(data)), levels = levels(training[[variable]]), ordered = is.ordered(training[[variable]]))
  for (transformations in .sapModelTransformations(attr(fit, "modelTerms"))) for (variable in names(transformations)) {
    if (any(.sapTransformationInvalid(as.numeric(as.character(expanded[[variable]])), transformations[[variable]])))
      stop(gettextf("The reference value of %1$s is outside its transformation domain. Choose Average or valid Custom values.", jaspBase::decodeColNames(variable)))
  }
  native <- flexsurv::normboot.flexsurvreg(fit, B = 1L, newdata = expanded,
    rawsim = .sapPredictionPointCoefficients(fit), transform = FALSE)
  design <- attr(native, "X")
  averaged <- rowsum(design, rows, reorder = FALSE) / nrow(grid)
  rownames(averaged) <- NULL
  return(averaged)
}
.sapPredictionConditionedFit <- function(fit, data, options, used) {
  attr(fit, "predictionX") <- .sapPredictionConditionDesign(fit, data, options, used)
  attr(fit, "predictionLabels") <- NULL
  return(fit)
}
.sapPredictionUsedVariables <- function(options, xVariable = options[["predictionsXAxisVariables"]]) {
  if (options[["predictionsOutputPlotType"]] == "contour") return(.sapPredictionTableVariables(options))
  return(setdiff(unique(c(xVariable, options[["predictionsSeparateLines"]], options[["predictionsSeparatePlots"]])), options[["predictionsLifetimeTimeId"]]))
}
.sapPredictionPointSummary <- function(fit, data, type, at, transform = identity, fn = NULL) {
  parameters <- flexsurv::normboot.flexsurvreg(fit, B = 1L, X = attr(fit, "predictionX"),
    rawsim = .sapPredictionPointCoefficients(fit), transform = FALSE)
  parameters <- if (is.matrix(parameters)) parameters[rep(1L, nrow(data)), , drop = FALSE] else do.call(rbind, parameters)
  nt <- length(at)
  expanded <- parameters[rep(seq_len(nrow(parameters)), each = nt), fit[["dlist"]][["pars"]], drop = FALSE]
  if (is.null(fn)) fn <- .sapNativePredictionFunction(fit, type)
  estimates <- transform(do.call(fn, c(list(t = rep(at, times = nrow(data)), start = 0), as.list(as.data.frame(expanded)), fit[["aux"]])))
  result <- lapply(seq_len(nrow(data)), function(i) {
    index <- (i - 1L) * nt + seq_len(nt)
    data.frame(time = at, est = estimates[index], lcl = NA_real_, ucl = NA_real_)
  })
  attr(result, "X") <- attr(fit, "predictionX")
  return(result)
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
