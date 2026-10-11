# Plan axes from predictor ranges, before raster cells add their half-widths.
.sapPredictionContourPlotAxes <- function(fit, options, axes) {
  ranges <- lapply(axes, function(variable)
    range(unlist(lapply(fit, .sapPredictionAxisRange, options = options, variable = variable), use.names = FALSE)))
  mappings <- lapply(axes, function(variable)
    .sapPredictionAxisScale(fit, variable, .sapPredictionAxisSettings(options, variable)[["scale"]], options = options))
  plot <- ggplot2::ggplot(data.frame(x = ranges[[1L]], y = ranges[[2L]]), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_blank()
  positions <- c("x", "y")
  for (i in seq_along(mappings)) if (!is.null(mappings[[i]]))
    plot <- plot + .sapPredictionPositionScale(mappings[[i]], positions[i])
  plot <- .sapPredictionPrettyAxes(plot)
  scales <- lapply(positions, plot$scales$get_scales)
  bounds <- lapply(scales, function(scale) sort(scale$get_transformation()$inverse(scale$limits)))
  settings <- .sapPredictionContourSettings(options)
  for (i in seq_along(bounds)) {
    prefix <- paste0("predictionsCovariateXAxisSettings", toupper(positions[i]), "Axis")
    settings[[paste0(prefix, "From")]] <- sprintf("%.17g", bounds[[i]][1L])
    settings[[paste0(prefix, "To")]] <- sprintf("%.17g", bounds[[i]][2L])
  }
  return(list(mappings = mappings, scales = scales, bounds = bounds, settings = settings))
}
