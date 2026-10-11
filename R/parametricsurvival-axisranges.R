# Tick endpoints cover the rendered geometry, including bounds and overlays.
.sapPredictionPrettyAxes <- function(plot, quantileAxis = FALSE, probabilityAxis = FALSE) {
  built <- ggplot2::ggplot_build(plot)
  estimates <- which(vapply(plot$layers, function(layer)
    (inherits(layer$geom, "GeomLine") || inherits(layer$geom, "GeomPoint")) && inherits(layer$data, "waiver"), logical(1)))
  if (length(estimates) && !any(vapply(built$data[estimates], function(layer)
      any(is.finite(layer$x) & is.finite(layer$y)), logical(1))))
    stop(gettext("No finite predictions remain after the selected axis transformations. Choose a different scale or evaluation range."))
  plot$scales <- plot$scales$clone()
  for (position in c("x", "y")) {
    scale <- built$layout[[paste0("panel_scales_", position)]][[1L]]
    if (scale$is_discrete()) next
    columns <- paste0(position, c("", "min", "max", "end", "intercept"))
    values <- unlist(lapply(seq_along(built$data), function(i) {
      # A one-dimensional rug has no meaningful coordinate on the other axis.
      if (inherits(plot$layers[[i]]$geom, "GeomRug") &&
          !position %in% names(plot$layers[[i]]$mapping)) return(NULL)
      layer <- built$data[[i]]
      unlist(layer[intersect(columns, names(layer))], use.names = FALSE)
    }), use.names = FALSE)
    if (is.numeric(scale$limits)) values <- c(values, scale$limits)
    values <- values[is.finite(values)]
    if (!length(values)) next
    transformation <- scale$get_transformation()
    bounds <- range(values)
    intervals <- if (position == "x" && !quantileAxis) 3L else 5L
    tolerance <- 64 * .Machine$double.eps * max(abs(bounds))
    validTicks <- function(ticks) {
      raw <- suppressWarnings(transformation$inverse(ticks))
      mapped <- suppressWarnings(transformation$transform(raw))
      valid <- is.finite(raw) & is.finite(mapped)
      if ((position == "x" && quantileAxis) || (position == "y" && probabilityAxis))
        valid <- valid & raw >= 0 & raw <= 1
      return(mapped[valid])
    }
    if (position == "x" && quantileAxis && transformation$name == "log-10") {
      # Label round percentiles, positioned on the logarithmic scale.
      rawBounds <- transformation$inverse(bounds)
      rawTicks <- if (diff(bounds) < 1) jaspGraphs::getPrettyAxisBreaks(rawBounds, n = intervals) else
        scales::breaks_log(n = intervals)(rawBounds)
      ticks <- validTicks(suppressWarnings(transformation$transform(rawTicks)))
    } else {
      # Ask JASP for a coarser grid; never select uneven indices from that grid.
      for (attempt in seq_len(8L)) {
        ticks <- validTicks(jaspGraphs::getPrettyAxisBreaks(bounds, n = intervals))
        if (length(ticks) && min(ticks) <= bounds[1L] + tolerance && max(ticks) >= bounds[2L] - tolerance) break
        intervals <- intervals * 2L
      }
    }
    # Cover any boundary omitted by the selected break algorithm.
    if (!length(ticks) || min(ticks) > bounds[1L] + tolerance) ticks <- c(bounds[1L], ticks)
    if (!length(ticks) || max(ticks) < bounds[2L] - tolerance) ticks <- c(ticks, bounds[2L])
    ticks <- sort(unique(ticks))
    rawTicks <- suppressWarnings(transformation$inverse(ticks))
    roundTrip <- suppressWarnings(transformation$transform(rawTicks))
    valid <- is.finite(rawTicks) & is.finite(roundTrip)
    ticks <- roundTrip[valid]
    rawTicks <- rawTicks[valid]
    if (!length(ticks)) next
    replacement <- scale$clone()
    replacement$breaks <- rawTicks
    if (transformation$name == "log-10" && inherits(replacement$labels, "waiver"))
      replacement$labels <- function(x) format(signif(x, 2L), trim = TRUE)
    # ggplot stores continuous limits in transformed coordinates.
    replacement$limits <- range(c(ticks, bounds))
    replacement$oob <- scales::oob_keep
    plot$scales$scales <- Filter(function(other) !position %in% other$aesthetics, plot$scales$scales)
    plot$scales$add(replacement)
  }
  return(plot)
}
