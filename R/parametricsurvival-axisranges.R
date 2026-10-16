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
    if (position == "y" && probabilityAxis && transformation$name == "logmlogmp") {
      ticks <- .sapPredictionProbabilityTicks(bounds, transformation, intervals)
    } else if (transformation$name == "log-10") {
      # Labels remain in original units on logarithmic axes.
      rawBounds <- sort(transformation$inverse(bounds))
      rawTicks <- if (diff(bounds) < 1) jaspGraphs::getPrettyAxisBreaks(rawBounds, n = intervals) else
        scales::breaks_log(n = intervals)(rawBounds)
      ticks <- validTicks(suppressWarnings(transformation$transform(rawTicks)))
      anchors <- .sapPredictionTickAnchors(rawBounds)
      anchors <- validTicks(suppressWarnings(transformation$transform(anchors[anchors > 0])))
    } else {
      ticks <- validTicks(jaspGraphs::getPrettyAxisBreaks(bounds, n = intervals))
      anchors <- validTicks(.sapPredictionTickAnchors(bounds))
    }
    if (!(position == "y" && probabilityAxis && transformation$name == "logmlogmp")) {
      lower <- anchors[anchors <= bounds[1L] + tolerance]
      upper <- anchors[anchors >= bounds[2L] - tolerance]
      if ((!length(ticks) || min(ticks) > bounds[1L] + tolerance) && length(lower)) ticks <- c(max(lower), ticks)
      if ((!length(ticks) || max(ticks) < bounds[2L] - tolerance) && length(upper)) ticks <- c(ticks, min(upper))
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
    if (position == "y" && probabilityAxis && transformation$name != "identity" && inherits(replacement$labels, "waiver"))
      replacement$labels <- .sapPredictionProbabilityLabels
    # ggplot stores continuous limits in transformed coordinates.
    replacement$limits <- range(c(ticks, bounds))
    replacement$oob <- scales::oob_keep
    plot$scales$scales <- Filter(function(other) !position %in% other$aesthetics, plot$scales$scales)
    plot$scales$add(replacement)
  }
  return(plot)
}

# Round finite-domain endpoints without making the entire tick grid denser.
.sapPredictionTickAnchors <- function(bounds) {
  magnitude <- abs(bounds[bounds != 0])
  if (!length(magnitude)) return(0)
  return(sort(unique(c(0, as.vector(outer(c(-5, -2, -1, 1, 2, 5),
    10^seq(floor(log10(min(magnitude))) - 1, ceiling(log10(max(magnitude))) + 1), `*`))))))
}

# Choose round probabilities, with additional resolution near either boundary.
.sapPredictionProbabilityTicks <- function(bounds, transformation, intervals) {
  rawBounds <- sort(transformation$inverse(bounds))
  probabilities <- sort(unique(c(
    jaspGraphs::getPrettyAxisBreaks(rawBounds, n = intervals),
    seq(.1, .9, .1), as.vector(outer(c(1, 2, 5), 10^seq(-323, -1), `*`)),
    1 - as.vector(outer(c(1, 2, 5), 10^seq(-16, -1), `*`)))))
  probabilities <- unique(signif(probabilities, 15L))
  probabilities <- probabilities[is.finite(probabilities) & probabilities > 0 & probabilities < 1]
  ticks <- transformation$transform(probabilities)
  lower <- which(ticks <= bounds[1L])
  upper <- which(ticks >= bounds[2L])
  ticks <- ticks[seq.int(if (length(lower)) max(lower) else 1L,
    if (length(upper)) min(upper) else length(ticks))]
  targets <- seq(min(ticks), max(ticks), length.out = intervals + 1L)
  return(unique(ticks[vapply(targets, function(target) which.min(abs(ticks - target)), integer(1))]))
}

.sapPredictionProbabilityLabels <- function(probabilities) {
  return(vapply(probabilities, function(probability) {
    label <- format(probability, digits = 15L, trim = TRUE)
    if (probability < 1 && label == "1")
      label <- paste0("1 \u2212 ", format(1 - probability, digits = 2L, trim = TRUE))
    return(label)
  }, character(1)))
}
