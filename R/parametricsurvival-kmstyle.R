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
