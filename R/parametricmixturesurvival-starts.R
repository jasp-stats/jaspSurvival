#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Starting memberships for the mixture estimator.
.sapmStarts                     <- function(options, family, survObject, emSurvObject, components, previous) {

  logTime       <- .sapmLogTimes(emSurvObject)
  probabilities <- .sapmStartMembershipProbabilities(options)
  # Keep decimal complements stable across probability and softening inputs.
  softness      <- signif(1 - probabilities, 15)
  starts        <- list()
  add <- function(name, posterior, probabilityIndex) {
    if (probabilityIndex > 1)
      name <- paste0(name, "Probability", probabilityIndex)
    starts[[length(starts) + 1]] <<- list(name = name, posterior = posterior)
  }
  addPartition <- function(name, membership) {
    for (i in seq_along(probabilities))
      add(name, .sapmSoftPosterior(membership, components, softness = softness[i]), i)
  }

  # the random draws are seeded so that the starting values do not depend on the preceding fits
  # (the solution with one component fewer is fitted first when it is not part of the analysis)
  if (options[["mixtureStartKmeans"]]) {
    jaspBase::.setSeedJASP(options)
    addPartition("kmeans", .sapmKmeansMembership(logTime, components))
  }

  if (options[["mixtureStartQuantiles"]])
    addPartition("quantiles", .sapmQuantileMembership(logTime, components))

  if (options[["mixtureStartTails"]]) {
    shares <- .sapmTailShares(components)
    for (i in seq_along(shares))
      addPartition(paste0("tails", i), .sapmTailMembership(logTime, shares[[i]], components))
  }

  if (options[["mixtureStartSplit"]] && !is.null(previous))
    for (j in seq_len(components - 1)) for (i in seq_along(probabilities)) {
      posterior <- try(.sapmSplitPosterior(family, survObject, previous, j, softness[i]), silent = TRUE)
      if (!jaspBase::isTryError(posterior))
        add(paste0("split", j), posterior, i)
    }

  if (options[["mixtureStartRandom"]]) {
    jaspBase::.setSeedJASP(options)
    events <- .sapmEventIndicator(emSurvObject)
    for (i in seq_len(options[["mixtureStartRandomCount"]]))
      addPartition(paste0("random", i), .sapmRandomMembership(logTime, events, components))
  }

  return(starts)
}
.sapmStartMembershipProbabilities <- function(options) {

  probabilities <- .sapCleanCustomOptions(options[["mixtureStartMembershipProbabilities"]],
    gettext("Membership probabilities were specified in an incorrect format. Try '0.95, 0.99'."))

  if (any(probabilities <= 0.5 | probabilities >= 1))
    .quitAnalysis(gettext("Membership probabilities must be greater than 0.5 and less than 1."))
  if (anyDuplicated(probabilities))
    .quitAnalysis(gettext("Membership probabilities must be distinct."))

  return(probabilities)
}
.sapmSoftPosterior              <- function(membership, components, softness = 0.05) {

  # soft start avoids empty components
  nObs      <- length(membership)
  posterior <- matrix(softness / (components - 1), nObs, components)
  posterior[cbind(seq_len(nObs), membership)] <- 1 - softness

  return(posterior)
}
.sapmLogTimes                   <- function(survObject) {
  time <- .sapmObservedTimes(survObject)
  return(log(pmax(time, min(time[time > 0]))))
}
.sapmEventIndicator             <- function(survObject) {

  type <- attr(survObject, "type")
  if (type %in% c("right", "counting"))
    return(survObject[, "status"] == 1)

  # exact, left-censored and interval-censored observations all establish that the event occurred
  return(survObject[, "status"] %in% c(1, 2, 3))
}
.sapmKmeansMembership           <- function(logTime, components) {

  if (length(unique(logTime)) <= components)
    return(.sapmQuantileMembership(logTime, components))

  clusters <- stats::kmeans(logTime, centers = components, nstart = 20)

  return(match(clusters[["cluster"]], order(clusters[["centers"]][, 1])))
}
.sapmQuantileMembership         <- function(logTime, components) {
  return(as.integer(cut(rank(logTime, ties.method = "first"), components, labels = FALSE)))
}
.sapmTailShares                 <- function(components) {
  return(switch(
    as.character(components),
    "2" = list(c(0.15, 0.85), c(0.85, 0.15)),
    "3" = list(c(0.15, 0.70, 0.15)),
    "4" = list(c(0.10, 0.40, 0.40, 0.10)),
    list()
  ))
}
.sapmTailMembership             <- function(logTime, shares, components) {

  nObs                   <- length(logTime)
  breaks                 <- unique(c(0, round(cumsum(shares) * nObs)))
  breaks[length(breaks)] <- nObs
  membership             <- as.integer(cut(rank(logTime, ties.method = "first"), breaks = breaks, labels = FALSE))
  membership[is.na(membership)] <- 1L

  return(pmin(pmax(membership, 1L), components))
}
.sapmRandomMembership           <- function(logTime, events, components) {

  if (length(unique(logTime)) < components)
    return(.sapmQuantileMembership(logTime, components))

  # centres are drawn from the event times whenever there are enough of them
  pool <- unique(logTime[events])
  if (length(pool) < components + 1)
    pool <- unique(logTime)
  centers <- sort(sample(pool, components))

  return(apply(abs(outer(logTime, centers, "-")), 1, which.min))
}
.sapmSplitPosterior             <- function(family, survObject, previous, j, softness = 0.05) {

  # component j of the previous solution is split into a lower and an upper child at its median: events are
  # assigned by their observed time and censored observations by the probability of the censored interval
  parameters <- previous[["parameters"]][[j]]
  nObs       <- nrow(survObject)
  type       <- attr(survObject, "type")
  expand     <- function() lapply(parameters, function(x) if (length(x) > 1) x else rep(x, nObs))
  quantileAt <- function(p) do.call(family[["q"]], c(list(rep(p, nObs)), expand()))
  survivalAt <- function(q) do.call(family[["p"]], c(list(q), expand(), list(lower.tail = FALSE)))
  failureAt  <- function(q) do.call(family[["p"]], c(list(q), expand(), list(lower.tail = TRUE)))

  lower <- ifelse(.sapmObservedTimes(survObject) <= quantileAt(0.5), 1 - softness, softness)

  if (type %in% c("right", "counting")) {
    censored <- survObject[, "status"] != 1
    survival <- survivalAt(survObject[, if (type == "right") "time" else "stop"])
    lower[censored] <- pmin(pmax(pmax(0, (survival - 0.5) / survival), softness), 1 - softness)[censored]
  } else {
    status   <- survObject[, "status"]
    survival <- survivalAt(survObject[, "time1"])
    failure  <- failureAt(survObject[, "time1"])
    lower[status == 0] <- pmin(pmax(pmax(0, (survival - 0.5) / survival), softness), 1 - softness)[status == 0]
    lower[status == 2] <- pmin(pmax(pmin(1, 0.5 / failure), softness), 1 - softness)[status == 2]
  }
  lower[!is.finite(lower)] <- 0.5

  parent    <- previous[["posterior"]]
  posterior <- cbind(
    parent[, seq_len(j - 1), drop = FALSE],
    parent[, j] * lower,
    parent[, j] * (1 - lower),
    parent[, -seq_len(j), drop = FALSE]
  )
  posterior <- pmax(posterior, 1e-12)

  return(posterior / rowSums(posterior))
}
