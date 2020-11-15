#' Get Average Points Against
#'
#' @param points_against A `data.frame` of points scored against all teams in the Premier League, typically the output
#' of [FPL$get_points_against()].
#'
#' @return
#' A `data.frame` containing the `opponent`; fixture `location`; player `position`; and the `points` scored.
get_average_points_against <- function(points_against) {
  with(points_against, aggregate(points ~ opponent + location + position, FUN = mean))
}

#' Get Extrema Points Against
#'
#' @param points_against A `data.frame` of points scored against all teams in the Premier League, typically the output
#' of [FPL$get_points_against()].
#'
#' @return
#' A `data.frame` containing fixture `location`; player `position`; and the `min`imum and `max`imum points against.
get_extrema_points_against <- function(points_against) {
  extrema <- with(points_against, aggregate(points ~ location + position, FUN = range))
  extrema <- do.call(data.frame, extrema)
  colnames(extrema) <- c("location", "position", "min", "max")
  extrema
}

#' Calculate FDR
#'
#' @param average_points A `data.frame`. The output of [get_average_points_against()].
#' @param extrema A `data.frame`. The output of [get_extrema_points_against()].
#'
#' @return
#' A `data.frame` containing the opponent; the location of the scoring team; the player position; the fixture difficulty
#' for the scoring player against the opponent.
calculate_fdr <- function(average_points, extrema) {
  res <- merge(average_points, extrema)
  res["fdr"] <- scale(res$points, res$min, res$max)
  res[, c("opponent", "location", "position", "fdr")]
}
