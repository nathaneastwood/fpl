#' Determine if the chosen gameweek(s) are valid
#'
#' @param gameweek `numeric(n)`. The gameweek(s).
#'
#' @return `TRUE`, invisibly, if the gameweek is valid, otherwise errors.
#'
#' @examples
#' valid_gameweek(1)
#'
#' @noRd
valid_gameweek <- function(gameweek) {
  if (gameweek < fpl_env$min_gameweek || gameweek > fpl_env$max_gameweek) {
    stop("`gameweek` must be a number between ", fpl_env$min_gameweek, " and ", fpl_env$max_gameweek, ".")
  }
  invisible(TRUE)
}

#' Scales value between upper and lower values, depending on the given minimum and maximum value.
#'
#' @param x `numeric(n)`. The values to scale.
#' @param min `numeric(n)`. The minimum possible value of `x`.
#' @param max `numeric(n)`. The maximum possible value of `x`.
#' @param lower `numeric(1)`. The lower value to scale between.
#' @param upper `numeric(1)`. The upper value to scale between.
#'
#' @return
#' A `numeric(n)` vector of scaled values.
#'
#' @noRd
scale <- function(x, min, max, lower = 1, upper = 5) {
  if (length(lower) != 1 || !is.numeric(lower)) stop("`lower` must be a `numeric(1)`")
  if (length(upper) != 1 || !is.numeric(upper)) stop("`upper` must be a `numeric(1)`")
  numerator <- (lower - upper) * (x - min)
  denominator <- max - min
  numerator / denominator + upper
}
