#' Determine if the chosen gameweek(s) are valid
#'
#' param gameweek `numeric(n)`. The gameweek(s).
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
