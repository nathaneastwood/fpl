#' Position converter
#'
#' Converts a player's `element_type` to their actual position.
#'
#' @param position `numeric(1)`. The player's position.
#'
#' @return
#' A `character` string of the player's actual position.
#'
#' @export
position_converter <- function(position) {
  position_map <- c("Goalkeeper", "Defender", "Midfielder", "Forward")
  position_map[position]
}

#' Team Converter
#'
#' Convert a team's ID to their actual name.
#'
#' @param teams A `data.frame` containing the teams data. See details for more.
#' @param team_id `numeric(1)`. The team's id.
#'
#' @details
#' `teams` is typically the output of the teams element of `https://fantasy.premierleague.com/api/bootstrap-static/`.
#'
#' @return
#' A `character(1)` containing the team's actual name.
#'
#' @export
team_converter <- function(teams, team_id) {
  if (missing(teams)) teams <- curl_async(build_url("bootstrap-static"))$teams
  teams[teams$id %in% team_id, "name"]
}
