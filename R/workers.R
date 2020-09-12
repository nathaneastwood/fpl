#' Get team information.
#'
#' @param teams A `data.frame`. The teams data.
#' @param team_ids `numeric(n)`. The team ids.
#'
#' @return A `list` of R6 `Team` objects.
#'
#' @noRd
get_team_worker = function(teams, team_ids) {
  lapply(
    team_ids,
    function(i, team_ids) Team$new(team = teams[teams$id %in% i, ]),
    team_ids = team_ids
  )
}

#' Get player information.
#'
#' @param players A `data.frame`. The players data.
#' @param player_id `numeric(n)`. The player ids.
#'
#' @return A `list` of R6 `Player` objects.
#'
#' @noRd
get_player_worker <- function(players, player_id) {
  # TODO: include_summary
  player_na <- which(!player_id %in% players$id)
  if (any(player_na)) {
    stop("The following `player_id`s are not available:\n    ", paste(player_id[player_na], collapse = ", "))
  }
  lapply(player_id, function(i) Player$new(players[players$id == i, ]))
}

#' Get player summary information.
#'
#' @param player_ids `numeric(n)`. The player ids.
#'
#' @return A `list` of R6 `PlayerSummary` objects.
#'
#' @noRd
get_player_summary_worker = function(player_ids) {
  lapply(
    player_ids,
    function(i, player_ids) {
      PlayerSummary$new(player_id = i)
    },
    player_ids = player_ids
  )
}
