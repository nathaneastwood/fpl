#' Get team information.
#'
#' @param teams A `data.frame`. The teams data.
#' @param team_id `numeric(n)`. The team ids.
#'
#' @return A `list` of R6 `Team` objects.
#'
#' @noRd
get_team_worker <- function(teams, team_id) {
  if (any(team_id > 20 || team_id < 1)) stop("`team_id`(s) must be between 1 and 20.")
  lapply(
    team_id,
    function(i, team_id) Team$new(team = teams[teams$id %in% i, ]),
    team_id = team_id
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
get_player_worker <- function(players, player_id, include_summary) {
  if (any(player_id <= 0)) stop("`player_id`(s) must be positive `numeric`(s).")
  player_na <- which(!player_id %in% players$id)
  if (any(player_na)) {
    stop("The following `player_id`s are not available:\n    ", paste(player_id[player_na], collapse = ", "))
  }
  player_id <- player_id[order(player_id)]
  players <- lapply(player_id, function(id) Player$new(players[players$id == id, ]))
  if (isTRUE(include_summary)) {
    summaries <- curl_async(build_url("element-summary", player_id))
    summaries_order <- do.call(c, lapply(summaries, function(x) unique(x$history$element)))
    players <- lapply(
      player_id,
      function(i, summaries, players) list2env(summaries[[match(i, summaries_order)]], players[[i]]),
      summaries = summaries, players = players
    )
  }
  players
}

#' Get player summary information.
#'
#' @param player_ids `numeric(n)`. The player ids.
#'
#' @return A `list` of R6 `PlayerSummary` objects.
#'
#' @noRd
get_player_summary_worker <- function(player_id) {
  summaries <- curl_async(build_url("element-summary", player_id))
  lapply(summaries, PlayerSummary$new)
}
