#' R6 Class Representing a Player
#'
#' @description
#' The [FPL$get_player()] and [FPL$get_players()] methods return objects of class `Player`.
#'
#' @details
#' Information is taken from <https://fantasy.premierleague.com/api/bootstrap-static/>.
Player <- R6::R6Class(
  "Player",
  public = list(
    #' @field player A `data.frame`. Player data for a single player.
    player = data.frame(),
    #' @field history A `data.frame`. The current season's data for a given player. Empty by default unless particular
    #' active bindings are called.
    history = data.frame(),
    #' @description
    #' Instantiate the `Player` class.
    #'
    #' @param player A `data.frame`. Player data for a single player.
    #'
    #' @return An R6 object of class `Player`.
    #'
    #' @examples
    #' fpl <- FPL$new()
    #' player <- fpl$get_player(250)
    initialize = function(player) {
      self$player <- player
    },
    #' @description
    #' Pretty printing of the the `Player`.
    #' @param ... Not used.
    print = function(...) {
      print(self$player$web_name)
    }
  ),
  active = list(
    #' @field games_played `integer(1)`. The number of games the player has partaken in this season.
    games_played = function() {
      if (length(self$history) == 0L) self$history <- PlayerSummary$new(self$player$id)$history
      sum(self$history$minutes > 0)
    },
    #' @field price `numeric(1)`. The player's in game price.
    price = function() {
      self$player$now_cost / 10
    },
    #' @field pp90 `numeric(1)`. Points scored per 90 minutes.
    pp90 = function() {
      minutes <- self$player$minutes
      if (minutes == 0) return(0)
      self$player$total_points / (minutes / 90)
    },
    #' @field ppm `numeric(1)`. Points scored per match.
    ppm = function() {
      games_played <- self$games_played
      if (games_played == 0) return(0)
      self$player$total_points / games_played
    },
    #' @field ppmm `numeric(1)`. Points scored per match per £1M.
    ppmm = function() {
      self$ppm / self$price
    },
    #' @field vapm `numeric(1)`. Value added per £1M.
    vapm = function() {
      price <- self$price
      if (self$games_played == 0 || price == 0) return(0)
      (self$ppm - 2) / price
    }
  )
)

#' R6 Class Representing a Player's Historic Data
#'
#' @description
#' The [FPL$get_player_summary()] and [FPL$get_player_summaries()] methods return objects of class `PlayerSummary`.
#'
#' @details
#' Information is taken from <https://fantasy.premierleague.com/api/element-summary/{player_id}/>.
PlayerSummary <- R6::R6Class(
  "PlayerSummary",
  public = list(
    #' @field fixtures A `data.frame`. The player's fixture list for the season.
    fixtures = data.frame(),
    #' @field history A `data.frame`. The player's data for the current seasom.
    history = data.frame(),
    #' @field history_past A `data.frame`. The player's summary data for previous seasons.
    history_past = data.frame(),
    #' @description
    #' Instantiate the `PlayerSummary` class.
    #'
    #' @param player_id `numeric(1)`. The player's id.
    #'
    #' @return An R6 object of class `PlayerSummary`.
    #'
    #' @examples
    #' fpl <- FPL$new()
    #' player_summary <- fpl$get_player_summary(250)
    initialize = function(player_id) {
      data <- jsonlite::fromJSON(paste0(fpl_env$base_url, "element-summary/", player_id, "/"))
      for (i in names(data)) {
        # In pre-season, data$history will be an empty `list` so set it as a `data.frame`
        # Also, new players will return a `list` for data$history_past
        self[[i]] <- if (length(data[[i]]) == 0L) data.frame() else data[[i]]
      }
    }
  )
)
