#' R6 Class Representing a Team
#'
#' @description
#' The [FPL$get_team()] and [FPL$get_teams()] methods return objects of class `Team`.
#'
#' @details
#' Information is taken from <https://fantasy.premierleague.com/api/bootstrap-static/>.
Team <- R6::R6Class(
  "Team",
  public = list(
    #' @field team A `data.frame`. Data for a single Premier League team.
    team = data.frame(),
    #' @field players A `data.frame`. Player data for a single Premier League team. Empty by default unless the
    #' [FPL$get_players()] method is called.
    players = data.frame(),
    #' @description
    #' Instantiate the `Team` class.
    #'
    #' @param team A `data.frame`. Data for a single Premier League team.
    #'
    #' @return An R6 object of class `Team`.
    #'
    #' @examples
    #' fpl <- FPL$new()
    #' team <- fpl$get_team(11)
    initialize = function(team) {
      self$team <- team
    },
    #' @description
    #' Get information about specific players from the `team`.
    #'
    #' @examples
    #' team$get_players()
    #'
    #' @return
    #' A `list` of R6 `Player` objects who play for the `Team`.
    get_players = function() {
      if (nrow(self$players) == 0L) {
        players <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements
        self$players <- players[players$team == self$team$id, ]
      }
      get_player_worker(players, self$players$id)
    },
    #' Pretty printing of the `Team`.
    #' @param ... Not used.
    print = function(...) {
      print(self$team$name)
    }
  )
)
