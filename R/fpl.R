#' R6 Class representing the Fantasy Premier League's API
#'
#' @description
#' The FPL class is the main class used for interacting with Fantasy Premier League’s API.
#'
#' @details
#' Information is taken from <https://fantasy.premierleague.com/api/bootstrap-static/>.
#'
#' @export
FPL <- R6::R6Class(
  "FPL",
  public = list(
    #' @field events A `data.frame`. Game week event data.
    events = data.frame(),
    #' @field game_settings A `list`. FPL game settings.
    game_settings = list(),
    #' @field phases A `data.frame`. Monthly gameweek information.
    phases = data.frame(),
    #' @field teams A `data.frame`. Information pertaining to a Premier League team.
    teams = data.frame(),
    #' @field total_players `integer(1)`. The total number of participants in FPL.
    total_players = integer(),
    #' @field elements A `data.frame`. The players data.
    elements = data.frame(),
    #' @field element_stats A `data.frame`. Labels for certain stats based columns.
    element_stats = data.frame(),
    #' @field element_types A `data.frame`. Squad position information.
    element_types = data.frame(),
    #' @description
    #' Instantiate the `FPL` class.
    #'
    #' @return An R6 `FPL` object.
    #'
    #' @examples
    #' fpl <- FPL$new()
    initialize = function() {
      static <- jsonlite::fromJSON(paste0(fpl_env$base_url, "bootstrap-static/"))
      for (i in names(static)) {
        self[[i]] <- static[[i]]
      }
    },
    #' @description
    #' Get information about a specific user.
    #'
    #' @param user_id `numeric(1)`. The user's id.
    #'
    #' @examples
    #' fpl$get_user(555690)
    #'
    #' @return
    #' An R6 [User()] object.
    get_user = function(user_id) {
      if (user_id <= 0) stop("`user_id` should be a postitive `numeric(1)`.")
      user <- jsonlite::fromJSON(paste0(fpl_env$base_url, "entry/", user_id, "/"))
      User$new(user)
    },
    #' @description
    #' Get information about a specific team.
    #'
    #' @param team_id `numeric(1)`. The team's id.
    #'
    #' @examples
    #' fpl$get_team(1)
    #'
    #' @return
    #' An R6 `Team` object.
    get_team = function(team_id) {
      if (length(team_id) > 1L) stop("`team_id` must be a `numeric(1)`.")
      if (team_id > 20 || team_id < 1) stop("`team_id` must be between 1 and 20.")
      get_team_worker(self$teams, team_id)[[1L]]
    },
    #' @description
    #' Get information about specific teams.
    #'
    #' @param team_ids `numeric(n)`. The team ids.
    #'
    #' @examples
    #' fpl$get_teams(1)
    #'
    #' @return
    #' A `list` of R6 [Team()] objects.
    get_teams = function(team_ids = NULL) {
      if (is.null(team_ids)) team_ids <- self$teams$id
      if (any(team_ids > 20 || team_ids < 1)) stop("`team_ids` must be between 1 and 20.")
      get_team_worker(self$teams, team_ids)
    },
    #' @description
    #' Get information about a specific player.
    #'
    #' @param player_id `numeric(1)`. The player's id.
    #'
    #' @examples
    #' fpl$get_player(1)
    #'
    #' @return
    #' An R6 [Player()] object.
    get_player = function(player_id) {
      if (player_id <= 0 || length(player_id) > 1L) stop("`player_id` must be a `numeric(1)`.")
      player_na <- which(!player_id %in% self$elements$id)
      if (any(player_na)) {
        stop("The following `player_id`s are not available:\n    ", paste(player_id[player_na], collapse = ", "))
      }
      get_player_worker(self$elements, player_id)[[1L]]
    },
    #' @description
    #' Get information about specific players.
    #'
    #' @param player_ids `numeric(n)`. The player ids.
    #'
    #' @examples
    #' fpl$get_players(c(1, 10, 100))
    #'
    #' @return
    #' A `list` of R6 [Player()] objects.
    get_players = function(player_ids) {
      if (any(player_ids <= 0)) stop("`player_ids` must be a `numeric(n)`.")
      get_player_worker(self$elements, player_ids)
    },
    #' @description
    #' Get information about a specific player.
    #'
    #' @param player_id `numeric(1)`. The player's id.
    #'
    #' @examples
    #' fpl$get_player_summary(1)
    #'
    #' @return
    #' An R6 [PlayerSummary()] object.
    get_player_summary = function(player_id) {
      if (player_id <= 0 || length(player_id) > 1L) stop("`player_id` must be a `numeric(1)`.")
      get_player_summary_worker(player_id)[[1L]]
    },
    #' @description
    #' Get information about specific players.
    #'
    #' @param player_ids `numeric(n)`. The player ids.
    #'
    #' @examples
    #' fpl$get_player_summaries(c(1, 10, 100))
    #'
    #' @return
    #' A `list` of R6 [PlayerSummary()] objects.
    get_player_summaries = function(player_ids) {
      if (any(player_ids <= 0)) stop("`player_ids` must be a `numeric(n)`.")
      get_player_summary_worker(player_ids)
    }
  ),
  active = list(
    #' @field current_gameweek `integer(1)`. The current gameweek.
    current_gameweek = function() {
      current_gameweek <- self$events[which(self$events$is_current), "id"]
      if (length(current_gameweek) != 1L) current_gameweek <- 0L
      current_gameweek
    }
  )
)
