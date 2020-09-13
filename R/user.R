#' R6 Class representing a User
#'
#' @description
#' The [FPL$get_user()] method returns an object of class `User`.
User <- R6::R6Class(
  "User",
  public = list(
    #' @field user A `list` of user details.
    user = list(),
    #' @field history A `list` of `data.frame`s containing the historic gameweek performance data for the user.
    history = list(),
    #' @field cup A `list` of data related to the user's cup performance.
    cup = list(),
    #' @field picks A `list` of the user's picks information for each gameweek.
    picks = list(),
    #' @field transfers A `list` of the user's weekly transfers.
    transfers = list(),
    #' @description
    #' Instantiate the `User` class.
    #'
    #' @param user `list`. Data for a single FPL user.
    #'
    #' @return An R6 `User` object.
    #'
    #' @examples
    #' fpl <- FPL$new()
    #' user <- fpl$get_user(555690)
    initialize = function(user) {
      self$user <- user
    },
    #' @description
    #' Get the gameweek history for the user.
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return A `data.frame`.
    #'
    #' @examples
    #' user$get_gameweek_history()
    get_gameweek_history = function(gameweek = NULL) {
      private$populate_user_history()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(self$history$current[self$history$current$event %in% gameweek, ])
      }
      self$history$current
    },
    #' @description
    #' Get the seasonal history for the user.
    #'
    #' @return A `data.frame`.
    #'
    #' @examples
    #' user$get_season_history()
    get_season_history = function() {
      private$populate_user_history()
      self$history$past
    },
    #' @description
    #' Get the chip history for the user.
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return A `data.frame`.
    #'
    #' @examples
    #' user$get_gameweek_history()
    get_chips_history = function(gameweek = NULL) {
      private$populate_user_history()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(self$history$chips[self$history$chips$event %in% gameweek, ])
      }
      self$history$chips
    },
    #' @description
    #' Get the user's picks for the chosen `gameweek`(s).
    #'
    #' @param gameweek `numeric(n)`. The gameweek picks to return (default: `NULL` returns all gameweeks).
    #'
    #' @return
    #' A `list` containing the user's picks details for the chosen gameweek(s).
    #'
    #' @examples
    #' user$get_picks()
    get_picks = function(gameweek = NULL) {
      private$populate_user_picks()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(lapply(
          self$picks,
          function(x, gameweek) if (x$entry_history$event %in% gameweek) x,
          gameweek = gameweek
        ))
      }
      self$picks
    },
    #' @description
    #' Get the user's cup status.
    #'
    #' @return
    #' A `list` of cup status information.
    #'
    #' @examples
    #' user$get_cup_status()
    get_cup_status = function() {
      private$populate_user_cup()
      self$cup$cup_status
    },
    #' @description
    #' Get the user's cup matches.
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return
    #' A `list` of cup match information.
    #'
    #' @examples
    #' user$get_cup_matches()
    get_cup_matches = function(gameweek = NULL) {
      private$populate_user_cup()
      if (!is.null(gameweek) && length(self$cup) > 0L) {
        valid_gameweek(gameweek)
        return(self$cup$cup_matches[self$cup$cup_matches$event %in% gameweek, ])
      }
      self$cup$cup_matches
    },
    #' @description
    #' Get the user's active chip for each gameweek or the active chip of the chosen `gameweek`(s).
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return
    #' A vector of active chips.
    #'
    #' @examples
    #' user$get_active_chips()
    get_active_chips = function(gameweek = NULL) {
      private$populate_user_picks()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(lapply(
          self$picks,
          function(x, gameweek) if (x$entry_history$event %in% gameweek) x$active_chip,
          gameweek = gameweek
        ))
      }
      lapply(self$picks, function(x) x$active_chip)
    },
    #' @description
    #' Get a `list` of automatic substitutions for the chosen `gameweek`(s).
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return
    #' A vector of active chips.
    #'
    #' @examples
    #' user$get_automatic_substitutions()
    get_automatic_substitutions = function(gameweek = NULL) {
      private$populate_user_picks()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(lapply(
          self$picks,
          function(x, gameweek) if (x$entry_history$event %in% gameweek) x$automatic_subs,
          gameweek = gameweek
        ))
      }
      lapply(self$picks, function(x) x$automatic_subs)
    },
    #' @description
    #' Get a `list` of the user's `gameweek` history.
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return
    #' A `list` of `data.frame`s of the user's gameweek history.
    #'
    #' @examples
    #' user$get_user_history()
    get_user_history = function(gameweek = NULL) {
      private$populate_user_picks()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(lapply(
          self$picks,
          function(x, gameweek) if (x$entry_history$event %in% gameweek) x$entry_history,
          gameweek = gameweek
        ))
      }
      lapply(self$picks, function(x) x$entry_history)
    },
    #' @description
    #' Get a `list` of all the user's transfers, or a `list` of transfers made in the given `gameweek`(s).
    #'
    #' @param gameweek `numeric(n)`. The gameweek history to return (default: `NULL` returns all gameweeks).
    #'
    #' @return
    #' A `list` of the user's transfers.
    #'
    #' @examples
    #' user$get_transfers()
    get_transfers = function(gameweek = NULL) {
      private$populate_user_transfers()
      if (!is.null(gameweek)) {
        valid_gameweek(gameweek)
        return(self$transfers[self$transfers$event %in% gameweek, ])
      }
      self$transfers
    },
    #' @description
    #' Pretty printing of the `User`.
    #' @param ... Not used.
    print = function(...) {
      print(paste0(self$user$player_first_name, " ", self$user$player_last_name, " - ", self$user$name))
    }
  ),
  private = list(
    populate_user_history = function() {
      if (length(self$history) == 0L) {
        self$history <- jsonlite::fromJSON(
          paste0(fpl_env$base_url, "entry/", self$user$id, "/history/")
        )
      }
    },
    populate_user_cup = function() {
      if (length(self$cup) == 0L) {
        self$cup <- jsonlite::fromJSON(paste0(fpl_env$base_url, "entry/", self$user$id, "/cup/"))
      }
    },
    populate_user_picks = function() {
      self$picks <- lapply(
        seq(self$user$started_event, self$user$current_event),
        function(i) {
          data <- jsonlite::fromJSON(
            paste0(fpl_env$base_url, "entry/", self$user$id, "/event/", i, "/picks/")
          )
          data$entry_history <- as.data.frame(data$entry_history)
          data
        }
      )
    },
    populate_user_transfers = function() {
      if (length(self$transfers) == 0L) {
        self$transfers <- jsonlite::fromJSON(
          paste0(fpl_env$base_url, "entry/", self$user$id, "/transfers/")
        )
      }
    }
  )
)

