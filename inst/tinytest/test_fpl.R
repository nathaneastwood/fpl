fpl <- FPL$new()

expect_equal(
  class(fpl),
  c("FPL", "R6"),
  info = "Check FPL instantiated class type."
)

# -- get_team() ----------------------------------------------------------------

team <- fpl$get_team(1)
expect_equal(
  class(team),
  c("Team", "R6"),
  info = "Check class of get_team() output."
)
expect_error(
  fpl$get_team(100),
  info = "`team_id` must be between 1 and 20."
)
expect_error(
  fpl$get_team(c(1, 2)),
  info = "`team_id` must be a `numeric(1)`."
)

# -- get_teams() ---------------------------------------------------------------

teams <- fpl$get_teams(c(1, 2))
expect_equal(
  lapply(teams, class),
  list(c("Team", "R6"), c("Team", "R6")),
  info = "get_teams() returns a list of Team classes."
)
expect_equal(
  length(fpl$get_teams()),
  20L,
  info = "team_ids = NULL returns all teams."
)
expect_error(
  fpl$get_team(c(1, 100)),
  info = "`team_id` must be between 1 and 20."
)
