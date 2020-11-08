expect_error(
  fpl:::valid_gameweek(100),
  info = "Gameweeks outside of the standard season will error."
)

expect_true(
  fpl:::valid_gameweek(1:38),
  info = "Gameweeks inside a regular season return `TRUE`."
)
