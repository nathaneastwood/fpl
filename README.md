
<!-- README.md is generated from README.Rmd. Please edit that file -->

{fpl} <a href='https://nathaneastwood.github.io/fpl/'><img src='man/figures/logo.png' align="right" height="139" /></a>
=======================================================================================================================

[![CRAN
status](https://www.r-pkg.org/badges/version/fpl)](https://cran.r-project.org/package=fpl)
[![Dependencies](https://tinyverse.netlify.com/badge/fpl)](https://cran.r-project.org/package=fpl)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/fpl)
![check\_cran](https://github.com/nathaneastwood/fpl/workflows/check_cran/badge.svg?branch=master)

Overview
--------

{fpl} is an R6 class wrapper around the Barclays Fantasy Premier League
API.

Installation
------------

You can install:

-   the development version from
    [GitHub](https://github.com/nathaneastwood/fpl) with

<!-- -->

    # install.packages("remotes")
    remotes::install_github("nathaneastwood/fpl")

Usage
-----

    library(fpl)

    fpl <- FPL$new()
    team <- fpl$get_team(11)
    team_players <- team$get_players()

Related Work
------------

-   [fpl](https://github.com/amosbastian/fpl) - a Python FPL API
    wrapper.
-   [{fplscrapR}](https://github.com/wiscostret/fplscrapR) - A
    functional FPL API wrapper.
