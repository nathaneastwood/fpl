#' Build FPL URL
#'
#' @param ... Elements of the url.
#'
#' @return
#' `character(n)`. FPL URLs.
build_url <- function(...) {
  paste0(fpl_env$base_url, paste(..., sep = "/"), "/")
}
