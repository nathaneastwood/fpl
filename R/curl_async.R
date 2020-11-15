#' Download Data
#'
#' Fetch multiple data sources at once and parse them once they are downloaded.
#'
#' @param urls `character(n)`. A vector of URLs.
#' @param .parse `function`. The function used to parse the JSON data.
#'
#' @return
#' The parsed JSON.
#'
#' @examples
#' curl_async("https://fantasy.premierleague.com/api/element-summary/1/")
#'
#' @importFrom curl curl_fetch_multi multi_run new_pool
#' @importFrom RcppSimdJson fparse
curl_async <- function(urls, .parse = RcppSimdJson::fparse) {
  out <- list()
  pool <- curl::new_pool()
  for (i in seq_along(urls)) {
    curl::curl_fetch_multi(
      urls[[i]],
      done = function(res) out <<- c(out, list(res$content)),
      fail = function(msg) warning(msg),
      pool = pool
    )
  }
  curl::multi_run(pool = pool)
  .parse(out)
}
