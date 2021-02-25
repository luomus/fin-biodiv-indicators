#' Set cache
#'
#' Cache data in a database.
#'
#' @param table Name of table.
#' @param ... Column names and data to set in the cache.
#'
#' @importFrom RPostgres dbAppendTable Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @export

set_cache <- function(table, ...) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- data.frame(time = Sys.time(), ...)

  RPostgres::dbAppendTable(db, table, df)

}

#' @import dbplyr
NULL

#' Get data from cache
#'
#' Get cached data from a database.
#'
#' @param table Name of table.
#' @param ... Column names and data to check in the cache
#'
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl
#' @export

get_from_cache <- function(table, ...) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  x <- dplyr::tbl(db, table)
  x <- dplyr::filter(x, ...)
  dplyr::collect(x)

}

#' Check if cached
#'
#' Check if there is any cached data.
#'
#' @param cache Cached data.
#'
#' @export

is_cached <- function(cache) {
  nrow(cache) > 0L
}

#' Check cache validity
#'
#' Check if the cache is newer than the last modification time.
#'
#' @param cache Cached data.
#'
#' @export

cache_valid <- function(cache, last_mod_time) {
  cache[, "time"] > last_mod_time
}
