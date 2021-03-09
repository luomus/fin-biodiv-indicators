#' @import dbplyr
#' @import rlang
NULL

#' Create a cache
#'
#' Create a database cache if it doesn't already exist.
#'
#' @importFrom RPostgres dbExistsTable dbWriteTable Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @export

create_cache <- function() {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- structure(
    list(
      time = as.POSIXct(integer(), origin = "1970-01-01"),
      hash = character(),
      data = list()
    ),
    class = "data.frame"
  )

  if (!RPostgres::dbExistsTable(db, "cache")) {

    RPostgres::dbWriteTable(db, "cache", df)

  }

}

#' Set cache
#'
#' Cache data in a database.
#'
#' @param hash Hash of input
#' @param data Raw data.
#'
#' @importFrom blob blob
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RPostgres dbAppendTable Postgres

#' @export

set_cache <- function(hash, data) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- data.frame(time = Sys.time(), hash = hash, data = blob::blob(data))

  RPostgres::dbAppendTable(db, "cache", df)

}

#' Get data from cache
#'
#' Get cached data from a database.
#'
#' @param hash Hash of input
#'
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl
#' @export

get_from_cache <- function(hash) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  x <- dplyr::tbl(db, "cache")
  x <- dplyr::filter(x, hash == !!hash)
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
