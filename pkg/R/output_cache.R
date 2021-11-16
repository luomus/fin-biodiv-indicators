#' @import dbplyr
#' @import rlang
NULL

#' Create an output cache
#'
#' Create a database output cache if it doesn't already exist.
#'
#' @importFrom RPostgres dbExistsTable dbWriteTable Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @export

create_output_cache <- function() {

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

  if (!RPostgres::dbExistsTable(db, "output_cache")) {

    RPostgres::dbWriteTable(db, "output_cache", df)

  }

}

#' Set output cache
#'
#' Cache output data in a database.
#'
#' @param hash Hash of input
#' @param data Raw data.
#'
#' @importFrom blob blob
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RPostgres dbAppendTable Postgres

#' @export

set_output_cache <- function(hash, data) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- data.frame(time = Sys.time(), hash = hash, data = blob::blob(data))

  RPostgres::dbAppendTable(db, "output_cache", df)

}

#' Get data from cache
#'
#' Get cached data from a database.
#'
#' @param hash Hash of input
#'

#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl
#' @importFrom rlang .data
#' @importFrom RPostgres Postgres
#'
#' @export

get_from_output_cache <- function(hash) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  x <- dplyr::tbl(db, "output_cache")
  x <- dplyr::filter(x, .data[["hash"]] == !!hash)
  dplyr::collect(x)

}

#' Check if cached
#'
#' Check if there is any cached data.
#'
#' @param cache Cached data.
#'
#' @export

is_output_cached <- function(cache) {
  nrow(cache) > 0L
}

#' Check cache validity
#'
#' Check if the cache is newer than the last modification time of the index data.
#'
#' @param cache Cached data.
#' @param use_cache Whether to use cached data.
#' @param index Which index.
#'
#' @export

output_cache_valid <- function(cache, use_cache, index) {

  is_cached <- is_output_cached(cache)

  if (missing(index) || !is_cached || use_cache) {

    return(is_cached)

  }

  unlist(cache[, "time"]) > last_modified(get_filter(index))

}
