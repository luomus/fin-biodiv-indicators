#' Create an input cache index
#'
#' Create a database input cache index if it doesn't already exist.
#'
#' @importFrom RPostgres dbExistsTable dbWriteTable Postgres
#' @importFrom DBI dbConnect dbDisconnect
#'
#' @export

create_input_cache_index <- function() {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- structure(
    list(
      time = as.POSIXct(integer(), origin = "1970-01-01"),
      hash = character(),
      name = character()
    ),
    class = "data.frame"
  )

  if (!RPostgres::dbExistsTable(db, "input_cache_index")) {

    RPostgres::dbWriteTable(db, "input_cache_index", df)

  }

}

#' Get data from cache index
#'
#' Get cache index data from a database.
#'
#' @param hash Hash of input.
#'
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl

get_from_input_cache_index <- function(hash) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  input_cache_index <- dplyr::tbl(db, "input_cache_index")

  ans <- dplyr::filter(input_cache_index, .data[["hash"]] == !!hash)

  dplyr::collect(ans)

}

#' Check if input is cached
#'
#' Check if there is any cached input data.
#'
#' @param hash Hash of input.

is_input_cached <- function(hash) {
  ans <- get_from_input_cache_index(hash)
  identical(nrow(ans), 1L)
}

#' Check input cache validity
#'
#' Check if the input cache is newer than the last modification time.
#'
#' @param hash  Hash of input.
#' @param last_mod_time Last time data was modified.

input_cache_valid <- function(hash, last_mod_time = as.Date("1970-01-01")) {

  ind <- get_from_input_cache_index(hash)

  if (identical(nrow(ind), 1L)) {

    ind[["time"]] > last_mod_time

  } else {

    FALSE

  }

}

#' Set input cache
#'
#' Cache input data in a database.
#'
#' @param name Name of input.
#' @param data Data.
#' @param hash Hash of input.
#' @param sp  Species.
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RPostgres dbAppendTable dbExistsTable dbWriteTable Postgres
#'
#' @export

set_input_cache <- function(name, data, hash, sp = NULL) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  RPostgres::dbAppendTable(
    db, "input_cache_index",
    data.frame(time = Sys.time(), hash = hash, name = name)
  )

  df <- as.data.frame(c(sp = sp, data))

  if (!RPostgres::dbExistsTable(db, name)) {

    RPostgres::dbWriteTable(db, name, df)

  } else {

    RPostgres::dbAppendTable(db, name, df)

  }

  data

}

#' Get data from cache
#'
#' Get cached data from a database.
#'
#' @param hash Hash of input
#' @param sp Species.
#' @param type Survey type.
#'
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl

get_from_input_cache <- function(hash, sp, type) {

  ind <- get_from_input_cache_index(hash)

  if (identical(nrow(ind), 1L)) {

    db <- DBI::dbConnect(RPostgres::Postgres())

    on.exit(DBI::dbDisconnect(db))

    name <- ind[["name"]]

    data <- dplyr::tbl(db, name)

    if (!missing(sp) && !missing(type)) {

      data <- dplyr::filter(data, sp == !!species[[type]][[sp]])
      data <- dplyr::select(data, -dplyr::any_of('sp'))

    }

    dplyr::collect(data)

  } else {

    invisible(NULL)

  }

}