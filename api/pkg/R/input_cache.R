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
      name = character(),
      available = logical()
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
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl
#' @importFrom rlang .data
#' @importFrom RPostgres Postgres

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

#' Check if input cache is available
#'
#' Check if the cached input data is available.
#'
#' @param hash Hash of input.

input_cache_available <- function(hash) {
  ans <- get_from_input_cache_index(hash)
  isTRUE(ans[["available"]])
}

#' Wait for input cache
#'
#' Wait for cached input data to become available.
#'
#' @param hash Hash of input.
#' @param wait How long to wait between checking availability.
#' @param time_out How many cycles to wait before timing out.
#'
#' @export

wait_for_input_cache_available <- function(hash, wait = 10L, time_out = 60L) {

  stopifnot(time_out > 0L)

  available <- input_cache_available(hash)

  if (!available) {

    Sys.sleep(wait)

    wait_for_input_cache_available(hash, wait, time_out - 1L)

  }

  invisible(NULL)

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
#' @param hash Hash of input.
#' @param available Is the data available?
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RPostgres dbAppendTable Postgres
#'
#' @export

set_input_cache_index <- function(name, hash, available) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  RPostgres::dbAppendTable(
    db, "input_cache_index",
    data.frame(
      time = Sys.time(), hash = hash, name = name, available = available
    )
  )

  invisible(NULL)

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
#' @importFrom RPostgres dbAppendTable dbExecute dbExistsTable dbWriteTable Postgres
#'
#' @export

set_input_cache <- function(name, data, hash, sp = NULL) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- as.data.frame(c(hash = hash, sp = sp, data))

  if (!RPostgres::dbExistsTable(db, name)) {

    RPostgres::dbWriteTable(db, name, df)

  } else {

    RPostgres::dbAppendTable(db, name, df)

  }

  DBI::dbExecute(
    db, sprintf("DELETE FROM input_cache_index WHERE hash = '%s'", hash)
  )

  set_input_cache_index(name, hash, TRUE)

  data

}

#' Get data from cache
#'
#' Get cached data from a database.
#'
#' @param hash Hash of input.
#' @param sp Species.
#' @param type Survey type.
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dplyr collect filter tbl
#' @importFrom rlang .data
#' @importFrom RPostgres Postgres
#'
#' @export

get_from_input_cache <- function(hash, sp, type) {

  ind <- get_from_input_cache_index(hash)

  if (identical(nrow(ind), 1L)) {

    db <- DBI::dbConnect(RPostgres::Postgres())

    on.exit(DBI::dbDisconnect(db))

    name <- ind[["name"]]

    data <- dplyr::tbl(db, name)

    if (!missing(sp) && !missing(type)) {

      data <- dplyr::filter(data, .data[["sp"]] == !!sp)
      data <- dplyr::select(data, -dplyr::any_of(c("sp", "hash")))

    }

    dplyr::collect(data)

  } else {

    invisible(NULL)

  }

}

#' Clean input cache
#'
#' Remove any unavailable caches from index and any unindexed data.
#'
#' @param ... Name components.

input_cache_name <- function(...) {

  paste("cached_input", ..., sep = "_")

}

#' Clean input cache
#'
#' Remove any unavailable caches from index and any unindexed data.
#'

#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom dplyr count filter pull tbl
#' @importFrom rlang .data
#' @importFrom RPostgres  dbListTables dbRemoveTable Postgres
#'
#' @export

clean_input_cache <- function() {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  DBI::dbExecute(
    db, "DELETE FROM input_cache_index WHERE available IS NOT TRUE"
  )

  index <- dplyr::tbl(db, "input_cache_index")

  caches <- RPostgres::dbListTables(db)
  caches <- grep("^cached_input", caches, value = TRUE)

  for (i in caches) {

    rows  <- dplyr::filter(index, .data[["name"]] == !!i)

    nrows <- dplyr::count(rows)
    nrows <- dplyr::pull(nrows, "n")

    if (identical(nrows, 0L)) {

      RPostgres::dbRemoveTable(db, i)

      next

    }

    indexed_hashes <- dplyr::pull(rows, "hash")

    cache <- dplyr::tbl(db, i)

    unindexed_hashes <- dplyr::filter(
      cache, !.data[["hash"]] %in% !!indexed_hashes
    )
    unindexed_hashes <- dplyr::distinct(cache, .data[["hash"]])
    unindexed_hashes <- dplyr::pull(unindexed_hashes, "hash")

    for (j in unindexed_hashes) {

      DBI::dbExecute(db, sprintf("DELETE FROM %s WHERE hash = '%s'", i, j))

    }

  }

}
