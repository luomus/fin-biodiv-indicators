#' @importFrom pool dbExistsTable
#' @importFrom dplyr all_of .data filter pull tbl

last_cached <- function(index, table, db) {

  ans <- as.Date(-Inf, "1970-1-1")

  if (pool::dbExistsTable(db, table)) {

    last_cache_on <- dplyr::tbl(db, table)

    last_cache_on <- dplyr::filter(last_cache_on, .data[["index"]] == !!index)

    last_cache_on <- dplyr::pull(last_cache_on, dplyr::all_of("date"))

    if (length(last_cache_on) > 0L) ans <- last_cache_on

  }

  ans

}
