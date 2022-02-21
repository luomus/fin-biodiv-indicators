#' Clean cache
#'
#' Remove unneeded tables and rows from database cache.
#'
#' @param db Connection. Database cache.
#'
#' @importFrom config get
#' @importFrom dplyr distinct pull tbl
#' @importFrom pool dbExecute dbListTables dbRemoveTable
#' @export

clean_cache <- function(db) {

  pg_tables <-c(
    "pg_stat_statements",
    "primarytable"
  )

  index_tables <- c(
    "surveys",
    "surveys_cached_date"
  )

  taxon_tables  <- c(
    "counts",
    "counts_cached_date"
  )

  model_tables <- c(
    "model_output",
    "svg",
    "data",
    "data_csv",
    "count_summary",
    "trends"
  )

  tables <- c(pg_tables, index_tables, taxon_tables, model_tables)

  db_tables <- pool::dbListTables(db)

  for (table in setdiff(db_tables, tables)) {

    pool::dbRemoveTable(db, table)

  }

  indices <- config::get("indices")

  taxa <- .mapply(
    config::get, list(config = indices), list(value = "taxa")
  )

  taxa <- lapply(taxa, vapply, getElement, "", "code")

  sep <- list(sep = "_")

  taxa_indices <- .mapply(paste, list(indices, taxa), sep)

  taxa_indices <- do.call(c, taxa_indices)

  models <- .mapply(
    config::get, list(config = indices), list(value = "model")
  )

  models <- lapply(models, names)

  model_indices <- .mapply(paste, list(indices, models), sep)

  taxa_model_indices <- .mapply(
    outer, list(model_indices, taxa), c(list(FUN = paste), sep)
  )

  taxa_model_indices <- do.call(c, c(model_indices, taxa_model_indices))

  clean_indices(index_tables, db_tables,indices, db)

  clean_indices(taxon_tables, db_tables, taxa_indices, db)

  clean_indices(model_tables, db_tables, taxa_model_indices, db)

}

clean_indices <- function(tables, db_tables, indices, db) {

  for (table in intersect(tables, db_tables)) {

    db_indices <- dplyr::tbl(db, table)

    db_indices <- dplyr::distinct(db_indices, .data[["index"]])

    db_indices <- dplyr::pull(db_indices, .data[["index"]])

    indices_to_drop <- setdiff(db_indices, indices)

    for (index in indices_to_drop) {

      pool::dbExecute(
        db, sprintf("DELETE FROM \"%s\" WHERE \"index\" = '%s'", table, index)
      )

    }

  }

  invisible(NULL)

}
