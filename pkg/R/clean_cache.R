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
    "output_cache_time",
    "model_output",
    "model_state",
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

  indices <- vapply(config::get("indices"), getElement, "", "code")

  indices <- c(indices, paste(indices, c("north", "south"), sep = "_"))

  sep <- list(sep = "_")

  taxa <- .mapply(
    config::get, list(config = indices), list(value = "taxa")
  )

  taxa <- lapply(taxa, vapply, getElement, "", "code")

  ind <- vapply(taxa, length, 0L) > 1

  taxa_indices <- .mapply(paste, list(indices, taxa), sep)[ind]

  taxa_indices <- do.call(c, taxa_indices)

  extra_taxa <- .mapply(
    config::get, list(config = indices), list(value = "extra_taxa")
  )

  extra_taxa <- lapply(extra_taxa, vapply, getElement, "", "code")

  ind <- vapply(extra_taxa, length, 0L) > 1

  extra_taxa_indices <- .mapply(paste, list(indices, extra_taxa), sep)[ind]

  extra_taxa_indices <- do.call(c, extra_taxa_indices)

  models <- .mapply(
    config::get, list(config = indices), list(value = "model")
  )

  models <- lapply(models, names)

  model_indices <- .mapply(paste, list(indices, models), sep)

  taxa_model_indices <- .mapply(
    outer, list(model_indices, taxa), c(list(FUN = paste), sep)
  )

  taxa_model_indices <- do.call(c, c(model_indices, taxa_model_indices))

  taxa_model_indices <- c(
    taxa_model_indices,
    paste(taxa_model_indices, c("north", "south"), sep = "_")
  )

  extra_taxa_model_indices <- .mapply(
    outer, list(model_indices, extra_taxa), c(list(FUN = paste), sep)
  )

  extra_taxa_model_indices <- do.call(c, c(extra_taxa_model_indices))

  extra_taxa_model_indices <- c(
    extra_taxa_model_indices,
    paste(extra_taxa_model_indices, c("north", "south"), sep = "_")
  )

  clean_indices(index_tables, db_tables, indices, db)

  clean_indices(
    taxon_tables, db_tables, c(taxa_indices, extra_taxa_indices), db
  )

  clean_indices(
    model_tables, db_tables, c(taxa_model_indices, extra_taxa_model_indices), db
  )

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
