#' Clear output and input caches
#'
#' Clear cache database of all its tables.
#'
#' @importFrom RPostgres dbListTables dbRemoveTable Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @export

clear_cache <- function() {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  tables <- RPostgres::dbListTables(db)

  pg_tables <- c("pg_stat_statements", "primarytable")

  for (i in setdiff(tables, pg_tables)) {

    RPostgres::dbRemoveTable(db, i)

  }

}
