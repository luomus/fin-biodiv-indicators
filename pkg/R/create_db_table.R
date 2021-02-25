#' Create a DB table
#'
#' Create a database table if it doesn't already exist.
#'
#' @param table Name of table.
#' @param ... Columns of table.
#'
#' @importFrom RPostgres dbExistsTable dbWriteTable Postgres
#' @importFrom DBI dbConnect dbDisconnect
#' @export

create_db_table <- function(table, ...) {

  db <- DBI::dbConnect(RPostgres::Postgres())

  on.exit(DBI::dbDisconnect(db))

  df <- list(time = as.POSIXct(integer(), origin = "1970-01-01"), ...)

  df <- structure(df, class = "data.frame")

  if (!RPostgres::dbExistsTable(db, table)) {
    RPostgres::dbWriteTable(db, table, df)
  }

}
