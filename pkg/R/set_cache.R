#' @importFrom pool dbAppendTable dbExecute dbExistsTable dbListFields
#' @importFrom pool dbWriteTable
#' @importFrom dplyr .data filter pull tbl

set_cache <- function(index, table, df, db) {

  df <- as.data.frame(df)

  if (!pool::dbExistsTable(db, table)) {

    pool::dbWriteTable(db, table, df)

  } else {

    pool::dbExecute(
      db, sprintf("DELETE FROM %s WHERE index = '%s'", table, index)
    )

    db_cols <- pool::dbListFields(db, table)

    for (i in setdiff(names(df), db_cols)) {

      dt <- switch(
        class(df[[i]]), integer = "int", float = "float4", "varchar"
      )

      pool::dbExecute(
        db, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, i, dt)
      )

    }

    pool::dbAppendTable(db, table, df)

  }

}
