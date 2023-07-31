#' @importFrom pool dbAppendTable dbExecute dbExistsTable dbListFields
#' @importFrom pool dbWriteTable

set_cache <- function(index, table, df, db) {

  df <- as.data.frame(df)

  nms <- names(df)

  rmax <- 1e5L

  idx <- seq_len(nrow(df))

  df <- lapply(split(idx, ceiling(idx / rmax)), slice_rows, df)

  if (!pool::dbExistsTable(db, table)) {

    for (dfi in df) {

      pool::dbWriteTable(db, table, dfi)

    }

  } else {

    pool::dbExecute(
      db, sprintf("DELETE FROM \"%s\" WHERE \"index\" = '%s'", table, index)
    )

    db_cols <- pool::dbListFields(db, table)

    for (i in setdiff(nms, db_cols)) {

      dt <- switch(
        class(df[[1L]][[i]]), integer = "int", numeric = "real", "varchar"
      )

      pool::dbExecute(
        db, sprintf("ALTER TABLE \"%s\" ADD COLUMN \"%s\" %s", table, i, dt)
      )

    }

    for (dfi in df) {

      pool::dbAppendTable(db, table, dfi)

    }

  }

}

slice_rows <- function(idx, df) {

  df[idx, , drop = FALSE]

}
