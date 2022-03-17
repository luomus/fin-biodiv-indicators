needs_update <- function(index, filter, table, db) {

  ans <- Sys.getenv("CHECK_UPDATE", TRUE)

  ans <- as.logical(ans)

  if (isFALSE(ans)) {

    ans

  } else {

    last_mod_date <- last_mod(filter)

    last_cached_on <- last_cached(index, table, db)

    last_mod_date > last_cached_on

  }

}
