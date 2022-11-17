#' @importFrom config get

needs_update <- function(index, filter, table, db) {

  ans <- Sys.getenv("CHECK_UPDATE", TRUE)

  ans <- as.logical(ans)

  if (isFALSE(ans)) {

    ans

  } else {

    last_mod_date <- last_mod(filter)

    last_cached_on <- last_cached(index, table, db)

    out_of_date <- !isTRUE(last_cached_on > last_mod_date)

    current_year <- as.integer(format(Sys.Date(), "%Y"))

    use_after_date <- config::get("use_data_after", config = index)

    use_after_date <- as.Date(paste(current_year, use_after_date, sep = "-"))

    use_current_year <- Sys.Date() >= use_after_date

    cached_before_using <- last_cached_on < use_after_date

    out_of_date | use_current_year & cached_before_using

  }

}
