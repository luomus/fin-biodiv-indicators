#' Get survey data
#'
#' Get survey data from finbif.
#'
#' @param name Name of input.
#' @param fltr Filter.
#' @param slct Column selection.
#'
#' @importFrom digest digest
#' @importFrom finbif finbif_occurrence
#' @importFrom promises future_promise promise_resolve

get_survey_data <- function(name, fltr, slct) {

  force(name)
  force(fltr)
  force(slct)

  hash <- digest::digest(list(name, fltr, slct))

  log_message("Checking if ", name, " survey data is cached")

  cached <- is_input_cached(hash)

  if (cached && input_cache_valid(hash)) {

    log_message("Getting ", name, " survey data from cache")

    promises::promise_resolve(get_from_input_cache(hash))

  } else {

    log_message("Getting ", name, " survey data from FinBIF")

    promises::future_promise({

      n <- finbif::finbif_occurrence(
        filter = fltr, select = slct, aggregate = "events", count_only = TRUE
      )

      surveys <- finbif::finbif_occurrence(
        filter = fltr, select = slct, aggregate = "events", n = n, quiet = TRUE
      )

      set_input_cache(name, surveys, hash)},
      globals = c("name", "fltr", "hash", "slct"),
      packages = c("finbif", "indicators"),
      seed = TRUE
    )

  }

}
