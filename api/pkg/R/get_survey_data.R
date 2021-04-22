#' Get survey data
#'
#' Get survey data from finbif.
#'
#' @param name Name of input.
#' @param fltr Filter.
#' @param slct Column selection.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom finbif finbif_occurrence
#' @importFrom promises future_promise promise_resolve

get_survey_data <- function(name, fltr, slct, id) {

  force(name)
  force(fltr)
  force(slct)

  hash <- digest::digest(list(name, fltr, slct))

  log_message(id, "Checking if ", name, " survey data is cached")

  cached <- is_input_cached(hash)

  if (cached && input_cache_valid(hash)) {

    log_message(id, "Getting ", name, " survey data from cache")

    if (input_cache_available(hash)) {

      promises::promise_resolve(get_from_input_cache(hash))

    } else {

      log_message(id, "Waiting for ", name, " survey cache to be available")

      op <- options()

      promises::future_promise({
        options(op)
        wait_for_input_cache_available(hash)
        get_from_input_cache(hash)},
        globals = c("hash", "op"),
        packages = "indicators",
        seed = TRUE
      )

    }

  } else {

    log_message(id, "Setting ", name, " in cache index")

    set_input_cache_index(name, hash, FALSE)

    log_message(id, "Getting ", name, " survey data from FinBIF")

    op <- options()

    promises::future_promise({

      options(op)

      n <- finbif::finbif_occurrence(
        filter = fltr, select = slct, aggregate = "events", count_only = TRUE
      )

      surveys <- finbif::finbif_occurrence(
        filter = fltr, select = slct, aggregate = "events", n = n, quiet = TRUE
      )

      surveys[["n_events"]] <- NULL

      set_input_cache(name, surveys, hash)},
      globals = c("name", "fltr", "hash", "slct", "op"),
      packages = c("finbif", "indicators"),
      seed = TRUE
    )

  }

}
