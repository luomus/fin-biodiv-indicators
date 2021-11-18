#' Get species observation data
#'
#' Get species observation data from finbif.
#'
#' @param index Which index.
#' @param sp Species.
#' @param use_cache Whether to use cached data.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom dplyr arrange first left_join matches mutate select
#' @importFrom finbif finbif_occurrence
#' @importFrom lubridate quarter
#' @importFrom promises future_promise promise_resolve then
#' @importFrom rlang .data
#' @importFrom tidyr replace_na pivot_longer pivot_wider
#' @importFrom tidyselect all_of

get_sp_data <- function(index, sp, use_cache, id) {

  force(sp)

  hash <- digest::digest(list(index, sp))

  log_message(id, "Checking if ", sp, "'s ", index, " data is cached")

  cached <- is_input_cached(hash)

  fltr <- get_filter(index)

  if (use_cache) {

    last_mod_time <- as.Date("1970-01-01")

  } else {

    last_mod_time <- last_modified(fltr)

  }

  if (cached && input_cache_valid(hash, last_mod_time)) {

    log_message(id, "Getting ", sp, "'s ", index, " data from input cache")

    if (input_cache_available(hash)) {

      promises::promise_resolve(get_from_input_cache(hash, sp, index))

    } else {

      log_message(
        id, "Waiting for ", sp, "'s ", index, " survey cache to be available"
      )

      op <- options()

      promises::future_promise({
        options(op)
        wait_for_input_cache_available(hash)
        get_from_input_cache(hash, sp, index)},
        globals = c("hash", "sp", "index", "op"),
        packages = "indicators",
        seed = TRUE
      )

    }

  } else {

    log_message(id, "Setting ", sp, " ", index, " count data in cache index")

    cache_name <- input_cache_name(index, "counts")

    set_input_cache_index(cache_name, hash, FALSE)

    sp_id <- species(index)[[sp]]

    log_message(id, "Getting ", index, " survey data")

    slct <- c("document_id", "location_id", "year", "month", "day")

    surveys <- get_survey_data(index, fltr, slct, last_mod_time, id)

    slct <- c("document_id", "section", "abundance")

    log_message(id, "Getting ", sp, " count data from FinBIF")

    op <- options()

    sp_data <- promises::future_promise({
      options(op)

      finbif::finbif_occurrence(
        sp_id, filter = fltr, select = slct, n = "all", quiet = TRUE
      )
      },
      globals = c("sp_id", "fltr", "slct", "op"),
      packages = c("finbif", "indicators"),
      seed = TRUE
    )

    all_data <- promises::promise_all(surveys = surveys, sp_data = sp_data)

    process_counts <- get_process_counts_fun(index)

    promises::then(
      all_data,
      ~{

        counts <- process_counts(.)

        set_input_cache(cache_name, counts, hash, sp)

        counts

      }
    )

  }
}

#' @importFrom tidyselect vars_select_helpers

where <- tidyselect::vars_select_helpers[["where"]]

max_gt_zero <- function(x) max(x, na.rm = TRUE) > 0
