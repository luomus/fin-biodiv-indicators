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

  fltr <- filter_gen(index)

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

    surveys <- get_survey_data(
      input_cache_name(index, "surveys"),
      fltr,
      c("event_id", "location_id", "date_time"),
      last_mod_time,
      id
    )

    surveys <- promises::then(
      surveys,
      ~{
        dplyr::mutate(
          ., year = floor(lubridate::quarter(.data[["date_time"]], TRUE, 12L))
        )
      }
    )

    slct <- c("event_id", "taxon_id", "abundance")

    log_message(id, "Getting ", sp, " count data from FinBIF")

    op <- options()

    sp_data <- promises::future_promise({
      options(op)

      n <- finbif::finbif_occurrence(
        sp_id, filter = fltr, select = slct, count_only = TRUE
      )

      finbif::finbif_occurrence(
        sp_id, filter = fltr, select = slct, n = n, quiet = TRUE
      )
      },
      globals = c("sp_id", "fltr", "slct", "op"),
      packages = c("finbif", "indicators"),
      seed = TRUE
    )

    all_data <- promises::promise_all(surveys = surveys, sp_data = sp_data)

    then(
      all_data,
      ~{
        counts <- dplyr::left_join(
          .[["surveys"]], .[["sp_data"]], by = "event_id"
        )
        counts <- dplyr::arrange(counts, .data[["date_time"]])
        counts <- dplyr::mutate(
          counts, taxon_id = tidyr::replace_na(.data[["taxon_id"]], "NO_TAXA")
        )
        counts <- tidyr::pivot_wider(
          counts, tidyselect::all_of(c("year", "location_id")),
          names_from = .data[["taxon_id"]], values_from = .data[["abundance"]],
          values_fill = 0L, values_fn = dplyr::first
        )
        counts <- dplyr::select(counts, dplyr::matches("[^NO_TAXA]"))
        counts <- tidyr::pivot_wider(
          counts, .data[["year"]], names_from = .data[["location_id"]],
          values_from = !tidyselect::all_of(c("year", "location_id"))
        )
        counts <- dplyr::select(counts, .data[["year"]], where(max_gt_zero))
        counts <- tidyr::pivot_longer(
          counts, !.data[["year"]], names_to = "site", values_to = "count",
          values_drop_na = TRUE
        )

        set_input_cache(cache_name, counts, hash, sp)
      }
    )
  }
}

#' @importFrom tidyselect vars_select_helpers

where <- tidyselect::vars_select_helpers[["where"]]

max_gt_zero <- function(x) max(x, na.rm = TRUE) > 0
