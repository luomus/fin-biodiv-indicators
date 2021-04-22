#' Get species observation data
#'
#' Get species observation data from finbif.
#'
#' @param sp Species.
#' @param type Survey type.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom dplyr arrange first left_join matches mutate select
#' @importFrom finbif finbif_occurrence
#' @importFrom lubridate day month today quarter
#' @importFrom promises future_promise promise_resolve then
#' @importFrom rlang .data
#' @importFrom tidyr replace_na pivot_longer pivot_wider
#' @importFrom tidyselect all_of

get_sp_data <- function(sp, type, id) {

  force(sp)

  hash <- digest::digest(list(sp, type))

  log_message(id, "Checking if ", sp, "'s ", type, " data is cached")

  cached <- is_input_cached(hash)

  if (cached && input_cache_valid(hash)) {

    log_message(id, "Getting ", sp, "'s ", type, " data from input cache")

    if (input_cache_available(hash)) {

      promises::promise_resolve(get_from_input_cache(hash, sp, type))

    } else {

      log_message(
        id, "Waiting for ", sp, "'s ", type, " survey cache to be available"
      )

      op <- options()

      promises::future_promise({
        options(op)
        wait_for_input_cache_available(hash)
        get_from_input_cache(hash, sp, type)},
        globals = c("hash", "sp", "type", "op"),
        packages = "indicators",
        seed = TRUE
      )

    }

  } else {

    log_message(id, "Setting ", sp, " ", type, " count data in cache index")

    cache_name <- input_cache_name(type, "counts")

    set_input_cache_index(cache_name, hash, FALSE)

    sp_id <- species[[type]][[sp]]

    begin_date <- "1958-12-01"

    end_date <- lubridate::today()
    lubridate::month(end_date) <- 1L
    lubridate::day(end_date) <- 31L

    fltr <- list(
      date_range_ymd = list(begin_date, end_date),
      date_range_md = c("12-01", "01-31"),
      collection = "Winter Bird Census",
      quality_issues = "both"
    )

    log_message(id, "Getting ", type, " survey data")

    surveys <- get_survey_data(
      input_cache_name(type, "surveys"),
      fltr, c("event_id", "location_id", "date_time"),
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
