#' Get species observation data
#'
#' Get species observation data from finbif.
#'
#' @param sp Species.
#' @param type Survey type.
#'
#' @importFrom digest digest
#' @importFrom dplyr arrange first left_join matches mutate select
#' @importFrom finbif finbif_occurrence
#' @importFrom lubridate day month today quarter
#' @importFrom promises future_promise promise_resolve then
#' @importFrom rlang .data
#' @importFrom tidyr replace_na pivot_longer pivot_wider
#' @importFrom tidyselect all_of

get_sp_data <- function(sp, type) {

  force(sp)

  hash <- digest::digest(list(sp, type))

  log_message("Checking if ", sp, "'s ", type, " data is cached")

  cached <- is_input_cached(hash)

  if (cached && input_cache_valid(hash)) {

    log_message("Getting ", sp, "'s ", type, " data from input cache")

    promises::promise_resolve(get_from_input_cache(hash, sp, type))

  } else {

    sp <- species[[type]][[sp]]

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

    log_message("Getting ", type, " survey data")

    surveys <- get_survey_data(
      paste0(type, "_surveys"),
      fltr, c("event_id", "location_id", "date_time")
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


    log_message("Getting ", sp, " count data from FinBIF")

    sp_data <- promises::future_promise({
      n <- finbif::finbif_occurrence(
        sp, filter = fltr, select = slct, count_only = TRUE
      )

      finbif::finbif_occurrence(
        sp, filter = fltr, select = slct, n = n, quiet = TRUE
      )
      },
      globals = c("sp", "fltr", "slct"),
      packages = "finbif",
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
          names_from = .data[["taxon_id"]],
          values_from = .data[["abundance"]], values_fill = 0L,
          values_fn = dplyr::first
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

        set_input_cache(paste0(type, "counts"), counts, hash, sp)
      }
    )
  }
}

#' @importFrom tidyselect vars_select_helpers

where <- tidyselect::vars_select_helpers[["where"]]

max_gt_zero <- function(x) max(x, na.rm = TRUE) > 0