#' @importFrom dplyr arrange first left_join matches mutate select
#' @importFrom finbif finbif_occurrence
#' @importFrom lubridate day month today quarter
#' @importFrom rlang .data
#' @importFrom tidyr replace_na pivot_longer pivot_wider
#' @importFrom tidyselect all_of

get_sp_data <- function(sp, type) {

  x <- list(sp, type)

  cached <- is_input_cached(x)

  if (cached && input_cache_valid(x)) {

    get_from_input_cache(x)

  } else {

    sp <- species[[type]][[sp]]

    begin_date <- "1958-12-01"

    end_date <- lubridate::today()
    lubridate::month(end_date) <- 1L
    lubridate::day(end_date) <- 31L

    fltr <- list(
      date_range_ymd = list(begin_date, end_date),
      date_range_md = c("12-01", "01-31"),
      collection = "Winter Bird Census"
    )

    surveys <- get_survey_data(fltr)
    surveys <- dplyr::mutate(
      surveys,
      year = floor(lubridate::quarter(.data[["date_time"]], TRUE, 12L))
    )

    slct <- c("event_id", "taxon_id", "abundance")

    n <- finbif::finbif_records(
      filter = c(fltr, taxon_id = sp), select = slct, count_only = TRUE
    )

    counts <- finbif::finbif_occurrence(
      sp, filter = fltr, select = slct, n = n$content$total, quiet = TRUE
    )
    counts <- dplyr::left_join(surveys, counts, by = "event_id")
    counts <- dplyr::arrange(counts, .data[["date_time"]])
    counts <- dplyr::mutate(
      counts, taxon_id = tidyr::replace_na(.data[["taxon_id"]], "NO_TAXA")
    )
    counts <- tidyr::pivot_wider(
      counts, tidyselect::all_of(c("year", "location_id")),
      names_from = .data[["taxon_id"]],
      values_from = .data[["abundance"]], values_fill = 0,
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

    set_input_cache(counts)

  }

}
#' @importFrom tidyselect vars_select_helpers
where <- tidyselect::vars_select_helpers[["where"]]

max_gt_zero <- function(x) max(x, na.rm = TRUE) > 0
