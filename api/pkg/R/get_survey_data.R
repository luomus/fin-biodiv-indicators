#' @importFrom dplyr filter
#' @importFrom finbif finbif_occurrence

get_survey_data <- function(fltr) {

  cached <- is_input_cached(fltr)

  if (cached && input_cache_valid(fltr)) {

    get_from_input_cache(fltr)

  } else {

    slct <- c("event_id", "location_id", "date_time")

    n <- finbif::finbif_records(
      filter = fltr, select = slct, aggregate = "events", count_only = TRUE
    )

    surveys <- finbif::finbif_occurrence(
      filter = fltr, select = slct, aggregate = "events", n = n$content$total,
      quiet = TRUE
    )

    surveys <- dplyr::filter(surveys, !is.na(.data[["location_id"]]))

    set_input_cache(surveys)

  }

}
