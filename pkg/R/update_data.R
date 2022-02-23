#' Update data
#'
#' Update input data from FinBIF.
#'
#' @param type Character. Which type of input data (e.g., surveys or counts)
#' @param index Character. Update the data of which index?
#' @param taxon Character. Update the data for which taxon? Ignored if
#'   `type = "surveys"`
#' @param db Connection. Database in which to update the data from FinBIF.
#' @param do_update Logical. Update data regardless of need.
#'
#' @importFrom config get
#' @importFrom finbif finbif_occurrence
#' @export

update_data <- function(type, index, taxon, db, do_update = FALSE) {

  taxon <- switch(type, surveys = NULL, taxon)

  filter <- config::get("filters", config = index)

  filter[["taxon_id"]] <- taxon[["code"]]

  filter[["has_value"]] <- config::get(type, config = index)[["has_value"]]

  select <- config::get(type, config = index)[["selection"]]

  abundance <- config::get("counts", config = index)[["abundance"]]

  index <- paste(c(index, taxon[["code"]]), collapse = "_")

  cache_date <- sprintf("%s_cached_date", type)

  if (needs_update(index, filter, cache_date, db) || do_update) {

    aggregate <- switch(type, surveys = "events", "none")

    message(
      sprintf(
        "INFO [%s] Fetching %s data for %s from FinBIF", Sys.time(), type, index
      )
    )

    data <- finbif::finbif_occurrence(
      filter = filter, select = select, aggregate = aggregate, n = "all",
      quiet = TRUE, aggregate_counts = FALSE
    )

    data <- data[!is.na(data[["document_id"]]), ]

    message(
      sprintf(
        "INFO [%s] Fetched %s %s for %s", Sys.time(), attr(data, "nrec_dnld"),
        type, index
      )
    )

    abundance_data <- data[[abundance]]

    data[[abundance]] <- NULL

    data[["abundance"]] <- abundance_data

    data[["index"]] <- index

    set_cache(index, type, data, db)

    set_cache(
      index, cache_date, data.frame(index = index, date = Sys.Date()), db
    )

    TRUE

  } else {

    FALSE

  }

}
