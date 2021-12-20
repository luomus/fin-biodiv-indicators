#' Update data
#'
#' Update input data from FinBIF.
#'
#' @param type Character. Which type of input data (e.g., surveys or counts)
#' @param index Character. Update the data of which index?
#' @param taxon Character. Update the data for which taxon? Ignored if
#'   `type = "surveys"`
#' @param db Connection. Database in which to update the data from FinBIF.
#'
#' @importFrom config get
#' @importFrom finbif finbif_occurrence

update_data <- function(type, index, taxon, db) {

  taxon <- switch(type, surveys = NULL, taxon)

  filter <- config::get("filters", config = index)

  filter[["taxon_id"]] <- taxon

  select <- config::get(type, config = index)[["selection"]]

  index <- paste(c(index, taxon), collapse = "_")

  cache_date <- sprintf("%s_cached_date", type)

  if (needs_update(index, filter, cache_date, db)) {

    aggregate <- switch(type, surveys = "events", "none")

    message(
      sprintf(
        "INFO [%s] Fetching %s data for %s from FinBIF", Sys.time(), type, index
      )
    )

    data <- finbif::finbif_occurrence(
      filter = filter, select = select, aggregate = aggregate, n = "all",
      quiet = TRUE
    )

    message(
      sprintf(
        "INFO [%s] Fetched %s %s for %s", Sys.time(), attr(data, "nrec_dnld"),
        type, index
      )
    )

    data[["n_events"]] <- NULL

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
