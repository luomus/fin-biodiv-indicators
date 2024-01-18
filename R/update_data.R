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

  filter[["taxon_id"]] <- c(taxon[["code"]], taxon[["extra_codes"]])

  filter[["subtaxa"]] <- taxon[["subtaxa"]]

  filter[["has_value"]] <- config::get(type, config = index)[["has_value"]]

  select <- config::get(type, config = index)[["selection"]]

  select <- switch(type, surveys = c(select, "municipality"), select)

  abundance <- config::get("counts", config = index)[["abundance"]]

  index <- paste(c(index, taxon[["code"]]), collapse = "_")

  cache_date <- sprintf("%s_cached_date", type)

  if (needs_update(index, filter, cache_date, db) || do_update) {

    aggregate <- switch(type, surveys = "events", "none")

    writeLines(
      sprintf(
        "INFO [%s] Fetching %s data for %s from FinBIF",
        format(Sys.time()),
        type,
        index
      )
    )

    names(select) <- select

    names(select)[select == abundance] <- "abundance"

    data <- finbif::finbif_occurrence(
      filter = filter, select = select, aggregate = aggregate, n = "all",
      quiet = TRUE, aggregate_counts = FALSE
    )

    writeLines(
      sprintf(
        "INFO [%s] Fetched %s %s for %s",
        format(Sys.time()),
        attr(data, "nrec_dnld"),
        type, index
      )
    )

    index_data <- character()

    if (nrow(data) > 0L) {

      index_data <- index

    }

    data[["index"]] <- index_data

    regions <- switch(type, surveys = to_region(data[["municipality"]]), NULL)

    data[["municipality"]] <- NULL

    set_cache(index, type, data, db)

    set_cache(
      index, cache_date, data.frame(index = index, date = Sys.Date()), db
    )

    if (!is.null(regions)) {

      for (i in c("north", "south")) {

        ind_i <- !is.na(regions) & regions == i

        index_region <- paste(index, i, sep = "_")

        index_data <- character()

        if (nrow(data[ind_i, ]) > 0L) {

          index_data <- index_region

        }

        data[ind_i, "index"] <- index_data

        set_cache(index_region, type, data[ind_i, ], db)

        set_cache(
          index_region, cache_date,
          data.frame(index = index_region, date = Sys.Date()),
          db
        )

      }

    }

    TRUE

  } else {

    FALSE

  }

}

to_region <- function(x) {

  as.vector(municipalities[x])

}
