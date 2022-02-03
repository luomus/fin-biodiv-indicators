#' Update taxon index
#'
#' Update the relative abundance index for a taxon.
#'
#' @param index Character. Update which index?
#' @param taxon Character. Update the data for which taxa?
#' @param db Connection. Database in which to update index.
#'
#' @importFrom config get
#' @importFrom dplyr .data collect select
#' @importFrom rtrim count_summary index trim
#' @export

update_taxon_index <- function(index, taxon, db) {

  surveys <- get_from_db("surveys", index, NULL, db)

  counts <- get_from_db("counts", index, taxon[["code"]], db)

  for (i in config::get("surveys", config = index)[["process"]]) {

    surveys <- do.call(process_funs[[i]], list(surveys))

  }

  for (i in config::get("counts", config = index)[["process"]]) {

    counts <- do.call(
      process_funs[[i]],
      list(counts = counts, surveys = surveys, taxon = taxon)
    )

  }

  counts <- dplyr::select(
    counts, .data[["abundance"]], .data[["location_id"]], .data[["year"]]
  )

  counts <- dplyr::collect(counts)

  od <- config::get("model", config = index)[["trim"]][["overdispersion"]]

  message(
    sprintf(
      "INFO [%s] Calculating %s index for %s", Sys.time(), index,
      taxon[["code"]]
    )
  )

  trim <- rtrim::trim(
    abundance ~ location_id + year, data = counts, changepoints = "all",
    overdisp = od
  )

  base <- config::get("model", config = index)[["trim"]][["base_year"]]

  base <- which(trim[["time.id"]] == base)

  trim <- rtrim::index(trim, base = base)

  index_taxon <- paste(index, taxon[["code"]], sep = "_")

  attr(trim, "count_summary") <- rtrim::count_summary(
    as.data.frame(counts), "abundance", "location_id"
  )[-2L]

  cache_outputs(index_taxon, trim, db)

  trim[["index"]] <- index_taxon

  set_cache(index_taxon, "trim", trim, db)

  invisible(NULL)

}
