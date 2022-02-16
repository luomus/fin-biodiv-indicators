#' Update taxon index
#'
#' Update the relative abundance index for a taxon.
#'
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param taxon Character. Update the data for which taxa?
#' @param db Connection. Database in which to update index.
#'
#' @importFrom config get
#' @export

update_taxon_index <- function(index, model, taxon, db) {

  surveys <- get_from_db("surveys", index, NULL, db)

  counts <- get_from_db("counts", index, taxon[["code"]], db)

  model_spec <- config::get("model", config = index)[[model]]

  for (i in model_spec[["surveys_process"]]) {

    surveys <- do.call(process_funs()[[i]], list(surveys))

  }

  for (i in model_spec[["counts_process"]]) {

    counts <- do.call(
      process_funs()[[i]],
      list(counts = counts, surveys = surveys, taxon = taxon)
    )

  }

  model_data <- run_model(index, taxon, surveys, counts, model)

  index_taxon <- paste(index, model, taxon[["code"]], sep = "_")

  cache_outputs(index_taxon, model_data, db)

  model_data[["index"]] <- index_taxon

  set_cache(index_taxon, "model_output", model_data, db)

  invisible(NULL)

}
