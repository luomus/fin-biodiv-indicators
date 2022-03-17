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

  surveys <- get_from_db(db, "surveys", index)

  counts <- get_from_db(db, "counts", index, taxon[["code"]])

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

  model_data <- tryCatch(
    run_model(index, taxon, surveys, counts, model),
    error = err_msg
  )

  cond <- !inherits(model_data, "error")
  cond <- cond && length(which(is.finite(model_data[["time"]]))) > 0L

  if (cond) {

    index_taxon <- paste(index, model, taxon[["code"]], sep = "_")

    cache_outputs(index_taxon, model_data, db)

    model_data[["index"]] <- index_taxon

    set_cache(index_taxon, "model_output", model_data, db)

  }

  invisible(NULL)

}

err_msg <- function(x) {

  message(sprintf("ERROR [%s] %s", Sys.time(), x[["message"]]))

  x

}

