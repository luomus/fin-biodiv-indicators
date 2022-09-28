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
#' @importFrom dplyr filter
#' @export

update_taxon_index <- function(index, model, taxon, db) {

  surveys <- get_from_db(db, "surveys", index)

  index_base <- sub("_north|_south", "", index)

  counts <- get_from_db(db, "counts", index_base, taxon[["code"]])

  model_spec <- config::get("model", config = index_base)[[model]]

  for (i in model_spec[["surveys_process"]]) {

    surveys <- do.call(process_funs()[[i]], list(surveys))

  }

  for (i in model_spec[["counts_process"]]) {

    counts <- do.call(
      process_funs()[[i]],
      list(counts = counts, surveys = surveys, taxon = taxon)
    )

  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  next_year <- current_year + 1L

  surveys <- dplyr::filter(surveys, .data[["year"]] < next_year)

  counts <- dplyr::filter(counts, .data[["year"]] < next_year)

  use_after_date <- config::get("use_data_after", config = index_base)

  use_after_date <- paste(current_year, use_after_date, sep = "-")

  use_current_year <- Sys.Date() >= as.Date(use_after_date)

  use_current_year_env <- as.logical(
    Sys.getenv(paste0(index_base, "_UCY"), "true")
  )

  use_current_year <- use_current_year && use_current_year_env

  if (!use_current_year) {

    surveys <- dplyr::filter(surveys, .data[["year"]] < current_year)

    counts <- dplyr::filter(counts, .data[["year"]] < current_year)

  }

  model_data <- withCallingHandlers(
    tryCatch(
      run_model(index_base, taxon, surveys, counts, model),
      error = err_msg
    ),
    warning = warn_msg
  )

  cond <- !inherits(model_data, "error")
  cond <- cond && length(which(is.finite(model_data[["time"]]))) > 0L

  index_taxon <- paste(index, model, taxon[["code"]], sep = "_")

  if (cond) {

    cache_outputs(index_taxon, model_data, db)

    model_data[["index"]] <- index_taxon

    set_cache(
      index_taxon, "model_state",
      data.frame(index = index_taxon, state = "success", time = Sys.time()), db
    )

    set_cache(index_taxon, "model_output", model_data, db)

  } else {

    set_cache(
      index_taxon, "model_state",
      data.frame(index = index_taxon, state = "fail", time = Sys.time()), db
    )

  }

  invisible(NULL)

}

err_msg <- function(x) {

  message(
    sprintf(
      "ERROR [%s] %s",
      Sys.time(),
      gsub("\n|\r|\r\n", "; ", x[["message"]])
    )
  )

  x

}

warn_msg <- function(x) {

  message(
    sprintf(
      "WARNING [%s] %s",
      Sys.time(),
      gsub("\n|\r|\r\n", "; ", x[["message"]])
    )
  )

  invokeRestart("muffleWarning")

}
