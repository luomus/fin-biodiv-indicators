#' Get output
#'
#' Get serialized indicator outputs
#'
#' @param output Character. Which type of output?
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param taxon Character. Which taxon?
#' @param region Character. Which region?
#' @param db Connection. Database from which to get output.
#'
#' @importFrom config get
#' @importFrom dplyr .data all_of filter pull select tbl
#' @export

get_output <- function(output, index, model, taxon, region, db) {

  taxon <- switch(taxon, none = NULL, taxon)

  model <- switch(
    model, "default" = names(config::get("model", config = index))[[1L]], model
  )

  region <- switch(region, north = region, south = region, NULL)

  index <- paste(c(index, region, model, taxon), collapse = "_")

  ans <- dplyr::tbl(db, output)

  ans <- dplyr::filter(ans, .data[["index"]] == !!index)

  ans <- dplyr::pull(ans, dplyr::any_of("data", "state"))

  if (length(ans) < 1L) {

    ans <- dplyr::tbl(db, "model_state")

    ans <- dplyr::filter(ans, .data[["index"]] == !!index)

    ans <- dplyr::select(ans, dplyr::all_of(c("state", "time")))

    ans <- dplyr::collect(ans)

    if (nrow(ans) < 1L)  {

      ans <- list(state = "not_run")

    } else {

      ans <- as.list(ans)

    }

  } else {

    ans <- ans[[1L]]

  }

  ans

}
