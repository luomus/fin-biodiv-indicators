#' Get output
#'
#' Get serialized indicator outputs
#'
#' @param output Character. Which type of output?
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param taxon Character. Which taxon?
#' @param db Connection. Database from which to get output.
#'
#' @importFrom config get
#' @importFrom dplyr .data filter pull select tbl
#' @export

get_output <- function(output, index, model, taxon, db) {

  taxon <- switch(taxon, none = NULL, taxon)

  model <- switch(
    model, "default" = names(config::get("model", config = index))[[1L]], model
  )

  index <- paste(c(index, model, taxon), collapse = "_")

  ans <- dplyr::tbl(db, output)

  ans <- dplyr::filter(ans, .data[["index"]] == !!index)

  ans <- dplyr::select(ans, .data[["data"]])

  dplyr::pull(ans)[[1L]]

}
