#' @importFrom config get
#' @importFrom finbif finbif_occurrence
#' @importFrom dplyr .data all_of filter select tbl

get_from_db <- function(x, index, taxon = NULL, db) {

  select <- config::get(x, config = index)[["selection"]]

  index <- paste(c(index, taxon), collapse = "_")

  x <- dplyr::tbl(db, x)

  x <- dplyr::filter(x, .data[["index"]] == !!index)

  dplyr::select(x, dplyr::all_of(select))

}
