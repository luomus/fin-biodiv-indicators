#' @importFrom config get
#' @importFrom finbif finbif_occurrence
#' @importFrom dplyr .data all_of filter select tbl

get_from_db <- function(db, tbl, index, taxa, select) {

  if (missing(select)) {

    index_base <- sub("_north|_south", "", index)

    select <- config::get(tbl, config = index_base)[["selection"]]

    abundance <- config::get(tbl, config = index_base)[["abundance"]]

    select[select == abundance] <- "abundance"

  }

  if (!missing(taxa)) index <- paste(index, taxa, sep = "_")

  tbl <- dplyr::tbl(db, tbl)

  tbl <- dplyr::filter(tbl, .data[["index"]] %in% !!index)

  dplyr::select(tbl, dplyr::all_of(select))

}
