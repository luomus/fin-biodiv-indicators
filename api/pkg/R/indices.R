#' Index list
#'
#' Get a list of available indices
#'
#' @export

indices <- function() {
  c(`Winter Bird Census` = "wb", `butterflies` = "bf")
}


#' Check index
#'
#' Check if an index is available
#'
#' @param index Which index to check.
#'
#' @export

check_index <- function(index) {
  index <- tolower(index)
  stopifnot(index %in% indices())
  index
}