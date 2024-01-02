#' @importFrom dbplyr window_order
#' @importFrom dplyr arrange

window_arrange <- function(x, ...) {

  UseMethod("window_arrange")

}

window_arrange.default <- function(x, ...) {

  dplyr::arrange(x, ..., by_group = TRUE)

}

window_arrange.tbl_lazy <- function(x, ...) {

  dbplyr::window_order(x, ...)

}
