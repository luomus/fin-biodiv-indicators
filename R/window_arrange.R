#' @importFrom dbplyr window_order
#' @importFrom dplyr arrange

window_arrange <- function(x, ...) {

  UseMethod("window_arrange")

}

#' @export
window_arrange.default <- function(x, ...) {

  dplyr::arrange(x, ..., by_group = TRUE)

}

#' @export
window_arrange.tbl_lazy <- function(x, ...) {

  dbplyr::window_order(x, ...)

}
