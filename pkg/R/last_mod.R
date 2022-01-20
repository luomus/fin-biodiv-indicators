#' @importFrom finbif finbif_occurrence

last_mod <- function(filter) {

  ans <- finbif::finbif_occurrence(
    filter = filter, select = "load_date", order_by = "-load_date", n = 1L,
    quiet = TRUE
  )

  as.Date(ans[[1L, 1L]])

}
