#' @importFrom finbif finbif_occurrence

last_modified <- function(fltr) {
  ans <- finbif::finbif_occurrence(
    filter = fltr, select = "load_date", order_by = "-load_date", n = 1L
  )
  ans <- unlist(ans)
  as.Date(ans)
}
