#' @importFrom finbif finbif_occurrence

last_mod <- function(filter) {

  res <- finbif::finbif_occurrence(
    filter = filter, select = "load_date", order_by = "-load_date", n = 1L,
    quiet = TRUE
  )

  ans <- as.Date(NULL)

  if (nrow(res) > 1L) {

    ans <- as.Date(res[[1L, 1L]])

  }

  ans

}
