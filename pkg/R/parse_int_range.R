#' Parse integer range string
#'
#' Parse a string indicating a range of integers.
#'
#' @param ints Integer range as a string.
#' @export

parse_int_range <- function(ints) {

  ints <- strsplit(ints, "\\D+")
  ints <- ints[[1L]]
  ints <- as.integer(ints)
  ints <- rep_len(ints, 2L)

  seq.int(ints[[1L]], ints[[2L]])

}
