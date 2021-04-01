#' @importFrom digest digest

is_input_cached <- function(x) {
  hash <- digest::digest(x)
  FALSE
}

input_cache_valid <- function(x) {
  TRUE
}

get_from_input_cache <- function(x) {
  NULL
}

set_input_cache <- function(x) {
  x
}

