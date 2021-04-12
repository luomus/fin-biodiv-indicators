#' Log message
#'
#' Create a log message.
#'
#' @param ... Message components
#'
#' @export

log_message <- function(...) {
  if (getOption("indicator_logging")) {
    message("[", Sys.time(), "] ", ...)
  }
  invisible(NULL)
}
