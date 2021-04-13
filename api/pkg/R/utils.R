#' Log message
#'
#' Create a log message.
#'
#' @param id Request ID for logging.
#' @param ... Message components.
#'
#' @export

log_message <- function(id, ...) {
  if (getOption("indicator_logging")) {
    message("[", substr(id, 1L, 5L), "# ", Sys.time(), "] ", ...)
  }
  invisible(NULL)
}
