#' @importFrom memoise memoise timeout

.onLoad <- function(libname, pkgname) {
  last_modified <<- memoise::memoise(
    last_modified, ~memoise::timeout(60L * 60L * 24L)
  )
}
