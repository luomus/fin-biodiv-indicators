#' Plot index
#'
#' Create a plot of an index
#'
#' @param data Data to plot
#' @importFrom graphics plot
#' @importFrom lubridate parse_date_time
#' @export

plot_index <- function(data) {
  graphics::plot(
    lubridate::parse_date_time(data[["year"]], "Y"),
    data[["index"]],
    type = "b",
    las = 1,
    xlab = 'Year',
    ylab = "Index"
  )
}

#' Index svg
#'
#' Create an svg from an indicator plot.
#'
#' @param sp Species
#' @param year Year
#' @param base Base year of index
#' @importFrom digest digest
#' @importFrom grDevices dev.off
#' @importFrom promises is.promise then
#' @importFrom svglite svglite
#' @export

svg_index <- function(sp, year, base) {

  data <- sp_index(sp, year, base)

  if (promises::is.promise(data)) {

    promises::then(data, svg_data)

  } else {

    svg_data(data)

  }

}

svg_data <- function(data) {

  hash <- digest::digest(data)

  cached_data <- get_from_cache(hash)

  if (is_cached(cached_data)) {

    unlist(cached_data[["data"]])

  } else {

    tmp <- tempfile(tmpdir = "tmpsvgs", fileext = ".svgz")
    on.exit(unlink(tmp))

    svglite::svglite(tmp)
    plot_index(data)
    grDevices::dev.off()

    svg <- readBin(tmp, "raw", n = file.info(tmp)[["size"]])

    set_cache(hash, svg)

    svg

  }

}
