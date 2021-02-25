#' Plot indicator
#'
#' Create a plot of an indicator over a range of years.
#'
#' @param year Years to plot.
#' @param type Type of indicator.
#' @importFrom graphics plot
#' @importFrom lubridate parse_date_time
#' @export

plot_indicator <- function(year, type) {
  graphics::plot(
    lubridate::parse_date_time(year, "Y"),
    stats::runif(length(year)),
    type = "b",
    main = type,
    las = 1,
    xlab = 'Year',
    ylab = "Indicator"
  )
}

#' @import rlang
NULL

#' Indicator svg
#'
#' Create an svg from an indicator plot.
#'
#' @inheritParams plot_indicator
#' @importFrom blob blob
#' @importFrom grDevices dev.off
#' @importFrom svglite svglite
#' @export

indicator_svg <- function(year, type) {

  cached_data <- get_from_cache("graph", year == !!year, type == !!type)

  if (is_cached(cached_data)) {

    unlist(cached_data[["svg"]])

  } else {

    parsed_years <- parse_int_range(year)

    tmp <- tempfile(tmpdir = "tmpsvgs", fileext = ".svgz")
    on.exit(unlink(tmp))

    svglite::svglite(tmp)
    plot_indicator(parsed_years, type)
    grDevices::dev.off()

    svg <- readBin(tmp, "raw", n = file.info(tmp)[["size"]])

    set_cache(
      "graph", year = paste(range(parsed_years), collapse = ":"), type = type,
      svg = blob::blob(svg)
    )

    svg

  }

}
