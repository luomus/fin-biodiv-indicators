#' Plot index
#'
#' Create a plot of an index
#'
#' @param data Data to plot.
#' @param id Request ID for logging.
#'
#' @importFrom ggplot2 aes ggplot geom_ribbon geom_line theme_minimal xlab ylab
#' @importFrom lubridate parse_date_time
#' @export

plot_index <- function(data, id) {

  log_message(id, "Creating a plot")

  year <- data[["year"]]
  index <- data[["index"]]
  sd <- data[["sd"]]

  gg <- ggplot2::ggplot() +
    ggplot2::aes(
      x = lubridate::parse_date_time(year, "Y"),
      y = index,
      ymin = index - sd,
      ymax = index + sd
    ) +
    ggplot2::geom_ribbon(alpha = .2) +
    ggplot2::geom_line() +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_minimal()

  print(gg)

}

#' Index svg
#'
#' Create an svg from an indicator plot.
#'
#' @param sp Species.
#' @param year Year.
#' @param base Base year of index.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom grDevices dev.off
#' @importFrom promises is.promise then
#' @importFrom svglite svglite
#' @export

svg_index <- function(sp, year, base, id) {

  data <- sp_index(sp, year, base, id)

  if (promises::is.promise(data)) {

    promises::then(data, ~{svg_data(., id)})

  } else {

    svg_data(data, id)

  }

}

svg_data <- function(data, id) {

  hash <- digest::digest(data)

  log_message(id, "Checking output cache for plot data")

  cached_data <- get_from_output_cache(hash)

  if (is_output_cached(cached_data)) {

    log_message(id, "Getting plot data from output cache")

    unlist(cached_data[["data"]])

  } else {

    tmp <- tempfile(tmpdir = "tmpsvgs", fileext = ".svgz")
    on.exit(unlink(tmp))

    svglite::svglite(tmp)
    plot_index(data, id)
    grDevices::dev.off()

    svg <- readBin(tmp, "raw", n = file.info(tmp)[["size"]])

    set_output_cache(hash, svg)

    svg

  }

}
