#' Multi-species index svg
#'
#' Create an svg from a multispecies indicator plot.
#'
#' @param index Which index?
#' @param use_cache Whether to use cached data.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom grDevices dev.off
#' @importFrom promises is.promise then
#' @importFrom svglite svglite
#' @export

svg_ms_index <- function(index, use_cache, id) {

  data <- ms_index(index, use_cache, id)

  promises::then(
    data,
    ~{
      svg_data(., index, id)
    },
    onRejected = function (err) {

      log_message(id, "An error occured creating an svg image: ", err)

    }
  )

}

#' Species index svg
#'
#' Create an svg from an indicator plot.
#'
#' @param index Which index?
#' @param sp Species.
#' @param use_cache Whether to use cached data.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom grDevices dev.off
#' @importFrom promises is.promise then
#' @importFrom svglite svglite
#' @export

svg_sp_index <- function(index, sp, use_cache, id) {

  data <- sp_index(index, sp, use_cache, id)

  promises::then(
    data,
    ~{
      svg_data(., index, id)
    },
    onRejected = function (err) {

      log_message(id, "An error occured in creating an svg image: ", err)

    }
  )

}

svg_data <- function(data, index, id) {

  hash <- digest::digest(data)

  log_message(id, "Checking output cache for plot data")

  cached_data <- get_from_output_cache(hash)

  if (output_cache_valid(cached_data)) {

    log_message(id, "Getting plot data from output cache")

    unlist(cached_data[["data"]])

  } else {

    tmp <- tempfile(tmpdir = "tmp", fileext = ".svgz")
    on.exit(unlink(tmp))

    svglite::svglite(tmp)
    plot_index(data, id)
    grDevices::dev.off()

    svg <- readBin(tmp, "raw", n = file.info(tmp)[["size"]])

    set_output_cache(hash, svg)

    svg

  }

}

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
