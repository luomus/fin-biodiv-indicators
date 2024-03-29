#' @importFrom blob blob
#' @importFrom ggplot2 aes ggplot geom_line geom_ribbon theme_minimal xlab ylab
#' @importFrom grDevices dev.off
#' @importFrom lubridate parse_date_time
#' @importFrom svglite svglite

cache_outputs <- function(index, df, db) {

  set_cache(
    index, "output_cache_time", data.frame(index = index, time = Sys.time()), db
  )

  data_csv <- data.frame(index = index, data = blob::blob(serialize(df, NULL)))

  set_cache(index, "data_csv", data_csv, db)

  data <- cbind(df[["mean"]], df[["upper"]], df[["lower"]])

  data <- list(
    data = data, pointStart = min(df[["time"]]), pointInterval = 1,
    pointIntervalUnit = "year"
  )

  data <- data.frame(index = index, data = blob::blob(serialize(data, NULL)))

  set_cache(index, "data", data, db)

  count_summary <- attr(df, "count_summary")

  count_summary <- data.frame(
    index = index, data = blob::blob(serialize(count_summary, NULL))
  )

  set_cache(index, "count_summary", count_summary, db)

  trends <- attr(df, "trends")

  trends <- data.frame(
    index = index, data = blob::blob(serialize(trends, NULL))
  )

  set_cache(index, "trends", trends, db)

  p <- withCallingHandlers(
    tryCatch(
      {
        ggplot2::ggplot() +
          ggplot2::aes(
            x = lubridate::parse_date_time(df[["time"]], "Y"),
            y = df[["mean"]],
            ymin = df[["lower"]],
            ymax = df[["upper"]]
          ) +
          ggplot2::geom_ribbon(alpha = .2) +
          ggplot2::geom_line(na.rm = TRUE) +
          ggplot2::ylab(NULL) +
          ggplot2::xlab(NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(plot.margin = ggplot2::unit(c(1L, 1L, 1L, 1L), "cm"))
      },
      error = err_msg
    ),
    warning = warn_msg,
    message = msg_msg
  )

  tmp <- tempfile(fileext = ".svgz")

  on.exit(unlink(tmp))

  svglite::svglite(tmp, fix_text_size = FALSE)

  print(p)

  grDevices::dev.off()

  svg <- readBin(tmp, "raw", n = file.info(tmp)[["size"]])

  svg <- data.frame(index = index, data = blob::blob(svg))

  set_cache(index, "svg", svg, db)

}
