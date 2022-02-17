#' Update index
#'
#' Update index output data.
#'
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param db Connection. Database in which to update index.
#'
#' @importFrom config get
#' @importFrom dplyr .data collect count distinct group_by filter full_join
#' @importFrom dplyr inner_join lag lead mutate pull right_join row_number
#' @importFrom dplyr summarise sql tbl ungroup
#' @importFrom stats sd
#' @export

update_index <- function(index, model, db) {

  n <- 1000L
  maxcv <- 3
  minindex <- .01
  trunc <- 10
  taxa <-  vapply(config::get("taxa", config = index), getElement, "", "code")

  df <- dplyr::tbl(db, "model_output")

  df <- dplyr::filter(
    df, .data[["index"]] %in% !!paste(index, model, taxa, sep = "_")
  )

  years <- sort(dplyr::pull(dplyr::distinct(df, .data[["time"]])))

  nyears <- length(years)

  base <- config::get("model", config = index)[[model]][["base_year"]]

  base <- which(years == base)

  esab <- nyears - base + 1L

  nrows <- dplyr::pull(dplyr::count(df))

  df <- dplyr::mutate(
    df,
    cv = ifelse(
      .data[["mean"]] >= .1 & .data[["sd"]] > 0,
      .data[["sd"]] / .data[["mean"]],
      NA_real_
    )
  )

  df <- dplyr::group_by(df, .data[["index"]])

  df <- dplyr::mutate(df, cv = mean(.data[["cv"]], na.rm = TRUE))

  df <- dplyr::filter(df, .data[["cv"]] < maxcv)

  df <- dplyr::mutate(
    df, imputed = pmax(.data[["mean"]], minindex, na.rm = TRUE)
  )

  df <- dplyr::mutate(
    df,
    se_imp = ifelse(.data[["mean"]] > minindex, .data[["sd"]], 0)
  )

  seq_n <- dplyr::tbl(
    db,
    dplyr::sql(sprintf("SELECT generate_series(1, %s) AS j", n))
  )

  rand <- dplyr::tbl(
    db,
    dplyr::sql(sprintf("SELECT normal_rand(%s, 0, 1) AS mc", n * nrows))
  )

  rand <- dplyr::mutate(rand, i = dplyr::row_number())

  df <- dplyr::full_join(df, seq_n, by = character())

  df <- dplyr::mutate(df, i = dplyr::row_number())

  df <- dplyr::inner_join(df, rand, by = "i")

  df <- dplyr::mutate(
    df,
    mc = pmax(
      .data[["mc"]] * .data[["sd"]] /
        .data[["mean"]] + log(.data[["mean"]]),
      log(minindex),
      na.rm = TRUE
    )
  )

  df <- dplyr::group_by(df, .data[["j"]], .data[["index"]])

  df <- window_arrange(df, .data[["time"]])

  df <- dplyr::mutate(df, mcf = dplyr::lead(.data[["mc"]]) - .data[["mc"]])

  df <- window_arrange(df, -.data[["time"]])

  df <- dplyr::mutate(df, mcb = dplyr::lead(.data[["mc"]]) - .data[["mc"]])

  df <- dplyr::mutate(
    df,
    mcf = pmin(.data[["mcf"]], log(trunc), na.rm = TRUE),
    mcb = pmin(.data[["mcb"]], log(trunc), na.rm = TRUE)
  )

  df <- dplyr::mutate(
    df,
    mcf = pmax(.data[["mcf"]], log(1 / trunc), na.rm = TRUE),
    mcb = pmax(.data[["mcb"]], log(1 / trunc), na.rm = TRUE)
  )

  df <- dplyr::group_by(df, .data[["j"]], .data[["time"]])

  df <- dplyr::summarise(
    df,
    mcf = mean(.data[["mcf"]], na.rm = TRUE),
    mcb = mean(.data[["mcb"]], na.rm = TRUE),
    .groups = "keep"
  )

  df <- dplyr::ungroup(df, .data[["time"]])

  df <- window_arrange(df, .data[["time"]])

  df <- dplyr::mutate(df, mcf = dplyr::lead(.data[["mcf"]], base - 1L))

  double_zero <- dplyr::sql("cast(0 AS DOUBLE PRECISION)")

  df <- dplyr::mutate(df, mcf = dplyr::lag(.data[["mcf"]], base, double_zero))

  df <- dplyr::mutate(df, mcf = cumsum(.data[["mcf"]]))

  df <- window_arrange(df, -.data[["time"]])

  df <- dplyr::mutate(df, mcb = dplyr::lead(.data[["mcb"]], nyears - base))

  df <- dplyr::mutate(
    df, mcb = dplyr::lag(.data[["mcb"]], esab, double_zero)
  )

  df <- dplyr::mutate(df, mcb = cumsum(.data[["mcb"]]))

  df <- dplyr::group_by(df, .data[["time"]])

  df <- dplyr::summarise(
    df,
    mean =
      exp(mean(.data[["mcf"]], na.rm = TRUE)) *
      exp(mean(.data[["mcb"]], na.rm = TRUE)),
    sd =
      sd(.data[["mcf"]], na.rm = TRUE) *
      exp(mean(.data[["mcf"]], na.rm = TRUE)) +
      sd(.data[["mcb"]], na.rm = TRUE) *
      exp(mean(.data[["mcb"]], na.rm = TRUE))
  )

  df <- dplyr::mutate(
    df,
    lower = .data[["mean"]] - .data[["sd"]],
    upper = .data[["mean"]] + .data[["sd"]]
  )

  df <- dplyr::arrange(df, .data[["time"]])

  index <- paste(index, model, sep = "_")

  message(
    sprintf("INFO [%s] Calculating %s combined index", Sys.time(), index)
  )

  df <- dplyr::collect(df)

  attr(df, "count_summary") <- list(taxa = length(taxa))

  cache_outputs(index, df, db)

  invisible(NULL)

}
