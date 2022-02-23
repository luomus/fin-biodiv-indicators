#' Update index
#'
#' Update index output data.
#'
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param db Connection. Database in which to update index.
#'
#' @importFrom config get
#' @export

update_index <- function(index, model, db) {

  taxa <- config::get("taxa", config = index)

  from <- config::get("from", config = index)

  df <- switch(
    config::get("combine", config = index),
    cti = cti(from, index, model, taxa, db),
    geometric_mean = geometric_mean(index, model, taxa, db)
  )

  attr(df, "count_summary") <- list(taxa = length(taxa))

  cache_outputs(paste(index, model, sep = "_"), df, db)

  invisible(NULL)

}

#' @importFrom arm se.ranef
#' @importFrom config get
#' @importFrom dplyr .data collect copy_to group_by left_join summarise
#' @importFrom lme4 lmer
#' @importFrom pool poolCheckout poolReturn
#' @importFrom stats coef

cti <- function(index, cti, model, taxa, db) {

  con <- pool::poolCheckout(db)

  surveys <- get_from_db(con, "surveys", index)

  model_spec <- config::get("model", config = cti)[[model]]

  for (i in model_spec[["surveys_process"]]) {

    surveys <- do.call(process_funs()[[i]], list(surveys))

  }

  codes <- vapply(config::get("taxa", config = index), getElement, "", "code")

  select <- config::get("counts", config = index)[["selection"]]

  counts <- get_from_db(con, "counts", index, codes, c("index", select))

  for (i in model_spec[["counts_process"]]) {

    counts <- do.call(
      process_funs()[[i]], list(counts = counts, surveys = surveys)
    )

  }

  sti <- vapply(config::get("taxa", config = index), getElement, 0, "sti")

  sti <- data.frame(index = paste(index, codes, sep = "_"), sti = sti)

  sti <- dplyr::copy_to(con, sti)

  data <- dplyr::left_join(counts, sti, by = "index")

  data <- dplyr::group_by(data, .data[["location_id"]], .data[["year"]])

  data <- dplyr::summarise(
    data,
    cti = sum(.data[["abundance"]] * .data[["sti"]], na.rm = TRUE) /
      sum(.data[["abundance"]], na.rm = TRUE),
    .groups = "drop"
  )

  data <- dplyr::collect(data)

  pool::poolReturn(con)

  mod <- lme4::lmer(cti ~ (1 | location_id) + (1 | year), data)

  df <- data.frame(stats::coef(mod)[["year"]], arm::se.coef(mod)[["year"]])

  data.frame(
    time  = as.integer(rownames(df)),
    mean  = df[[1L]],
    sd    = df[[2L]],
    lower = df[[1L]] - df[[2L]],
    upper = df[[1L]] + df[[2L]]
  )

}

#' @importFrom config get
#' @importFrom dplyr arrange .data collect count distinct group_by filter
#' @importFrom dplyr full_join inner_join lag lead mutate pull right_join
#' @importFrom dplyr row_number summarise sql tbl ungroup

geometric_mean <- function(index, model, taxa, db) {

  n <- 1000L
  maxcv <- 3
  minindex <- .01
  trunc <- 10

  taxa <- vapply(taxa, getElement, "", "code")

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
    df, mean = pmax(.data[["mean"]], minindex, na.rm = TRUE)
  )

  df <- dplyr::mutate(
    df, se_imp = ifelse(.data[["mean"]] > minindex, .data[["sd"]], 0)
  )

  seq_n <- dplyr::tbl(
    db, dplyr::sql(sprintf("SELECT generate_series(1, %s) AS j", n))
  )

  rand <- dplyr::tbl(
    db, dplyr::sql(sprintf("SELECT normal_rand(%s, 0, 1) AS mc", n * nrows))
  )

  rand <- dplyr::mutate(rand, i = dplyr::row_number())

  df <- dplyr::full_join(df, seq_n, by = character())

  df <- dplyr::mutate(df, i = dplyr::row_number())

  df <- dplyr::inner_join(df, rand, by = "i")

  df <- dplyr::mutate(
    df,
    mc = pmax(
      .data[["mc"]] * .data[["sd"]] / .data[["mean"]] + log(.data[["mean"]]),
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

  df <- dplyr::mutate(df, mcb = dplyr::lag(.data[["mcb"]], esab, double_zero))

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

  message(
    sprintf(
      "INFO [%s] Calculating %s combined index",
      Sys.time(),
      paste(index, model, sep = "_")
    )
  )

  dplyr::collect(df)

}
