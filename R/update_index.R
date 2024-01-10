#' Update index
#'
#' Update index output data.
#'
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param region Character. Which region?
#' @param db Connection. Database in which to update index.
#'
#' @importFrom config get
#' @export

update_index <- function(index, model, region, db) {

  index_base <- sub("_north|_south", "", index)

  from <- config::get("from", config = index_base)

  from <- paste(c(from, region), collapse = "_")

  df <- withCallingHandlers(
    tryCatch(
      switch(
        config::get("combine", config = index_base),
        cti = cti(from, index, model, db),
        geometric_mean = geometric_mean(index, model, db),
        overall_abundance = overall_abundance(from, index, model, db)
      ),
      error = err_msg
    ),
    warning = warn_msg
  )

  index_model <- paste(index, model, sep = "_")

  if (!inherits(df, "error")) {

    cache_outputs(index_model, df, db)

    set_cache(
      index_model, "model_state",
      data.frame(index = index_model, state = "success", time = Sys.time()), db
    )

  } else {

    set_cache(
      index_model, "model_state",
      data.frame(index = index_model, state = "fail", time = Sys.time()), db
    )

  }

  invisible(NULL)

}

#' @importFrom arm se.ranef
#' @importFrom config get
#' @importFrom dplyr .data collect copy_to group_by left_join summarise
#' @importFrom lme4 lmer
#' @importFrom pool poolCheckout poolReturn
#' @importFrom stats coef

cti <- function(index, cti, model, db) {

  con <- pool::poolCheckout(db)

  surveys <- get_from_db(con, "surveys", index)

  index_base <- sub("_north|_south", "", index)

  cti_base <- sub("_north|_south", "", cti)

  model_spec <- config::get("model", config = cti_base)[[model]]

  surveys <- apply_process(
    model_spec[["surveys_process"]],
    what = "surveys",
    surveys = surveys
  )

  taxa <- config::get("taxa", config = index_base)

  extra_taxa <- config::get("extra_taxa", config = index_base)

  codes <- vapply(taxa, getElement, "", "code")

  extra_codes <- vapply(extra_taxa, getElement, "", "code")

  codes <- c(codes, extra_codes)

  select <- config::get("counts", config = index_base)[["selection"]]

  abundance <- config::get("counts", config = index_base)[["abundance"]]

  select[select == abundance] <- "abundance"

  counts <- get_from_db(con, "counts", index_base, codes, c("index", select))

  counts <- apply_process(
    model_spec[["counts_process"]],
    what = "counts",
    counts = counts,
    surveys = surveys
  )

  sti <- lapply(taxa, getElement, "sti")

  sti[vapply(sti, is.null, NA)] <- NA_real_

  extra_sti <- lapply(extra_taxa, getElement, "sti")

  extra_sti[vapply(extra_sti, is.null, NA)] <- NA_real_

  sti <- unlist(c(sti, extra_sti))

  sti_df <- data.frame(index = paste(index_base, codes, sep = "_"), sti = sti)

  sti_df <- dplyr::copy_to(con, sti_df, overwrite = TRUE)

  data <- dplyr::left_join(counts, sti_df, by = "index")

  data <- dplyr::group_by(data, .data[["location_id"]], .data[["year"]])

  data <- dplyr::summarise(
    data,
    cti = sum(.data[["abundance"]] * .data[["sti"]], na.rm = TRUE) /
      sum(.data[["abundance"]], na.rm = TRUE),
    .groups = "drop"
  )

  message(
    sprintf(
      "INFO [%s] Calculating %s combined index",
      format(Sys.time()),
      paste(cti, model, sep = "_")
    )
  )

  data <- dplyr::collect(data)

  pool::poolReturn(con)

  mod <- lme4::lmer(cti ~ (1 | location_id) + (1 | year), data)

  df <- data.frame(stats::coef(mod)[["year"]], arm::se.coef(mod)[["year"]])

  df <- data.frame(
    time  = as.integer(rownames(df)),
    mean  = df[[1L]],
    sd    = df[[2L]],
    lower = df[[1L]] - df[[2L]],
    upper = df[[1L]] + df[[2L]]
  )

  attr(df, "count_summary") <- list(taxa = length(which(!is.na(sti))))

  df

}

#' @importFrom config get
#' @importFrom dplyr all_of arrange .data desc collect count distinct group_by
#' @importFrom dplyr filter full_join inner_join lag lead mutate pull right_join
#' @importFrom dplyr row_number summarise sql tbl ungroup

geometric_mean <- function(index, model, db) {

  index_base <- sub("_north|_south", "", index)

  n <- 1000L
  maxcv <- 3
  minindex <- .01
  trunc <- 10

  taxa <- config::get("taxa", config = index_base)

  taxa <- vapply(taxa, getElement, "", "code")

  df <- dplyr::tbl(db, "model_output")

  df <- dplyr::filter(
    df, .data[["index"]] %in% !!paste(index, model, taxa, sep = "_")
  )

  years <- sort(dplyr::pull(dplyr::distinct(df, .data[["time"]])))

  nyears <- length(years)

  ntaxa <- length(dplyr::pull(dplyr::distinct(df, .data[["index"]])))

  base <- config::get("model", config = index_base)[[model]][["base_year"]]

  base <- which(years == base)

  esab <- nyears - base + 1L

  nrows <- dplyr::pull(dplyr::count(df), dplyr::all_of("n"))

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
    df, sd = ifelse(.data[["mean"]] > minindex, .data[["sd"]], 0)
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

  df <- dplyr::mutate(
    df,
    mcf = dplyr::lead(.data[["mc"]], 1L, NA_real_) - .data[["mc"]]
  )

  df <- window_arrange(df, dplyr::desc(.data[["time"]]))

  df <- dplyr::mutate(
    df,
    mcb = dplyr::lead(.data[["mc"]], 1L, NA_real_) - .data[["mc"]]
  )

  df <- dplyr::mutate(
    df,
    mcf = ifelse(
      is.na(.data[["mcf"]]),
      NA_real_,
      pmin(.data[["mcf"]], log(trunc), na.rm = TRUE)
    ),
    mcb = ifelse(
      is.na(.data[["mcb"]]),
      NA_real_,
      pmin(.data[["mcb"]], log(trunc), na.rm = TRUE)
    )
  )

  df <- dplyr::mutate(
    df,
    mcf = ifelse(
      is.na(.data[["mcf"]]),
      NA_real_,
      pmax(.data[["mcf"]], log(1 / trunc), na.rm = TRUE)
    ),
    mcb = ifelse(
      is.na(.data[["mcb"]]),
      NA_real_,
      pmax(.data[["mcb"]], log(1 / trunc), na.rm = TRUE)
    )
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

  df <- window_arrange(df, dplyr::desc(.data[["time"]]))

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
      format(Sys.time()),
      paste(index, model, sep = "_")
    )
  )

  df <- dplyr::collect(df)

  attr(df, "count_summary") <- list(taxa = ntaxa)

  df

}

#' @importFrom config get
#' @importFrom dplyr .data collect group_by summarise

overall_abundance <- function(index, oa, model, db) {

  surveys <- get_from_db(db, "surveys", index)

  index_base <- sub("_north|_south", "", index)

  oa_base <- sub("_north|_south", "", oa)

  model_spec <- config::get("model", config = oa_base)[[model]]

  surveys <- apply_process(
    model_spec[["surveys_process"]],
    what = "surveys",
    surveys = surveys
  )

  taxa <- config::get("taxa", config = index_base)

  extra_taxa <- config::get("extra_taxa", config = index_base)

  codes <- vapply(taxa, getElement, "", "code")

  extra_codes <- vapply(extra_taxa, getElement, "", "code")

  codes <- c(codes, extra_codes)

  select <- config::get("counts", config = index_base)[["selection"]]

  counts <- get_from_db(db, "counts", index_base, codes, c("index", select))

  counts <- apply_process(
    model_spec[["counts_process"]],
    what = "counts",
    counts = counts,
    surveys = surveys
  )

  data <- dplyr::group_by(counts, .data[["location_id"]], .data[["year"]])

  data <- dplyr::summarise(
    data,
    abundance = sum(.data[["abundance"]], na.rm = TRUE),
    .groups = "drop"
  )

  message(
    sprintf(
      "INFO [%s] Calculating %s combined index",
      format(Sys.time()),
      paste(oa, model, sep = "_")
    )
  )

  data <- dplyr::collect(data)

  df <- run_trim(oa_base, data)

  attr(df, "count_summary") <- c(
    attr(df, "count_summary"), taxa = length(codes)
  )

  df

}
