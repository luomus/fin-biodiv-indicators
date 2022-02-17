run_model <- function(index, taxon, surveys, counts, model) {

  message(
    sprintf(
      "INFO [%s] Calculating %s %s index for %s", Sys.time(), index, model,
      taxon[["code"]]
    )
  )

  switch(
    model,
    trim = run_trim(index, taxon, counts),
    rbms = run_rbms(index, taxon, surveys, counts)
  )

}

#' @importFrom config get
#' @importFrom rtrim count_summary index trim
#' @importFrom dplyr collect .data select

run_trim <- function(index, taxon, counts) {

  args <- config::get("model", config = index)[["trim"]][["args"]]

  counts <- dplyr::select(
    counts, .data[["abundance"]], .data[["location_id"]], .data[["year"]]
  )

  args[["object"]] <- dplyr::collect(counts)
  args[["count_col"]] <- "abundance"
  args[["site_col"]] <- "location_id"

  if (is.null(args[["changepoints"]])) {

    args[["changepoints"]] <- "all"

  }

  trim <- do.call(rtrim::trim, args)

  base <- config::get("model", config = index)[["trim"]][["base_year"]]

  base <- which(trim[["time.id"]] == base)

  model_data <- rtrim::index(trim, base = base)

  model_data <- data.frame(
    time = model_data[["time"]],
    mean = model_data[["imputed"]],
    sd = model_data[["se_imp"]],
    lower = model_data[["imputed"]] - model_data[["se_imp"]],
    upper = model_data[["imputed"]] + model_data[["se_imp"]]
  )

  attr(model_data, "count_summary") <- rtrim::count_summary(
    as.data.frame(counts), "abundance", "location_id"
  )[-2L]

  model_data

}

#' @importFrom config get
#' @importFrom rtrim count_summary

run_rbms <- function(index, taxon, surveys, counts) {

  args <- config::get("model", config = index)[["rbms"]][["args"]]

  args[["surveys"]] <- surveys

  args[["counts"]] <- counts

  args[["byr"]] <- config::get("model", config = index)[["rbms"]][["base_year"]]

  model_data <- do.call(rbms, args)

  attr(model_data, "count_summary") <- rtrim::count_summary(
    as.data.frame(counts), "abundance", "location_id"
  )[-2L]

  model_data

}

#' @importFrom dplyr collect .data filter mutate select
#' @importFrom rbms boot_sample collated_index flight_curve impute_count
#' @importFrom rbms site_index ts_dwmy_table ts_monit_count_site ts_monit_season
#' @importFrom rbms ts_monit_site
#' @importFrom stats quantile

rbms <- function(
  counts,
  surveys,
  byr,
  lower_ci = .159,
  upper_ci = .841,
  StartMonth = 4,
  EndMonth = 9,
  StartDay = 1,
  EndDay = 30,
  Anchor = TRUE,
  AnchorLength = 14,
  AnchorLag = 14,
  AnchorTimeUnit = "d",
  NbrSample = 500,
  MinVisit = 5,
  MinOccur = 2,
  MinNbrSite = 5,
  MaxTrial = 4,
  GamFamily = "nb",
  FlightCurveTimeUnit = "w",
  MultiVisit = "mean",
  MinFC = 0.10,
  boot_n = 200
) {

  surveys <- dplyr::filter(
    surveys,
    !is.na(.data[["location_id"]]) &
    !is.na(.data[["year"]]) &
    !is.na(.data[["month"]]) &
    !is.na(.data[["day"]])
  )

  surveys <- dplyr::select(
    surveys, site_id = .data[["location_id"]], .data[["year"]], .data[["date"]]
  )

  surveys <- dplyr::collect(surveys)

  base <- which(sort(unique(surveys[["year"]])) == byr)

  init_year <- min(surveys[["year"]])

  last_year <- max(surveys[["year"]])

  ts_date <- rbms::ts_dwmy_table(InitYear = init_year, LastYear = last_year)

  ts_season <- rbms::ts_monit_season(
    d_series     = ts_date,
    StartMonth   = StartMonth,
    EndMonth     = EndMonth,
    StartDay     = StartDay,
    EndDay       = EndDay,
    Anchor       = Anchor,
    AnchorLength = AnchorLength,
    AnchorLag    = AnchorLag,
    TimeUnit     = AnchorTimeUnit
  )

  ts_season_visit <- rbms::ts_monit_site(
    m_visit = surveys, ts_season = ts_season
  )

  counts <- dplyr::mutate(counts, species = 1)

  counts <- dplyr::select(
    counts,
    count = .data[["abundance"]],
    site_id = .data[["location_id"]],
    .data[["year"]],
    .data[["date"]],
    .data[["species"]]
  )

  counts <- dplyr::collect(counts)

  ts_season_count <- rbms::ts_monit_count_site(
    m_season_visit = ts_season_visit, m_count = counts
  )

  ts_flight_curve <- rbms::flight_curve(
    ts_season_count = ts_season_count,
    NbrSample       = NbrSample,
    MinVisit        = MinVisit,
    MinOccur        = MinOccur,
    MinNbrSite      = MinNbrSite,
    MaxTrial        = MaxTrial,
    GamFamily       = GamFamily,
    SpeedGam        = FALSE,
    TimeUnit        = FlightCurveTimeUnit,
    MultiVisit      = MultiVisit
  )

  impt_counts <- rbms::impute_count(
    ts_season_count = ts_season_count,
    ts_flight_curve = ts_flight_curve[["pheno"]],
    TimeUnit        = FlightCurveTimeUnit,
    MultiVisit      = MultiVisit
  )

  sindex <- rbms::site_index(
    butterfly_count = impt_counts, MinFC = MinFC
  )

  index_mean <- rbms::collated_index(data = sindex, s_sp = 1L)

  index_mean <- index_mean[["col_index"]][["COL_INDEX"]]

  ref <- index_mean[[base]]

  index_mean_scaled <- index_mean / ref

  bootsample <- rbms::boot_sample(data = sindex, boot_n = boot_n)

  index_b <- mapply(
    rbms::collated_index,
    bootID = seq_len(boot_n),
    MoreArgs = list(data = sindex, s_sp = 1L, boot_ind = bootsample),
    SIMPLIFY = FALSE
  )

  index_b <- lapply(index_b, getElement, "col_index")

  index_b <- do.call(rbind, index_b)

  index_interval <- tapply(
    index_b[["COL_INDEX"]] / ref, index_b[["M_YEAR"]], stats::quantile,
    c(lower_ci, upper_ci)
  )

  index_interval <- do.call(rbind, index_interval)

  data.frame(
    time  = seq.int(init_year, last_year),
    mean  = index_mean_scaled,
    sd    = (index_interval[, 2L] - index_interval[, 1L]) / 2,
    lower = index_interval[, 1L],
    upper = index_interval[, 2L]
  )

}
