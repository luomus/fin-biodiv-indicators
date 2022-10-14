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
#' @importFrom rtrim count_summary index overall trim
#' @importFrom dplyr all_of collect .data select

run_trim <- function(index, taxon, counts) {

  args <- config::get("model", config = index)[["trim"]][["args"]]

  counts <- dplyr::select(
    counts, dplyr::all_of(c("abundance", "location_id", "year"))
  )

  args[["object"]] <- dplyr::collect(counts)

  stopifnot("No count data available" = nrow(args[["object"]]) > 0L)

  args[["object"]][["abundance"]] <- as.integer(args[["object"]][["abundance"]])

  args[["count_col"]] <- "abundance"

  args[["site_col"]] <- "location_id"

  if (is.null(args[["changepoints"]])) {

    args[["changepoints"]] <- "all"

  }

  trim <- do.call(rtrim::trim, args)

  base <- config::get("model", config = index)[["trim"]][["base_year"]]

  base <- which(trim[["time.id"]] == base)

  base <- max(base, 1L)

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

  attr(model_data, "trends") <- as.list(rtrim::overall(trim)[["slope"]])

  model_data

}

#' @importFrom config get

run_rbms <- function(index, taxon, surveys, counts) {

  args <- config::get("model", config = index)[["rbms"]][["args"]]

  args[["surveys"]] <- surveys

  args[["counts"]] <- counts

  args[["byr"]] <- config::get("model", config = index)[["rbms"]][["base_year"]]

  do.call(rbms, args)

}

#' @importFrom dplyr all_of arrange collect .data filter group_by lag lead
#' @importFrom dplyr mutate select summarise
#' @importFrom rbms boot_sample collated_index flight_curve impute_count
#' @importFrom rbms site_index ts_dwmy_table ts_monit_count_site ts_monit_season
#' @importFrom rbms ts_monit_site
#' @importFrom rtrim count_summary
#' @importFrom stats sd

rbms <- function(
  counts,
  surveys,
  byr,
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

  surveys <- dplyr::select(
    surveys,
    site_id = dplyr::all_of("location_id"), dplyr::all_of(c("year", "date"))
  )

  surveys <- dplyr::collect(surveys)

  base <- which(sort(unique(surveys[["year"]])) == byr)

  base <- max(base, 1L)

  init_year <- min(surveys[["year"]])

  last_year <- max(surveys[["year"]])

  nyears <- last_year - init_year + 1L

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
    count = dplyr::all_of("abundance"),
    site_id = dplyr::all_of("location_id"),
    dplyr::all_of(c("year", "date", "species"))
  )

  counts <- dplyr::collect(counts)

  ts_season_count <- rbms::ts_monit_count_site(
    m_season_visit = ts_season_visit, m_count = counts
  )

  stopifnot("No count data available" = nrow(ts_season_count) > 0L)

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
    MultiVisit      = MultiVisit,
    verbose         = FALSE
  )

  impt_counts <- rbms::impute_count(
    ts_season_count = ts_season_count,
    ts_flight_curve = ts_flight_curve[["pheno"]],
    TimeUnit        = FlightCurveTimeUnit,
    MultiVisit      = MultiVisit
  )

  count_summary <- rtrim::count_summary(
    impt_counts, "IMPUTED_COUNT", "SITE_ID", "YEAR"
  )[-2L]

  sindex <- rbms::site_index(
    butterfly_count = impt_counts, MinFC = MinFC
  )

  bootsample <- rbms::boot_sample(data = sindex, boot_n = boot_n)

  index_mc <- suppressWarnings(
    mapply(
      rbms::collated_index,
      bootID = seq_len(boot_n),
      MoreArgs = list(data = sindex, s_sp = 1L, boot_ind = bootsample),
      SIMPLIFY = FALSE
    )
  )

  index_mc <- lapply(index_mc, getElement, "col_index")

  index_mc <- do.call(rbind, index_mc)

  index_mc <- dplyr::mutate(
    index_mc,
    mc = log(pmax(1 / 100, .data[["COL_INDEX"]])),
    time = .data[["M_YEAR"]]
  )

  index_mc <- dplyr::group_by(index_mc, .data[["BOOTi"]])

  index_mc <- dplyr::arrange(index_mc, .data[["time"]])

  index_mc <- dplyr::mutate(
    index_mc, mcf = dplyr::lead(.data[["mc"]]) - .data[["mc"]]
  )

  index_mc <- dplyr::arrange(index_mc, -.data[["time"]])

  index_mc <- dplyr::mutate(
    index_mc, mcb = dplyr::lead(.data[["mc"]]) - .data[["mc"]]
  )

  index_mc <- dplyr::arrange(index_mc, .data[["time"]])

  index_mc <- dplyr::mutate(
    index_mc,
    mcf = dplyr::lead(.data[["mcf"]], base - 1L)
  )

  index_mc <- dplyr::mutate(
    index_mc,
    mcf = dplyr::lag(.data[["mcf"]], base, 0)
  )

  index_mc <- dplyr::mutate(
    index_mc,
    mcf = cumsum(.data[["mcf"]])
  )

  index_mc <- dplyr::arrange(index_mc, -.data[["time"]])

  index_mc <- dplyr::mutate(
    index_mc,
    mcb = dplyr::lead(.data[["mcb"]], nyears - base)
  )

  index_mc <- dplyr::mutate(
    index_mc,
    mcb = dplyr::lag(.data[["mcb"]], nyears - base + 1L, 0)
  )

  index_mc <- dplyr::mutate(
    index_mc,
    mcb = cumsum(.data[["mcb"]])
  )

  index_mc <- dplyr::group_by(index_mc, .data[["time"]])

  index <- dplyr::summarise(
    index_mc,
    mean =
      exp(mean(.data[["mcf"]])) *
      exp(mean(.data[["mcb"]])),
    sd =
      stats::sd(.data[["mcf"]]) * exp(mean(.data[["mcf"]])) +
      stats::sd(.data[["mcb"]]) * exp(mean(.data[["mcb"]]))
  )

  index <- dplyr::mutate(
    index,
    lower = .data[["mean"]] - .data[["sd"]],
    upper = .data[["mean"]] + .data[["sd"]]
  )

  attr(index, "count_summary") <- count_summary

  index

}
