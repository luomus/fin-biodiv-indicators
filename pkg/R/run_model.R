run_model <- function(index, taxon, surveys, counts, model) {

  switch(
    model,
    trim = run_trim(index, taxon, counts)
  )

}

#' @importFrom config get
#' @importFrom rtrim count_summary index trim
#' @importFrom dplyr collect .data select

run_trim <- function(index, taxon, counts) {

  message(
    sprintf(
      "INFO [%s] Calculating %s index for %s", Sys.time(), index,
      taxon[["code"]]
    )
  )

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
