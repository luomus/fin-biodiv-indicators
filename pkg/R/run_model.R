run_model <- function(index, taxon, counts, model) {

  switch(
    model,
    trim = run_trim(index, taxon, counts)
  )

}

#' @importFrom config get
#' @importFrom rtrim count_summary index trim

run_trim <- function(index, taxon, counts) {

  message(
    sprintf(
      "INFO [%s] Calculating %s index for %s", Sys.time(), index,
      taxon[["code"]]
    )
  )

  args <- config::get("model", config = index)[["trim"]][["args"]]

  args[["object"]] <- counts
  args[["count_col"]] <- "abundance"
  args[["site_col"]] <- "location_id"

  if (is.null(args[["changepoints"]])) {

    args[["changepoints"]] <- "all"

  }

  trim <- do.call(rtrim::trim, args)

  base <- config::get("model", config = index)[["trim"]][["base_year"]]

  base <- which(trim[["time.id"]] == base)

  model_data <- rtrim::index(trim, base = base)

  names(model_data) <- c("time", "mean", "sd")

  attr(model_data, "count_summary") <- rtrim::count_summary(
    as.data.frame(counts), "abundance", "location_id"
  )[-2L]

  model_data

}
