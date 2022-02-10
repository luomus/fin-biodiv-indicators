pkgs <- c(
  "config", "indicators", "pool", "RPostgres"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

options(
  finbif_use_cache = FALSE,
  finbif_api_url = Sys.getenv("FINBIF_API"),
  finbif_warehouse_query = Sys.getenv("FINBIF_WAREHOUSE_QUERY"),
  finbif_email = Sys.getenv("FINBIF_EMAIL")
)

pool <- pool::dbPool(RPostgres::Postgres())

for (index in config::get("indices")) {

  index_update <- FALSE

  surveys <- update_data("surveys", index, NULL, pool)

  taxa <- config::get("taxa", config = index)

  models <- names(config::get("model", config = index))

  extra_taxa <- config::get("extra_taxa", config = index)

  for (taxon in c(taxa, extra_taxa)) {

    counts <- update_data("counts", index, taxon, pool)

    do_update <- isTRUE(
      as.logical(Sys.getenv(sub("\\.", "", taxon[["code"]]), FALSE))
    )

    do_update <- do_update || isTRUE(as.logical(Sys.getenv(index, FALSE)))

    taxon_index_update <- surveys || counts || do_update

    if (taxon_index_update) {

      for (model in models) {

        update_taxon_index(index, model, taxon, pool)

      }

    }

    index_update <- index_update || taxon_index_update

  }

  if (index_update) {

    for (model in models) {

      update_index(index, model, pool)

    }

  }

}

pool::poolClose(pool)
