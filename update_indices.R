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

  for (taxon in config::get("taxa", config = index)) {

    counts <- update_data("counts", index, taxon, pool)

    taxon_index_update <- surveys || counts

    if (taxon_index_update) {

      update_taxon_index(index, taxon, pool)

    }

    index_update <- index_update || taxon_index_update

  }

  if (index_update) {

    update_index(index, pool)

  }

}

pool::poolClose(pool)
