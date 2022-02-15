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

do_update <- function(index, type = c("input", "output")) {

  index <- sub("\\.", "", index)

  type <- match.arg(type)

  envvar <- paste(index, type, sep = "_")

  ans <- Sys.getenv(envvar, FALSE)

  ans <- as.logical(ans)

  isTRUE(ans)

}

message(sprintf("INFO [%s] Update starting...", Sys.time()))

for (index in config::get("indices")) {

  index_update <- FALSE

  do_upd <- do_update(index)

  surveys <- update_data("surveys", index, NULL, pool, do_upd)

  taxa <- config::get("taxa", config = index)

  extra_taxa <- config::get("extra_taxa", config = index)

  models <- names(config::get("model", config = index))

  for (taxon in c(taxa, extra_taxa)) {

    do_upd <- do_upd || do_update(taxon[["code"]])

    counts <- update_data("counts", index, taxon, pool, do_upd)

    do_upd <- do_update(index, "output")

    do_upd <- do_upd || do_update(taxon[["code"]], "output")

    taxon_index_update <- surveys || counts || do_upd

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
