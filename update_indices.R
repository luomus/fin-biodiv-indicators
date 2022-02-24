pkgs <- c("dplyr", "fbi", "pool", "RPostgres")

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

indices <- vapply(config::get("indices"), getElement, "", "code")

for (index in indices) {

  message(sprintf("INFO [%s] Updating %s index...", Sys.time(), index))

  index_update <- TRUE

  do_upd <- do_update(index)

  src <- config::get("from", config = index)

  if (is.null(src)) {

    surveys <- update_data("surveys", index, NULL, pool, do_upd)

    index_update <- FALSE

  }

  taxa <- config::get("taxa", config = index)

  extra_taxa <- config::get("extra_taxa", config = index)

  models <- names(config::get("model", config = index))

  for (taxon in c(taxa, extra_taxa)) {

    message(
      sprintf(
        "INFO [%s] Updating %s for %s index...",
        Sys.time(),
        taxon[["binomial"]],
        index
      )
    )

    do_upd <- do_update(index) || do_update(taxon[["code"]])

    counts <- update_data("counts", index, taxon, pool, do_upd)

    do_upd <- do_update(index, "output") || do_update(taxon[["code"]], "output")

    taxon_index_update <- surveys || counts || do_upd

    if (taxon_index_update) {

      for (model in models) {

        message(
          sprintf(
            "INFO [%s] Updating %s model for %s (%s index)...",
            Sys.time(),
            model,
            taxon[["binomial"]],
            index
          )
        )

        update_taxon_index(index, model, taxon, pool)

      }

    }

    index_update <- index_update || taxon_index_update

  }

  if (index_update) {

    for (model in models) {

      message(
        sprintf(
          "INFO [%s] Updating combined %s model for %s index...", Sys.time(),
          model, index
        )
      )

      needs_update <- TRUE

      if (!identical(src, index)) {

        last_mod <- dplyr::tbl(pool, "output_cache_time")

        src_model <- names(config::get("model", config = src))[[1L]]

        last_mod_src <- dplyr::filter(
          last_mod, .data[["index"]] == !!paste(src, src_model, sep = "_")
        )

        last_mod_src <- dplyr::pull(last_mod_src, .data[["time"]])

        last_mod_index <- dplyr::filter(
          last_mod, .data[["index"]] == !!paste(index, model, sep = "_")
        )

        last_mod_index <- dplyr::pull(last_mod_index, .data[["time"]])

        needs_update <- !isFALSE(last_mod_src > last_mod_index)

      }

      if (needs_update) {

        update_index(index, model, pool)

      }

    }

  }

}

pool::poolClose(pool)

message(sprintf("INFO [%s] Update complete", Sys.time()))
