res <- tryCatch(

  {

    pkgs <- c("stats", "dplyr", "fbi", "pool", "RPostgres", "tictoc")

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

    timeout_in_secs <- as.integer(Sys.getenv("TIMEOUT_IN_HOURS")) * 60L * 60L

    start_timer <- tictoc::tic()

    message(sprintf("INFO [%s] Update starting...", Sys.time()))

    Sys.setenv(R_CONFIG_FILE = "config.yml")

    indices <- vapply(get_indices(), getElement, "", "code")

    for (index in sample(indices)) {

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

      for (taxon in sample(c(taxa, extra_taxa))) {

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

        do_upd <- do_update(index, "output")

        do_upd <- do_upd || do_update(taxon[["code"]], "output")

        taxon_index_update <- surveys || counts || do_upd

        if (taxon_index_update) {

          for (model in models) {

            for (i in paste0(index, c("", "_north", "_south"))) {

              message(
                sprintf(
                  "INFO [%s] Updating %s model for %s (%s index)...",
                  Sys.time(),
                  model,
                  taxon[["binomial"]],
                  i
                )
              )

              update_taxon_index(i, model, taxon, pool)

            }

          }

        }

        index_update <- index_update || taxon_index_update

        stop_timer <- tictoc::toc(quiet = TRUE)

        tictoc::tic()

        if (stop_timer[["toc"]] - start_timer > timeout_in_secs) {

          message(
            sprintf(
              "INFO [%s] Reached time limit. Taxon update exiting", Sys.time()
            )
          )

          break

        }

      }

      if (index_update) {

        for (model in models) {

          for (i in list(NULL, "north", "south")) {

            message(
              sprintf(
                "INFO [%s] Updating combined %s model for %s index...",
                Sys.time(),
                model,
                paste0(index, i)
              )
            )

            needs_update <- TRUE

            if (!is.null(src)) {

              last_mod <- dplyr::tbl(pool, "output_cache_time")

              src_mdl <- names(config::get("model", config = src))[[1L]]

              last_mod_src <- dplyr::filter(
                last_mod,
                .data[["index"]] == !!paste(c(src, i, src_mdl), collapse = "_")
              )

              last_mod_src <- dplyr::pull(last_mod_src, .data[["time"]])

              last_mod_index <- dplyr::filter(
                last_mod,
                .data[["index"]] == !!paste(index, i, model, collapse = "_")
              )

              last_mod_index <- dplyr::pull(last_mod_index, .data[["time"]])

              needs_update <-
                !isFALSE(last_mod_src > last_mod_index) ||
                  do_update(paste0(index, i), "output")

            }

            if (needs_update) {

              update_index(paste(c(index, i), collapse = "_"), model, i, pool)

            }

          }

          stop_timer <- tictoc::toc(quiet = TRUE)

          tictoc::tic()

          if (stop_timer[["toc"]] - start_timer > timeout_in_secs) {

            message(
              sprintf(
                "INFO [%s] Reached time limit. Index update exiting", Sys.time()
              )
            )

            break

          }

        }

      }

      stop_timer <- tictoc::toc(quiet = TRUE)

      tictoc::tic()

      if (stop_timer[["toc"]] - start_timer > timeout_in_secs) {

        message(
          sprintf("INFO [%s] Reached time limit. Job exiting", Sys.time())
        )

        break

      }

    }

    pool::poolClose(pool)

    message(sprintf("INFO [%s] Update complete", Sys.time()))

    "true"

  },
  error = function(e) {

    message(sprintf("ERROR [%s] %s", Sys.time(), e[["message"]]))

    "false"

  }

)

httr::GET(
  paste0(Sys.getenv("APP_HOSTNAME"), ":", Sys.getenv("APP_PORT")),
  path = "job",
  query = list(status = res, secret = Sys.getenv("JOB_SECRET"))
)
