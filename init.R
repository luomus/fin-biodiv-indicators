pkgs <- c(
  "dplyr", "fbi", "logger", "plumber", "pool", "rapidoc", "RPostgres", "tictoc"
)

for (pkg in pkgs) {

  suppressPackageStartupMessages(
    library(pkg, quietly = TRUE, character.only = TRUE)
  )

}

Sys.setenv(R_CONFIG_FILE = "var/config.yml")

if (!dir.exists("var/logs")) {

  log_dir <- dir.create("var/logs", recursive = TRUE)

  stopifnot("Log dir creation failed" = log_dir)

}

if (!dir.exists("var/status")) dir.create("var/status")

log_dir <- "var/logs"

log_file <- tempfile("plumber_", log_dir, ".log")

log_appender(appender_tee(log_file))

convert_empty <- function(x) switch(paste0(".", x), . = "-", x)

options(
  finbif_use_cache = FALSE,
  finbif_api_url = Sys.getenv("FINBIF_API"),
  finbif_warehouse_query = Sys.getenv("FINBIF_WAREHOUSE_QUERY"),
  finbif_email = Sys.getenv("FINBIF_EMAIL")
)

pool <- dbPool(Postgres())

tryCatch(
  {
    if (!"tablefunc" %in% pull(tbl(pool, "pg_extension"), extname)) {

      dbExecute(pool, "CREATE EXTENSION tablefunc")

    }
  },
  error = function(e) message(e[["message"]])
)

p <- plumb("api.R")

p$registerHooks(
  list(
    preroute = function() tic(),
    postroute = function(req, res) {

      end <- tictoc::toc(quiet = TRUE)

      log_fn <- log_info

      if (res$status >= 400L) log_fn <- log_error

      if (identical(req[["PATH_INFO"]], "/healthz")) log_fn <- \(.) {}

      if (identical(req[["HTTP_USER_AGENT"]], "Zabbix")) log_fn <- \(.) {}

      log_fn(
        paste0(
          '{convert_empty(req$REMOTE_ADDR)} ',
          '"{convert_empty(req$HTTP_USER_AGENT)}" ',
          '{convert_empty(req$HTTP_HOST)} ',
          '{convert_empty(req$REQUEST_METHOD)} ',
          '{convert_empty(req$PATH_INFO)} ',
          '{convert_empty(res$status)} ',
          '{round(end$toc - end$tic, digits = getOption("digits", 5L))}'
        )
      )

    }
  )
)

p$run(host = "0.0.0.0", port = 8000L)
