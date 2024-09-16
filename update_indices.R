dir.create("var/logs", showWarnings = FALSE)

log_file_name <- sprintf("var/logs/update-%s.txt", format(Sys.time(), "%F-%H"))

log_file <- file(log_file_name, open = "wt")

sink(log_file, split = TRUE)

sink(log_file, type = "message")

res <- withCallingHandlers(
  tryCatch(

    {

      source("run_update.R")

      "true"

    },
    error = function(e) {

      writeLines(sprintf("ERROR [%s] %s", format(Sys.time()), e[["message"]]))

      "false"

    }

  ),
  warning = function(w) {

    writeLines(
      sprintf(
        "WARN [%s] %s",
        format(Sys.time()),
        gsub("\n|\r|\r\n", "; ", w[["message"]])
      )
    )

    tryInvokeRestart("muffleWarning")

  },
  messsage = function(m) {

    writeLines(
      sprintf(
        "WARN [%s] %s",
        format(Sys.time()),
        gsub("\n|\r|\r\n", "; ", m[["message"]])
      )
    )

    tryInvokeRestart("muffleMessage")

  }

)

sink(type = "message")

sink()

close(log_file)

httr::POST(
  paste0("http://", Sys.getenv("APP_HOSTNAME"), ":", Sys.getenv("APP_PORT")),
  path = "job-logs",
  query = list(secret = Sys.getenv("JOB_SECRET")),
  body = list(file = httr::upload_file(log_file_name))
)

httr::GET(
  paste0("http://", Sys.getenv("APP_HOSTNAME"), ":", Sys.getenv("APP_PORT")),
  path = "job",
  query = list(status = res, secret = Sys.getenv("JOB_SECRET"))
)
