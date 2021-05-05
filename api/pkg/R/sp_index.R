#' Population indicator
#'
#' Create a population indicator.
#'
#' @param sp Species.
#' @param year Year.
#' @param base Base year of index.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom promises then
#'
#' @export

sp_index <- function(sp, year, base, id) {

  if (missing(year)) year <- NULL
  if (missing(base)) base <- NULL

  data <- get_sp_data(sp, "winter_birds", id)

  promises::then(
    data, ~{

      datayear <- sort(unique(.[["year"]]))

      if (is.null(year)) {

        year <- datayear

      } else {

        year <- parse_int_range(year)

      }

      year <- as.character(year)

      if (is.null(base) || !base %in% datayear) {

        base <- 1L

      } else {

        base <- which(datayear == as.character(base))

      }

      hash <- digest::digest(list(sp, year, base))

      log_message(id, "Checking output cache for index data: ", hash)

      cached_data <- get_from_output_cache(hash)

      if (is_output_cached(cached_data)) {

        log_message(id, "Getting index data from output cache")

        unserialize(unlist(cached_data[["data"]]))

      } else {

        calc_sp_index(sp, year, base, id)

      }

    }

  )

}

#' @importFrom promises future_promise then
#' @importFrom digest digest
#' @importFrom rtrim index trim

calc_sp_index <- function(sp, year, base, id) {

  force(sp)
  force(year)
  force(base)

  hash <- digest::digest(list(sp, year, base))

  log_message(id, "Getting ", sp, " winter bird data")

  data <- get_sp_data(sp, "winter_birds", id)

  log_message(id, "Calculating index for ", sp, ": ", hash)

  promises::then(
    data,
    ~{
      df <- .
      op <- options()
      promises::future_promise({
        options(op)
        ans <- rtrim::index(
          rtrim::trim(
            count ~ site + year, data = df, model = 2, changepoints = "all"
          ),
          base = base
        )
        rownames(ans) <- ans[["time"]]
        names(ans) <- c("year", "index", "sd")
        ans <- ans[year, ]
        ans[["year"]] <- as.integer(year)
        rownames(ans) <- NULL

        set_output_cache(hash, serialize(ans, NULL))

        ans},
        globals = c("base", "df", "year", "hash", "op"),
        packages = c("rtrim", "indicators"),
        seed = TRUE
      )
    }
  )
}
