#' Population indicator
#'
#' Create a population indicator.
#'
#' @param index Which index.
#' @param sp Species.
#' @param use_cache Whether to use cached data.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom promises promise_resolve
#'
#' @export

sp_index <- function(index, sp, use_cache, id) {

  hash <- digest::digest(list(index, sp))

  log_message(id, "Checking output cache for index data: ", hash)

  cached_data <- get_from_output_cache(hash)

  if (output_cache_valid(cached_data, use_cache, index)) {

    log_message(id, "Getting index data from output cache")

    promises::promise_resolve(unserialize(unlist(cached_data[["data"]])))

  } else {

    calc_sp_index(index, sp, use_cache, id)

  }

}

#' @importFrom promises future_promise then
#' @importFrom digest digest
#' @importFrom rtrim index trim

calc_sp_index <- function(index, sp, use_cache, id) {

  force(sp)

  hash <- digest::digest(list(index, sp))

  log_message(id, "Getting ", sp, " ", index, " data")

  data <- get_sp_data(index, sp, use_cache, id)

  log_message(id, "Calculating index for ", sp, " from ", index, ": ", hash)

  promises::then(
    data,
    ~{
      df <- .
      op <- options()
      promises::future_promise({
        options(op)
        ans <- rtrim::index(
          rtrim::trim(
            abundance ~ location_id + year, data = df, model = 2,
            changepoints = "all"
          )
        )
        names(ans) <- c("year", "index", "sd")

        set_output_cache(hash, serialize(ans, NULL))

        ans
        },
        globals = c("df", "hash", "op"),
        packages = c("rtrim", "indicators"),
        seed = TRUE
      )
    }
  )
}
