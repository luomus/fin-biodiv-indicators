#' Population indicator
#'
#' Create a population indicator.
#'
#' @param sp Species
#' @param year Year
#' @param base Base year of index
#' @importFrom digest digest
#' @export

sp_index <- function(sp = "skylark", year, base) {

  sp <- match.arg(sp)

  data <- switch(sp, skylark = skylark)

  datayear <- sort(unique(data[["year"]]))

  if (missing(year)) {
    year <- datayear
  } else {
    year <- parse_int_range(year)
  }

  year <- as.character(year)

  if (missing(base) || !base %in% datayear) {
    base <- 1L
  } else {
    base <- which(datayear == as.character(base))
  }

  hash <- digest::digest(list(sp, year, base))

  cached_data <- get_from_cache(hash)

  if (is_cached(cached_data)) {

    unserialize(unlist(cached_data[["data"]]))

  } else {

    calc_index(sp, year, base)

  }

}

#' @importFrom promises future_promise
#' @importFrom digest digest
#' @importFrom rtrim index trim
#'
calc_index <- function(sp, year, base) {

  force(sp)
  force(year)
  force(base)

  data <- switch(sp, skylark = skylark)

  hash <- digest::digest(list(sp, year, base))

  promises::future_promise({
    ans <- rtrim::index(
      rtrim::trim(count ~ site + year, data = data, model = 3L), base = base
    )
    rownames(ans) <- ans[["time"]]
    names(ans) <- c("year", "index", "sd")
    ans <- ans[year, ]
    ans[["year"]] <- as.integer(year)
    rownames(ans) <- NULL

    set_cache(hash, serialize(ans, NULL))

    ans},
    globals = c("data", "base", "year", "hash"),
    packages = c("rtrim", "indicators")
  )

}