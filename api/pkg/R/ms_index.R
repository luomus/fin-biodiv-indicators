#' Multi-species population indicator
#'
#' Create a multi-species population indicator.
#'
#' @param index Which index.
#' @param id Request ID for logging.
#'
#' @importFrom digest digest
#' @importFrom promises promise_resolve
#'
#' @export

ms_index <- function(index, id) {

  hash <- digest::digest(index)

  log_message(id, "Checking output cache for multi-species index data: ", hash)

  cached_data <- get_from_output_cache(hash)

  if (is_output_cached(cached_data)) {

    log_message(id, "Getting multi-species index data from output cache")

    promises::promise_resolve(unserialize(unlist(cached_data[["data"]])))

  } else {

    calc_ms_index(index, hash = hash, id = id)

  }

}

#' @importFrom dplyr filter group_by if_else lead mutate right_join summarise
#' @importFrom dplyr ungroup
#' @importFrom promises promise_all then
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom stats rnorm sd
#' @importFrom tibble tibble
#' @importFrom tidyr expand unnest
#' @importFrom utils head

calc_ms_index <- function(
  index, n = 1000L, maxcv = 3, minindex = .01, trunc = 10, hash, id
) {

  spp <- species(index, "spcode")

  df <- promises::promise_all(
    .list = purrr::map(spp, ~sp_index(index = index, .x, id = id))
  )

  promises::then(
    df,
    ~{
      df <- tibble::tibble(sp = spp, data = .)
      df <- tidyr::unnest(df, data)
      df <- dplyr::right_join(
        df,
        tidyr::expand(df, .data[["sp"]], .data[["year"]]),
        by = c("sp", "year")
      )
      df <- dplyr::mutate(
        df,
        cv = dplyr::if_else(
          .data[["index"]] >= .1 & .data[["sd"]] > 0,
          .data[["sd"]] / .data[["index"]],
          NA_real_
        )
      )
      df <- dplyr::mutate(df, cv = mean(.data[["cv"]], na.rm = TRUE))
      df <- dplyr::filter(df, .data[["cv"]] < maxcv)
      df <- dplyr::mutate(df, index = pmax(.data[["index"]], minindex))
      df <- dplyr::mutate(
        df, sd = dplyr::if_else(.data[["index"]] > minindex, .data[["sd"]], 0)
      )
      df <- dplyr::group_by(df, .data[["sp"]], .data[["year"]])
      df <- dplyr::summarise(
        df,
        mc = stats::rnorm(
          n, log(.data[["index"]]), .data[["sd"]] / .data[["index"]]
        ),
        i = seq_len(n),
        .groups = "keep"
      )
      df <- dplyr::mutate(df, mc = pmax(.data[["mc"]], log(minindex)))
      df <- dplyr::group_by(df, .data[["i"]], .data[["sp"]])
      df <- dplyr::mutate(df, mc = dplyr::lead(.data[["mc"]]) - .data[["mc"]])
      df <- dplyr::mutate(df, mc = pmin(.data[["mc"]], log(trunc)))
      df <- dplyr::mutate(df, mc = pmax(.data[["mc"]], log(1 / trunc)))
      df <- dplyr::group_by(df, .data[["i"]], .data[["year"]])
      df <- dplyr::summarise(df, mc = mean(.data[["mc"]]), .groups = "keep")
      df <- dplyr::ungroup(df, .data[["year"]])
      df <- dplyr::mutate(
        df, mc = cumsum(c(0, utils::head(.data[["mc"]], -1L)))
      )
      df <- dplyr::group_by(df, .data[["year"]])

      ans <- dplyr::summarise(
        df, index = exp(mean(.data[["mc"]])),
        sd = stats::sd(.data[["mc"]]) * exp(mean(mc))
      )

      set_output_cache(hash, serialize(ans, NULL))

      ans
    }
  )
}
