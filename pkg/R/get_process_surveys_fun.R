#' @importFrom dplyr filter group_by mutate n_distinct slice_head ungroup
#' @importFrom rlang .data

get_process_surveys_fun <- function(index) {

  switch(
    index,
    wb = function(surveys) {

      surveys <- arrange(
        surveys, .data[["year"]], .data[["month"]], .data[["day"]]
      )

      surveys <- dplyr::mutate(
        surveys,
        year = ifelse(
          .data[["month"]] == 12L, .data[["year"]] + 1L, .data[["year"]]
        )
      )

      surveys <- dplyr::group_by(
        surveys, .data[["location_id"]], .data[["year"]]
      )

      surveys <- dplyr::slice_head(surveys, n = 1L)

      dplyr::ungroup(surveys)

    },
    bf = function(surveys) {

      surveys <- arrange(
        surveys, .data[["year"]], .data[["month"]], .data[["day"]]
      )

      surveys <- dplyr::mutate(
        surveys,
        fortnight = .data[["month"]] * 2L + (.data[["day"]] > 15L) - 10L
      )

      surveys <- dplyr::group_by(
        surveys, .data[["location_id"]], .data[["year"]]
      )

      surveys <- filter(surveys, dplyr::n_distinct(.data[["fortnight"]]) >= 7L)

      surveys <- dplyr::group_by(surveys, .data[["location_id"]])

      surveys <- filter(surveys, dplyr::n_distinct(.data[["year"]]) >= 2L)

      surveys <- dplyr::group_by(
        surveys, .data[["location_id"]], .data[["year"]], .data["fortnight"]
      )

      surveys <- dplyr::slice_head(surveys, n = 1L)

      dplyr::ungroup(surveys)

    }
  )

}
