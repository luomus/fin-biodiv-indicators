#' @importFrom dplyr filter group_by inner_join left_join mutate summarise
#' @importFrom rlang .data
#' @importFrom tidyr replace_na

get_process_counts_fun <- function(index) {

  switch(
    index,
    wb = function(data) {

      counts <- dplyr::left_join(
        data[["surveys"]], data[["sp_data"]], by = "document_id"
      )

      counts <- dplyr::mutate(
        counts, abundance = tidyr::replace_na(.data[["abundance"]], 0L)
      )

      counts <- dplyr::group_by(counts, .data[["location_id"]])

      dplyr::filter(counts, max(.data[["abundance"]]) >= 1L)

    },
    bf = function(data) {

      counts <- dplyr::filter(data[["sp_data"]], !is.na(.data[["section"]]))

      counts <- dplyr::inner_join(
        counts, data[["surveys"]], by = "document_id"
      )

      counts <- dplyr::group_by(counts, .data[["document_id"]])

      counts <- dplyr::summarise(counts, abundance = sum(.data[["abundance"]]))

      counts <- dplyr::left_join(data[["surveys"]], counts, by = "document_id")

      counts <- mutate(
        counts, abundance = tidyr::replace_na(.data[["abundance"]], 0L)
      )

      counts <- dplyr::group_by(counts, .data[["location_id"]], .data[["year"]])

      counts <- dplyr::summarise(
        counts, abundance = sum(.data[["abundance"]]), .groups = "drop_last"
      )

      dplyr::filter(counts, max(.data[["abundance"]]) >= 1L)

    }
  )

}
