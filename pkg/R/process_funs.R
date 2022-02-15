#' Process functions
#'
#' Functions to process indicator input data.
#'
#' @param surveys Survey data.
#' @param counts Count data.
#' @param taxon Taxon configuration.
#' @param ... Additional arguments.
#'
#' @importFrom dplyr dense_rank filter group_by inner_join mutate right_join
#' @importFrom dplyr select slice_min summarise ungroup
#' @export
process_funs <- function() {

  list(
    shift_year_winter              = shift_year_winter,
    pick_first_survey_in_winter    = pick_first_survey_in_winter,
    pick_first_survey_in_year      = pick_first_survey_in_year,
    require_seven_fortnights       = require_seven_fortnights,
    require_gt_two_years           = require_gt_two_years,
    pick_first_survey_in_fortnight = pick_first_survey_in_fortnight,
    zero_fill                      = zero_fill,
    remove_all_zero_locations      = remove_all_zero_locations,
    sum_over_sections              = sum_over_sections,
    sum_by_event                   = sum_by_event,
    set_start_year                 = set_start_year
  )

}

#' @export
#' @rdname process_funs
shift_year_winter <- function(surveys) {

  dplyr::mutate(
    surveys,
    year = ifelse(
      .data[["month"]] == 12L, .data[["year"]] + 1L, .data[["year"]]
    )
  )

}

#' @export
#' @rdname process_funs
pick_first_survey_in_winter <- function(surveys) {

  surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

  surveys <- dplyr::slice_min(
    surveys, 1L / .data[["month"]] + .data[["day"]] / 100L, n = 1L,
    with_ties = FALSE
  )

  dplyr::ungroup(surveys)

}

#' @export
#' @rdname process_funs
pick_first_survey_in_year <- function(surveys) {

  surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

  surveys <- dplyr::slice_min(
    surveys, .data[["month"]] + .data[["day"]] / 10L, n = 1L,
    with_ties = FALSE
  )

  dplyr::ungroup(surveys)

}

#' @export
#' @rdname process_funs
require_seven_fortnights <- function(surveys) {

  surveys <- dplyr::mutate(
    surveys,
    fortnight = .data[["month"]] * 2L + as.integer(.data[["day"]] > 15L)
  )

  surveys <- dplyr::filter(
    surveys, dplyr::between(.data[["fortnight"]], 11L, 17L)
  )

  surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

  surveys <- dplyr::mutate(
    surveys, fortnight = dplyr::dense_rank(.data[["fortnight"]])
  )

  surveys <- dplyr::filter(
    surveys, max(.data[["fortnight"]], na.rm = TRUE) >= 7L
  )

  dplyr::ungroup(surveys)

}

#' @export
#' @rdname process_funs
require_gt_two_years <- function(surveys) {

  surveys <- dplyr::group_by(surveys, .data[["location_id"]])

  surveys <- dplyr::mutate(
    surveys, year_rank = dplyr::dense_rank(.data[["year"]])
  )

  surveys <- dplyr::filter(
    surveys, max(.data[["year_rank"]], na.rm = TRUE) >= 2L
  )

  surveys <- dplyr::select(surveys, !.data[["year_rank"]])

  dplyr::ungroup(surveys)

}

#' @export
#' @rdname process_funs
pick_first_survey_in_fortnight <- function(surveys) {

  surveys <- dplyr::group_by(
    surveys, .data[["location_id"]], .data[["year"]], .data[["fortnight"]]
  )

  surveys <- dplyr::slice_min(
    surveys, .data[["day"]], n = 1L, with_ties = FALSE
  )

  dplyr::ungroup(surveys)

}

#' @export
#' @rdname process_funs
zero_fill <- function(counts, surveys, ...) {

  counts <- dplyr::right_join(counts, surveys, by = "document_id")

  dplyr::mutate(
    counts,
    abundance = ifelse(is.na(.data[["abundance"]]), 0L, .data[["abundance"]])
  )

}

#' @export
#' @rdname process_funs
remove_all_zero_locations <- function(counts, ...) {

  counts <- dplyr::group_by(counts, .data[["location_id"]])

  counts <- dplyr::filter(
    counts, max(.data[["abundance"]], na.rm = TRUE) >= 1L
  )

  dplyr::ungroup(counts)

}

#' @export
#' @rdname process_funs
sum_over_sections <- function(counts, ...) {

  counts <- dplyr::filter(counts, !is.na(.data[["section"]]))

  counts <- dplyr::group_by(counts, .data[["document_id"]])

  counts <- dplyr::summarise(
    counts, abundance = sum(.data[["abundance"]], na.rm = TRUE)
  )

  dplyr::ungroup(counts)

}

#' @export
#' @rdname process_funs
sum_by_event <- function(counts, ...) {

  counts <- dplyr::group_by(counts, .data[["location_id"]], .data[["year"]])

  counts <- dplyr::summarise(
    counts, abundance = sum(.data[["abundance"]], na.rm = TRUE),
    .groups = "drop_last"
  )

  dplyr::ungroup(counts)

}

#' @export
#' @rdname process_funs
set_start_year <- function(counts, taxon, ...) {

  if (exists("start_year", taxon)) {

    counts <- dplyr::filter(counts, .data[["year"]] >= !!taxon[["start_year"]])

  }

  counts

}
