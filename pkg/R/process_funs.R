#' Process functions
#'
#' Functions to process indicator input data.
#'
#' @param surveys Survey data.
#' @param counts Count data.
#' @param taxon Taxon configuration.
#' @param ... Additional arguments.
#'
#' @importFrom dplyr dense_rank filter group_by inner_join left_join mutate
#' @importFrom dplyr right_join select slice_min summarise ungroup
#' @export
process_funs <- function() {

  list(
    pick_first_survey_in_winter    = pick_first_survey_in_winter,
    pick_first_survey_in_year      = pick_first_survey_in_year,
    require_seven_fortnights       = require_seven_fortnights,
    require_two_years              = require_two_years,
    pick_first_survey_in_fortnight = pick_first_survey_in_fortnight,
    format_date                    = format_date,
    combine_with_surveys           = combine_with_surveys,
    zero_fill                      = zero_fill,
    remove_all_zero_locations      = remove_all_zero_locations,
    sum_over_sections              = sum_over_sections,
    sum_by_event                   = sum_by_event,
    set_start_year                 = set_start_year
  )

}

#' Pick first survey in winter
#'
#' Pick first winter survey in each year discarding subsequent surveys
#'
#' @details This function moves `surveys` occurring in December ahead one year.
#'   This enables all December surveys to be grouped with subsequent surveys
#'   occurring in the January of the same winter. Surveys are then grouped by
#'   location and year then ordered by date. Then all but the first survey in
#'   each group is removed. If two or more surveys share the same date and
#'   location then one is picked at random and the rest are removed. This
#'   function works on the assumption that surveys occur in winter from December
#'   to January and that the `surveys` data includes at least `day`, `month` and
#'   `year` (as integers) and `location_id`.
#'
#' @export
#' @inheritParams process_funs
pick_first_survey_in_winter <- function(surveys) {

  surveys <- dplyr::mutate(
    surveys,
    year = ifelse(
      .data[["month"]] == 12L, .data[["year"]] + 1L, .data[["year"]]
    )
  )

  surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

  surveys <- dplyr::slice_min(
    surveys, 1L / .data[["month"]] + .data[["day"]] / 100L, n = 1L,
    with_ties = FALSE
  )

  dplyr::ungroup(surveys)

}

#' Pick first survey in year
#'
#' Pick first survey in each year discarding subsequent surveys
#'
#' @details This function groups `surveys` by location and year then orders
#'   them by date. All but the first survey in each group is removed. If two or
#'   more surveys share the same date and location then one is picked at random
#'   and the rest are removed. The function assumes the `surveys` data at least
#'   `day`, `month` and `year` (as integers) and `location_id`.
#'
#' @export
#' @inheritParams process_funs
pick_first_survey_in_year <- function(surveys) {

  surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

  surveys <- dplyr::slice_min(
    surveys, .data[["month"]] + .data[["day"]] / 10L, n = 1L,
    with_ties = FALSE
  )

  dplyr::ungroup(surveys)

}

#' Require seven fortnights
#'
#' Divide year into approximate 2 week blocks selecting blocks 10-16 and
#' discarding locations without a survey in each remaining block
#'
#' @details This function assigns each survey to an approximate fortnight. Where
#'  a fortnight is defined as all the days before the 16th day of each month and
#'  all the days after the 15th day of each month. Then all the surveys falling
#'  outside of the date range of the seven fortnights from the second fortnight
#'  of May to the second fortnight of August are removed. Surveys are then
#'  grouped by location and year and all surveys belonging to groups that do not
#'  have at least one surveys occurring in the seven remaining fortnights are
#'  discarded. The function assumes the `surveys` data at least `day`, `month`
#'  and `year` (as integers) and `location_id`.
#'
#' @export
#' @inheritParams process_funs
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


#' Require at least two years
#'
#' Discard locations with less than two survey years
#'
#' @export
#' @inheritParams process_funs
require_two_years <- function(surveys) {

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

#' Pick first survey in fortnight
#'
#' Pick first survey in each fortnight discarding subsequent surveys
#'
#' @export
#' @inheritParams process_funs
pick_first_survey_in_fortnight <- function(surveys) {

  surveys <- dplyr::group_by(
    surveys, .data[["location_id"]], .data[["year"]], .data[["fortnight"]]
  )

  surveys <- dplyr::slice_min(
    surveys, .data[["day"]], n = 1L, with_ties = FALSE
  )

  dplyr::ungroup(surveys)

}

#' Format date
#'
#' Combine year, month, day of survey into a single date string
#'
#' @export
#' @inheritParams process_funs
format_date <- function(surveys) {

  dplyr::mutate(
    surveys,
    date = paste(.data[["year"]], .data[["month"]], .data[["day"]], sep = "-")
  )

}

#' Combine with surveys
#'
#' Combine count data with survey data
#'
#' @export
#' @inheritParams process_funs
combine_with_surveys <- function(counts, surveys, ...) {

  dplyr::left_join(counts, surveys, by = "document_id")

}

#' Zero fill
#'
#' Combine count data with survey data filling missing surveys in count data
#' with zero counts.
#'
#' @export
#' @inheritParams process_funs
zero_fill <- function(counts, surveys, ...) {

  counts <- dplyr::right_join(counts, surveys, by = "document_id")

  dplyr::mutate(
    counts,
    abundance = ifelse(is.na(.data[["abundance"]]), 0L, .data[["abundance"]])
  )

}


#' Remove all-zero locations
#'
#' Discard locations where taxa always had zero abundance
#'
#' @export
#' @inheritParams process_funs
remove_all_zero_locations <- function(counts, ...) {

  counts <- dplyr::group_by(counts, .data[["location_id"]])

  counts <- dplyr::filter(
    counts, max(.data[["abundance"]], na.rm = TRUE) >= 1L
  )

  dplyr::ungroup(counts)

}

#' Sum over sections
#'
#' Sum counts over the sections of surveys
#'
#' @export
#' @inheritParams process_funs
sum_over_sections <- function(counts, ...) {

  counts <- dplyr::group_by(counts, .data[["document_id"]])

  if ("index" %in% colnames(counts)) {

    counts <- dplyr::group_by(counts, .data[["index"]], .add = TRUE)

  }

  counts <- dplyr::summarise(
    counts, abundance = sum(.data[["abundance"]], na.rm = TRUE),
    .groups = "drop"
  )

}

#' Sum by event
#'
#' Sum the counts over the surveys or taxa in each year
#'
#' @export
#' @inheritParams process_funs
sum_by_event <- function(counts, ...) {

  counts <- dplyr::group_by(counts, .data[["location_id"]], .data[["year"]])

  if ("index" %in% colnames(counts)) {

    counts <- dplyr::group_by(counts, .data[["index"]], .add = TRUE)

  }

  dplyr::summarise(
    counts, abundance = sum(.data[["abundance"]], na.rm = TRUE),
    .groups = "drop"
  )

}

#' Set start year
#'
#' Discard counts from years before the start year
#'
#' @export
#' @inheritParams process_funs
set_start_year <- function(counts, taxon, ...) {

  if (exists("start_year", taxon)) {

    counts <- dplyr::filter(counts, .data[["year"]] >= !!taxon[["start_year"]])

  }

  counts

}
