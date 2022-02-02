#' @importFrom dplyr dense_rank filter group_by inner_join mutate right_join
#' @importFrom dplyr select slice_min summarise ungroup

process_funs <- list(

  shift_year_winter = function(surveys) {

    dplyr::mutate(
      surveys,
      year = ifelse(
        .data[["month"]] == 12L, .data[["year"]] + 1L, .data[["year"]]
      )
    )

  },

  pick_first_survey_in_year = function(surveys) {

    surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

    surveys <- dplyr::slice_min(
      surveys, 1L / .data[["month"]] + .data[["day"]] / 100L, n = 1L,
      with_ties = FALSE
    )

    dplyr::ungroup(surveys)

  },

  require_seven_fortnights = function(surveys) {

    surveys <- dplyr::mutate(
      surveys,
      fortnight = .data[["month"]] * 2L + as.integer(.data[["day"]] > 15L)
    )

    surveys <- dplyr::group_by(surveys, .data[["location_id"]], .data[["year"]])

    surveys <- dplyr::mutate(
      surveys, fortnight = dplyr::dense_rank(.data[["fortnight"]])
    )

    surveys <- dplyr::filter(
      surveys, max(.data[["fortnight"]], na.rm = TRUE) >= 7L
    )

    dplyr::ungroup(surveys)

  },

  require_gt_two_years = function(surveys) {

    surveys <- dplyr::group_by(surveys, .data[["location_id"]])

    surveys <- dplyr::mutate(
      surveys, year_rank = dplyr::dense_rank(.data[["year"]])
    )

    surveys <- dplyr::filter(
      surveys, max(.data[["year_rank"]], na.rm = TRUE) >= 2L
    )

    surveys <- dplyr::select(surveys, !.data[["year_rank"]])

    dplyr::ungroup(surveys)

  },

  pick_first_survey_in_fortnight = function(surveys) {

    surveys <- dplyr::group_by(
      surveys, .data[["location_id"]], .data[["year"]], .data[["fortnight"]]
    )

    surveys <- dplyr::slice_min(
      surveys, .data[["day"]], n = 1L, with_ties = FALSE
    )

    dplyr::ungroup(surveys)

  },

  zero_fill = function(counts, surveys, ...) {

    counts <- dplyr::right_join(counts, surveys, by = "document_id")

    dplyr::mutate(
      counts,
      abundance = ifelse(is.na(.data[["abundance"]]), 0L, .data[["abundance"]])
    )

  },

  remove_all_zero_locations = function(counts, ...) {

    counts <- dplyr::group_by(counts, .data[["location_id"]])

    counts <- dplyr::filter(
      counts, max(.data[["abundance"]], na.rm = TRUE) >= 1L
    )

    dplyr::ungroup(counts)

  },

  sum_over_sections = function(counts, surveys, ...) {

    counts <- dplyr::filter(counts, !is.na(.data[["section"]]))

    counts <- dplyr::inner_join(counts, surveys, by = "document_id")

    counts <- dplyr::group_by(counts, .data[["document_id"]])

    counts <- dplyr::summarise(
      counts, abundance = sum(.data[["abundance"]], na.rm = TRUE)
    )

    dplyr::ungroup(counts)

  },

  sum_by_event = function(counts, ...) {

    counts <- dplyr::group_by(counts, .data[["location_id"]], .data[["year"]])

    counts <- dplyr::summarise(
      counts, abundance = sum(.data[["abundance"]], na.rm = TRUE),
      .groups = "drop_last"
    )

    dplyr::ungroup(counts)

  },

  set_start_year = function(counts, taxon, ...) {

    if (exists("start_year", taxon)) {

      counts <- dplyr::filter(counts, .data[["year"]] > !!taxon[["start_year"]])

    }

    counts

  }

)
