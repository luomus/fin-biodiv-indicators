library(tibble)

df <- data.frame(location_id = "", year = 1:2, month = c(1:12), day = 1)

expect_equal(
  pick_first_survey_in_year(df),
  tibble(location_id = "", year = 1:2, month = 1:2, day = 1)
)

expect_equal(
  pick_first_survey_in_winter(subset(df, month %in% 11:12)),
  tibble(location_id = "", year = c(1, 3), month = 11:12, day = 1)
)

expect_equal(
  set_start_year(df, list(start_year = 2)),
  data.frame(location_id = "", year = 2, month = 1:6 * 2, day = 1)
)

expect_equal(
  fbi:::apply_process(
    list(list(format_date = list(list(dummy = NULL)))), "surveys", surveys = df
  ),
  fbi:::apply_process("format_date", "surveys", surveys = df)
)

df <- tibble(
  location_id = "", year = 1, region = "x", ordinal_day_start = 1:10,
  ordinal_day_end = 2:11
)

expect_equal(require_minimum_weeks(df, region = "x", min_weeks = 0)[-6], df)

expect_equal(require_minimum_weeks(df, min_weeks = 0)[-6], df)

expect_equal(require_minimum_gaps(df), df)
