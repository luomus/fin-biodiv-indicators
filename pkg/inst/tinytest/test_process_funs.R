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
