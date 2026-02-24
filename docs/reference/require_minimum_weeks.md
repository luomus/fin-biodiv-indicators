# Require minimum weeks

Remove survey site-years from a region covering less than a minimum
number of weeks.

## Usage

``` r
require_minimum_weeks(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function groups surveys data by `location_id` and `year`. It then
removes groups where the survey period is less than a minimum number of
weeks for a given `region`. It expects the `surveys` data to have at
least `location_id`, `year`, `region`, `ordinal_day_start` and
`ordinal_day_end`.
