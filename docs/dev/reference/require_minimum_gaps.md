# Require minimum gaps

Remove survey site-years that too many or to large sampling gaps.

## Usage

``` r
require_minimum_gaps(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function groups surveys data by `location_id` and `year`. It then
removes groups where the survey period has too many or too large
sampling gaps. Where too many is defined as a total gap length over the
`year` of 21 days and too large is any single sampling gap of more than
7 days. The function expects the `surveys` data to have at least
`location_id`, `year`, `ordinal_day_start` and `ordinal_day_end`.
