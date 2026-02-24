# Pick first survey in year

Pick first survey in each year discarding subsequent surveys

## Usage

``` r
pick_first_survey_in_year(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function groups surveys by `location_id` and `year` then orders
them by date. All but the first survey in each group is removed. If two
or more surveys share the same date and `location_id` then one is picked
at random and the rest are removed. The function assumes that the
`surveys` data includes `day`, `month` and `year` (as integers) and
`location_id`.
