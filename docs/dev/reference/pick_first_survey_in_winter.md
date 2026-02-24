# Pick first survey in winter

Pick first winter survey in each year discarding subsequent surveys

## Usage

``` r
pick_first_survey_in_winter(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function moves surveys occurring in December ahead one `year`. This
enables all December surveys to be grouped with subsequent surveys
occurring in the January of the same winter. Surveys are then grouped by
`location_id` and `year` and then ordered by date. Then all but the
first survey in each group is removed. If two or more surveys share the
same date and `location_id` then one is picked at random and the rest
are removed. This function works on the assumption that surveys are in
winter from December to January and that the `surveys` data includes
`day`, `month` and `year` (as integers) and `location_id`.
