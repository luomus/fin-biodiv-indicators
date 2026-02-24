# Pick first survey in fortnight

Pick first survey in each fortnight discarding subsequent surveys

## Usage

``` r
pick_first_survey_in_fortnight(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function groups surveys by `location_id`, `year` and
`fortnight`then orders them by date. All but the first survey in each
group is removed. If two or more surveys share the same date and
`location_id` then one is picked at random and the rest are removed. The
function assumes that the `surveys` data includes `day`, and `year` (as
integers) and `location_id`, and has been processed by the function
`require_seven_fortnights`.
