# Require at least two years

Discard locations with less than two survey years

## Usage

``` r
require_two_years(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function groups `surveys` by `location_id` and then removes all
surveys for locations that do not have data in more than one `year`. The
function assumes that `surveys` has data for `location_id` and `year`.
