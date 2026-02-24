# Sum by event

Sum the counts over the surveys or taxa in each year

## Usage

``` r
sum_by_event(counts, ...)
```

## Arguments

- counts:

  Count data.

- ...:

  Additional arguments.

## Details

This functions groups count data by `location_id` and `year`. If
multiple taxa `counts` are input then data is also grouped by taxa.
Counts are then summed across the survey events at the locations and
years.
