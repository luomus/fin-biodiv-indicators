# Sum over sections

Sum counts over the sections of surveys

## Usage

``` r
sum_over_sections(counts, ...)
```

## Arguments

- counts:

  Count data.

- ...:

  Additional arguments.

## Details

This functions groups count data by `document_id` (the IDs of the
individual surveys). If multiple taxa `counts` are input then data is
also grouped by taxa. Counts are then summed across survey sections when
count data has been provided as surveys split into parts.
