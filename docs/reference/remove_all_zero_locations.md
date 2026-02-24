# Remove all-zero locations

Discard locations where taxa always had zero abundance

## Usage

``` r
remove_all_zero_locations(counts, ...)
```

## Arguments

- counts:

  Count data.

- ...:

  Additional arguments.

## Details

This function groups `counts` by `location_id` and then removes all
`counts` includes `location_id` and `abundance`.
