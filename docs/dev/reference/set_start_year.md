# Set start year

Discard counts from years before the start year

## Usage

``` r
set_start_year(counts, taxon, ...)
```

## Arguments

- counts:

  Count data.

- taxon:

  Taxon configuration.

- ...:

  Additional arguments.

## Details

This function sets a start year for a taxon `counts`. If a variable
`start_year` has been configured for the given taxon all count data
prior to the `start_year` is removed.
