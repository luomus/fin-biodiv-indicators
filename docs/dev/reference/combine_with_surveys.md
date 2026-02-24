# Combine with surveys

Combine count data with survey data

## Usage

``` r
combine_with_surveys(counts, surveys, ...)
```

## Arguments

- counts:

  Count data.

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function combines `counts` and `surveys` data. It performs an inner
join of `counts` on `surveys` by `document_id`. The function assumes
that both `counts` and `surveys` data include `document_id`.
