# Zero fill

Combine count data with survey data filling missing surveys in count
data with zero counts.

## Usage

``` r
zero_fill(counts, surveys, ...)
```

## Arguments

- counts:

  Count data.

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function combines `counts` and `surveys` data. It performs a right
outer join of `counts` on `surveys` by `document_id`. Then all surveys
with no corresponding data for abundance are filled with zero. The
function assumes that both `counts` and `surveys` data include
`document_id` and that `counts` data includes `abundance`.
