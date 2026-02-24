# Format date

Combine year, month, day of survey into a single date string

## Usage

``` r
format_date(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function combines survey `year`, `month` and `day` into a character
string with `-` as a separator. The function assumes that survey data
includes `year`, `month` and `day`.
