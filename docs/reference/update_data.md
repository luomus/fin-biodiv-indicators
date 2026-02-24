# Update data

Update input data from FinBIF.

## Usage

``` r
update_data(type, index, taxon, db, do_update = FALSE)
```

## Arguments

- type:

  Character. Which type of input data (e.g., surveys or counts)

- index:

  Character. Update the data of which index?

- taxon:

  Character. Update the data for which taxon? Ignored if
  `type = "surveys"`

- db:

  Connection. Database in which to update the data from FinBIF.

- do_update:

  Logical. Update data regardless of need.
