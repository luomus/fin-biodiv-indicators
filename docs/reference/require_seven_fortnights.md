# Require seven fortnights

Divide year into approximate 2 week blocks, selecting blocks 10-16 and
discarding locations without a survey in each remaining block

## Usage

``` r
require_seven_fortnights(surveys, ...)
```

## Arguments

- surveys:

  Survey data.

- ...:

  Additional arguments.

## Details

This function assigns each survey to an approximate fortnight. A
fortnight is defined as all the days before the 16th `day` of each
`month` and all the days after the 15th `day` of each `month`. Then all
the surveys falling outside of the date range of the seven fortnights
from the second fortnight of May to the second fortnight of August are
removed. Surveys are then grouped by `location_id` and `year` and all
surveys belonging to groups that do not have at least one survey
occurring in each of the seven remaining fortnights are discarded. The
function assumes that the `surveys` data has `day`, `month` and `year`
(as integers) and `location_id`.
