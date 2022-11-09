# Finnish Biodiversity Indicators

Finnish biodiversity indicators (FBI) is a service providing time series of
abundance indices and related metrics for Finland. The input data for the
indices are provided by the
[Finnish Biodiversity Information Facility](https://laji.fi "FinBIF").

![Biodiversity Indicator for Birds in Winter](https://indicators.laji.fi/svg/wb "Winter Birds")

## Indicator types

Indicators are either single-taxa or multi-taxa. All indicators are calculated
for the whole of Finland and separate indices for the North (Lappi, Kainuu and
Pohjois-Pohjanmaa regions) and South (all other regions).

### Single-taxa indicators

Single-taxa indicators track the average abundance across monitoring sites,
relative to a base year. For example, if the base year is the year 2000 and the
the indicator for 2003 is 0.8, then the average abundance across sites monitored
in 2003 has declined by 20 percentage points relative to the average abundance
at sites monitored in the year 2000. Single taxa indices can be calculated using
different models including [TRIM](https://github.com/SNStatComp/rtrim/) and
[RBMS](https://retoschmucki.github.io/rbms/).

### Multi-taxa indicators

Multi-taxa indicators come in different forms. These include: the geometric mean
of relative abundance across a set of taxa; the community temperature index for
a set of taxa; and the total expected abundance for a taxon group.
