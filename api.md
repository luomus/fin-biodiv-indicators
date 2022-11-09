## Overview

Finnish biodiversity indicators (FBI) is a service providing time series of
abundance indices and related metrics for Finland. The input data for the
indicators are provided by the
[Finnish Biodiversity Information Facility](https://laji.fi "FinBIF").

### **Indicator types**

Indicators are either single-taxa or multi-taxa. All indicators are calculated
for the whole of Finland and separate indicators for the North (Lappi, Kainuu
and Pohjois-Pohjanmaa regions) and South (all other regions).

#### **Single-taxa indicators**

Single-taxa indicators track the average abundance across monitoring sites,
relative to a base year. For example, if the base year is the year 2000 and the
the indicator for 2003 is 0.8, then the average abundance across sites monitored
in 2003 has declined by 20 percentage points relative to the average abundance
at sites monitored in the year 2000. Single taxa indicators can be calculated
using different models including
[TRIM](https://github.com/SNStatComp/rtrim/ "rtrim Github Repository") and
[RBMS](https://retoschmucki.github.io/rbms/ "rbms Website").

#### **Multi-taxa indicators**

Multi-taxa indicators come in different forms. These include: the geometric mean
of relative abundance across a set of taxa; the community temperature index for
a set of taxa; and the total expected abundance for a taxon group.

### **Supported Operations**

* Get lists of available indicators
* Get the configuration of an indicator
* Get statistics for an indicator
* Get the output data for an indicator
  * as csv
  * as json
* Get an SVG image for a indicator

### **Examples**

Get a list of the multi-taxa indicators:

```bash
$ curl "https://indicators.laji.fi/indices"
```

Get a list of taxa included in the Winter Bird indicator:

```bash
$ curl "https://indicators.laji.fi/taxa/wb"
```

See how the Winter Bird indicator is configured:

```bash
$ curl "https://indicators.laji.fi/config/wb"
```

Get the Winter Bird indicator output as json:

```bash
$ curl "https://indicators.laji.fi/data/wb"
```

Get a single taxon (Goldcrest) Winter Bird trim index output for
southern Finland as CSV:

```bash
$ curl "https://indicators.laji.fi/csv/wb?taxon=MX.33954&model=trim&region=south"
```

Get an SVG image of the Farmland Butterflies RBMS index

```bash
$ curl -o bf.svg "https://indicators.laji.fi/svg/bf?model=rbms"
```
