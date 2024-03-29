## Overview

Finnish biodiversity indicators (FBI) is a service providing time series of
abundance indices and related metrics for Finland. The input data for the
indicators are provided by the
[Finnish Biodiversity Information Facility](https://laji.fi "FinBIF").

### **Indicator types**

Indicators are either single-taxon or multi-taxon. All indicators are calculated
for the whole of Finland as well as separately for the North (Lappi, Kainuu and
Pohjois-Pohjanmaa regions) and South (all other regions).

#### **Single-taxon indicators**

Single-taxon indicators track the average abundance across monitoring sites,
relative to a base year. For example, if the base is the year 2000 and the
indicator for 2003 is 0.8, then the average abundance across sites monitored in
2003 declined by 20 percentage points relative to the average abundance at sites
monitored in the year 2000. Single-taxon indicators can be calculated using
different models including
[TRIM](https://github.com/SNStatComp/rtrim/ "rtrim Github Repository") and
[RBMS](https://retoschmucki.github.io/rbms/ "rbms Website"). Single-taxon
indicators "belong" to a multi-taxon indicator but an individual taxa can
constitute more than one single-taxon indicator each "belonging" to a different
multi-taxon indicator. For example, [Goldcrest (_Regulus regulus_)
](https://laji.fi/en/taxon/MX.33954 "Goldcrest - Regulus regulus")
, belongs to both the Winter Bird indicator and the Forest Breeding Bird
indicator and there are two corresponding single-taxon indicators for this 
species. Not all taxon that "belong" to a multi-taxon indicator contribute to 
the given indicator (see
[/taxa-extra](#get-/taxa-extra/-index- "List of extra taxa available")). For
example, there is a single-taxon indicator for
[Eurasian Sparrowhawk (_Accipiter nisus_)
](https://laji.fi/en/taxon/MX.26639 "Eurasian Sparrowhawk - Accipiter nisus")
in winter that does not contribute the multi-taxon Winter Bird Indicator.

#### **Multi-taxon indicators**

Multi-taxon indicators come in different forms. These include: the geometric
mean of relative abundance across a set of taxa; the community temperature index
for a set of taxa; and the total expected abundance for a taxon group. Not all
multi-taxon indicators have single-taxon indicators that belong to them. 
Instead, multi-taxon indicators without single-taxon indicators can "reuse" the
input of another multi-taxon indicator. For example, while there are no
single-taxon indices that "belong" to the Winter Bird Community Temperature
Index, this multi-taxon indicator is constructed from the taxa (and extra taxa)
that fall under the Winter Bird indicator.

### **Supported Operations**

* Get lists of available indicators
* Get the configuration of an indicator
* Get statistics for an indicator
* Get the output data for an indicator
  * as CSV
  * as JSON
* Get an SVG image for a indicator

### **Examples**

Get a list of the multi-taxon indicators:

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

Get a single-taxon (Goldcrest) Winter Bird trim index output for
southern Finland as CSV:

```bash
$ curl "https://indicators.laji.fi/csv/wb?taxon=MX.33954&model=trim&region=south"
```

Get an SVG image of the Farmland Butterflies RBMS index

```bash
$ curl -o bf.svg "https://indicators.laji.fi/svg/bf?model=rbms"
```
