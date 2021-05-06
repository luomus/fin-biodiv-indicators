# Finnish Biodiversity Indicators API

An API for biodiversity indicators from Finland

## Requirements

* `git`
* `docker`
* `docker-compose`

## Install
```{bash}
git clone https://github.com/luomus/fin-biodiv-indicators.git
```

## Run
To run on [http://localhost:8000](http://localhost:8000) 
```{bash}
cd fin-biodiv-indicators
FINBIF_ACCESS_TOKEN=<token> PGUSER=<user> PGPASSWORD=<password> docker-compose up --build -d
```

## Usage
### JSON
[http://localhost:8000/sp-index/json?sp=SURULU&year=1985:1988](http://localhost:8000/sp-index/json?sp=SURULU&year=1985:1988)
```{javascript}
[
  {
    "year": 1985,
    "index": 0.7096,
    "sd": 0.0771
  },
  {
    "year": 1986,
    "index": 0.841,
    "sd": 0.078
  },
  {
    "year": 1987,
    "index": 0.829,
    "sd": 0.0773
  },
  {
    "year": 1988,
    "index": 0.9182,
    "sd": 0.084
  }
]
```

### CSV
[http://localhost:8000/sp-index/csv?sp=SURULU&year=1985:1988](http://localhost:8000/sp-index/csv?sp=SURULU&year=1985:1988)

### Graphics
[http://localhost:8000/sp-plot?sp=SURULU](http://localhost:8000/sp-plot?sp=SURULU)

![](api/pkg/man/figures/graph.svg)
