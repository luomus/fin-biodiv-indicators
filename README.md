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
[http://localhost:8000/sp-index/json?index=wb&sp=surulu](http://localhost:8000/sp-index/json?index=wb&sp=surulu)
```{javascript}
[
  {
    "year": 1959,
    "index": 1,
    "sd": 0
  },
  {
    "year": 1960,
    "index": 0.7512,
    "sd": 0.0704
  },

...
```

### CSV
[http://localhost:8000/sp-index/csv?sp=SURULU](http://localhost:8000/sp-index/csv?index=wb&sp=surulu)

### Graphics
[http://localhost:8000/sp-plot?sp=SURULU](http://localhost:8000/sp-plot?index=wb&sp=surulu)

![](api/pkg/man/figures/graph.svg)
