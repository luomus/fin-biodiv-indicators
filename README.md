# Finnish Biodiversity Indicators API

An API for biodiversity indicators from Finland

## Requirements

* `git`
* `docker`
* `docker-compose`

## Install
```
git clone https://github.com/luomus/fin-biodiv-indicators.git
```

## Run
To run on [https://api.localhost](http://api.localhost) 
```
cd fin-biodiv-indicators
FINBIF_ACCESS_TOKEN=<token> EMAIL=<email> HOST=localhost PGUSER=<user> PGPASSWORD=<password> docker-compose up --build -d
```

## Usage

[https://api.localhost/graph?type=test](http://api.localhost/graph?type=test)

![](pkg/man/figures/graph.svg)
