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
docker-compose up --build -d
```

## Usage
### JSON
[http://localhost:8000/data/wb](http://localhost:8000/data/wb)

### Graphics
[http://localhost:8000/svg/wb?taxa=MX.29008](http://localhost:8000/svg/wb?taxa=MX.29008)

![](pkg/man/figures/graph.svg)
