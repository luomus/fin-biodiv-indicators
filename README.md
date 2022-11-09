# Finnish Biodiversity Indicators

Finnish biodiversity indicators (FBI) is a service providing time series of
abundance indices and related metrics for Finland. The input data for the
indices are provided by the
[Finnish Biodiversity Information Facility](https://laji.fi "FinBIF").

## Documentation
See [here](https://indicators.laji.fi/) for details.

## HTTP API
See online [documentation](https://indicators.laji.fi/__docs__/#overview) for
details.

## Deploy

### Local

```bash
docker-compose up -d
```

### Openshift

Log in to an openshift instance and switch to or create a project. To deploy
run:

```bash
oc process -f openshift-template.yml --param-file=.env | oc create -f -
```

See openshift-template.yml for required environment variables (parameters) to
include in `.env` file.

## Tests

To run the test suite:

```bash
docker-compose --file docker-compose.test.yml build
docker-compose --file docker-compose.test.yml run --rm -u $(id -u) sut
docker-compose --file docker-compose.test.yml down
```
