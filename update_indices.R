library(config)
library(pool)
library(RPostgres)

pool <- pool::dbPool(RPostgres::Postgres())

for (index in config::get("indices")) {

  index_update <- FALSE

  surveys <- update_data("surveys", index, NULL, pool)

  for (taxon in config::get("taxa", config = index)) {

    counts <- update_data("counts", index, taxon, pool)

    taxon_index_update <- surveys || counts

    if (taxon_index_update) {

      update_taxon_index(index, taxon, pool)

    }

    index_update <- index_update || taxon_index_update

  }

  if (index_update) {

    update_index(index, pool)

  }

}

pool::poolClose(pool)
