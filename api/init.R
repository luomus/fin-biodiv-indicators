library("indicators")
options(
  finbif_cache_path = "tmpsvgs",
  finbif_warehouse_query = "warehouse/private-query/",
  finbif_email = Sys.getenv("FINBIF_EMAIL"),
  indicator_logging = TRUE
)

create_output_cache()
create_input_cache_index()
clean_input_cache()

library("promises")
library("future")
plan("multisession")

library("plumber")
p <- plumb("api.R")
p$run(host = "0.0.0.0", port = 8000L)
