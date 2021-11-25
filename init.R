library("indicators")
options(
  finbif_use_cache = FALSE,
  finbif_api_url = Sys.getenv("FINBIF_API"),
  finbif_warehouse_query = Sys.getenv("FINBIF_WAREHOUSE_QUERY"),
  finbif_email = Sys.getenv("FINBIF_EMAIL"),
  indicator_logging = TRUE
)

create_output_cache()
create_input_cache_index()
clean_input_cache()

library("promises")
library("future")
plan("multisession")

library("rapidoc")
library("plumber")
p <- plumb("api.R")
p$run(host = "0.0.0.0", port = 8000L)
