options(
  finbif_use_cache = TRUE,
  finbif_allow_query = FALSE,
  finbif_cache_path = "cache"
)

Sys.setenv(R_CONFIG_FILE = "/home/user/config.yml")

con <- pool::dbPool(RPostgres::Postgres())

pool::dbExecute(con, "CREATE EXTENSION tablefunc")

expect_true(update_data("counts", "bf", "MX.60914", con))

expect_false(update_data("counts", "bf", "MX.60914", con))

pool::dbWriteTable(con, "surveys", data.frame(index = character()))

expect_true(update_data("surveys", "bf", NULL, con))

expect_null(update_taxon_index("bf", "MX.60914", con))

expect_null(update_index("bf", con))
