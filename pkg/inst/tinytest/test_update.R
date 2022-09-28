options(
  finbif_use_cache = TRUE,
  finbif_allow_query = FALSE,
  finbif_cache_path = "cache"
)

Sys.setenv(R_CONFIG_FILE = "/home/user/config.yml")

con <- pool::dbPool(RPostgres::Postgres())

pool::dbExecute(con, "CREATE EXTENSION tablefunc")

expect_true(update_data("counts", "bf", list(code = "MX.60914"), con))

expect_false(update_data("counts", "bf", list(code = "MX.60914"), con))

pool::dbWriteTable(con, "surveys", data.frame(index = "redundant"))

expect_true(update_data("surveys", "bf", NULL, con))

expect_null(update_taxon_index("bf", "rbms", list(code = "MX.60914"), con))

Sys.setenv(bf_UCY = "false")

expect_null(update_taxon_index("bf", "trim", list(code = "MX.60914"), con))

expect_null(update_taxon_index("bf", "trim", list(code = "MX.MISSING"), con))

expect_null(update_index("bf", "trim", NULL, con))

expect_null(update_index("bf_south", "trim", NULL, con))

expect_null(update_index("bfcti", "lmer", NULL, con))

expect_null(update_index("bfoa", "glmer", NULL, con))

pool::dbWriteTable(con, "redundant", data.frame(index = character()))

expect_true(check_input("bf", "rbms", "MX.60914"))

expect_inherits(
  get_output("data", "bf", "rbms", "MX.60914", "none", con), "raw"
)

expect_inherits(
  get_output("data", "bf", "trim", "MX.MISSING", "none", con), "list"
)

expect_inherits(
  get_output("data", "bf", "trim", "MX.60916", "none", con), "list"
)

expect_null(clean_cache(con))

pool::poolClose(con)
