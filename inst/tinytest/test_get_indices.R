Sys.setenv(R_CONFIG_FILE = "config.yml")

expect_inherits(get_indices(), "list")
