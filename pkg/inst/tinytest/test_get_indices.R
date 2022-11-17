Sys.setenv(R_CONFIG_FILE = "/home/user/config.yml")

expect_inherits(get_indices(), "list")
