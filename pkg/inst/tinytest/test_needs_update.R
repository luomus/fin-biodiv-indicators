Sys.setenv(CHECK_UPDATE = "false")

expect_false(fbi:::needs_update())
