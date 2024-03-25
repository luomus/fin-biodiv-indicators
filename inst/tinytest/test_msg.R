Sys.setenv(DEBUG = "true")

expect_error(fbi:::err_msg(list(message = "message")))

Sys.unsetenv("DEBUG")

expect_stdout(fbi:::warn_msg(list(message = "message")), "^WARN")

expect_stdout(fbi:::msg_msg(list(message = "message")), "^WARN")
