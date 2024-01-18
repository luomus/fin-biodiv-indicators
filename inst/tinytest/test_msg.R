expect_stdout(fbi:::err_msg(list(message = "message")), "^ERROR")

expect_stdout(fbi:::warn_msg(list(message = "message")), "^WARN")
