expect_equal(
  indicators:::window_arrange.default(data.frame(a = 3:1), a),
  data.frame(a = 1:3)
)
