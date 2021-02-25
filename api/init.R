remotes::install_local("indicators", dependencies = FALSE, upgrade = "never")

library("indicators")

create_db_table(
  "pop",
  year = integer(),
  sp = character(),
  mean = numeric(),
  sd = numeric()
)

create_db_table(
  "ind",
  year = integer(),
  type = character(),
  mean = numeric(),
  sd = numeric()
)

create_db_table(
  "graph",
  year = character(),
  type = character(),
  svg = list()
)

library("plumber")
p <- plumb("api.R")
p$run(host = "0.0.0.0", port = 8000)
