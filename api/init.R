remotes::install_local("indicators", dependencies = FALSE, upgrade = "never")

library("indicators")

create_cache()

library("plumber")
p <- plumb("api.R")
p$run(host = "0.0.0.0", port = 8000L)
