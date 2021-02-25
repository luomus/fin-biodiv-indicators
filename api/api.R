#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader(
      "Access-Control-Allow-Headers",
      req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )
    res$status <- 200L
    return(list())
  } else {
    plumber::forward()
  }
}

#* Return population estimate
#* @param year Year
#* @param sp Species
#* @get /pop
function(year, sp) {
  list(mean = 100, sd = 25)
}

#* Return indicator
#* @param year Year
#* @param type Indicator type
#* @get /ind
function(year, type) {
  list(mean = 100, sd = 25)
}

#* Return a graph
#* @param year Year range
#* @param type Indicator type
#* @get /graph
function(year = paste0("2000:", format(Sys.Date(), "%Y")), type, res) {
  res$body <- indicator_svg(year, type)
  res$setHeader("Content-Type", "image/svg+xml")
  res$setHeader("Content-Encoding", "gzip")
  res$setHeader("Content-Disposition", "inline")
  res
}
