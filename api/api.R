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

#* Return single species index as json
#* @param sp Species
#* @param year Year
#* @param base Base year of index
#* @get /sp-index/json
function(sp = "skylark", year, base) {
  sp_index(sp, year, base)
}

#* Return single species index as csv
#* @param sp Species
#* @param year Year
#* @param base Base year of index
#* @serializer csv
#* @get /sp-index/csv
function(sp = "skylark", year, base) {
  sp_index(sp, year, base)
}


#* Return a plot
#* @param sp Species
#* @param year Year
#* @param base Base year of index
#* @get /sp-plot
function(sp = "skylark", year, base, res) {

  res$setHeader("Content-Type", "image/svg+xml")
  res$setHeader("Content-Encoding", "gzip")
  res$setHeader("Content-Disposition", "inline")

  svg <- svg_index(sp, year, base)

  if (promises::is.promise(svg)) {

    res_body <- function(svg) {
      res$body <- svg
      res
    }

    promises::then(svg, res_body)

  } else {

    res$body <- svg
    res

  }

}
