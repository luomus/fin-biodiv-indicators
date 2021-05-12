#* @apiTitle Finnish Biodiversity Indicators
#* @apiDescription Tracking biodiversity trends in Finland
#* @apiVersion 0.1.0.9001
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")
#* @apiTag Indices Get a biodiversity index
#* @apiTag Plots Get a plot of a biodiversity index
#* @apiTag Lists List available indices

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

#* Return list of available indices
#* @tag Lists
#* @get /list/indices
function(req) {

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for list of indices")

  indices()

}

#* Return list of species for an index
#* @param index Which index to return
#* @tag Lists
#* @get /list/sp
function(index, req) {

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for list of species from ", index)

  species(index, "spcode")

}

#* Return multi-species index as json
#* @param index Which index to return
#* @tag Indices
#* @get /ms-index/json
function(index, req) {

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for multi-species index of ", index)

  ms_index(index, id)

}

#* Return multi-species index as csv
#* @param index Which index to return
#* @serializer csv
#* @tag Indices
#* @get /ms-index/csv
function(index, req) {

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "CSV request made for multi-species index of ", index)

  ms_index(index, id)

}

#* Return single-species index as json
#* @param index Which index to return
#* @param sp Species
#* @tag Indices
#* @get /sp-index/json
function(index, sp, req) {

  index <- check_index(index)
  sp <- check_sp(sp)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for index of ", sp, " from ", index)

  sp_index(index, sp, id)

}

#* Return single species index as csv
#* @param index Which index to return
#* @param sp Species
#* @serializer csv
#* @tag Indices
#* @get /sp-index/csv
function(index, sp, req) {

  index <- check_index(index)
  sp <- check_sp(sp)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "CSV request made for index of ", sp, " from ", index)

  sp_index(index, sp, id)

}

#* Return a multi-species index plot
#* @param index Which index to return
#* @tag Plots
#* @get /ms-plot
function(index, res, req) {

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "Request made for plot of ", index)

  res$setHeader("Content-Type", "image/svg+xml")
  res$setHeader("Content-Encoding", "gzip")
  res$setHeader("Content-Disposition", "inline")

  svg <- svg_ms_index(index, id)

  promises::then(svg, ~{
    res$body <- .
    res
  })

}

#* Return single-species index plot
#* @param index Which index to return
#* @param sp Species
#* @tag Plots
#* @get /sp-plot
function(index, sp, res, req) {

  index <- check_index(index)
  sp <- check_sp(sp)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "Request made for plot of ", sp, " from ", index)

  res$setHeader("Content-Type", "image/svg+xml")
  res$setHeader("Content-Encoding", "gzip")
  res$setHeader("Content-Disposition", "inline")

  svg <- svg_sp_index(index, sp, id)

  promises::then(svg, ~{
    res$body <- .
    res
  })

}
