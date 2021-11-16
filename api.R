#* @apiTitle Finnish Biodiversity Indicators HTTP API
#* @apiDescription Tracking biodiversity trends in Finland using single- and multi-species population index based indicators.
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiVersion 0.1.0.9002
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")
#* @apiTag lists Endpoints to list available indices.
#* @apiTag indices Endpoints to get biodiversity index data in json or csv format.
#* @apiTag plots Endpoints to get plots of biodiversity indices in SVG image format.

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

#* Get list of available multi-species indices
#* Gets a list of shortcodes for the multi-species indices available.
#* The list is returned in boxed JSON format.
#* @tag lists
#* @get /list/indices
#* @response 200 A json array response
function(req) {

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for list of indices")

  indices()

}

#* Get list of species for an index
#* Gets a list of species codes representing species that make up a given
#* multi-species index. The list is returned in boxed JSON format.<br>To list
#* the multi-species indices available see
#* [/list/indices](#/lists/get_list_indices).
#* @tag lists
#* @get /list/spp
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @response 200 A json array response
function(index, req) {

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for list of species from ", index)

  species(index, "spcode")

}

#* Get data for a multi-species index as JSON
#* Gets the time series data for a given multi-species index in boxed JSON
#* format.<br>To list the multi-species indices available see
#* [/list/indices](#/lists/get_list_indices).
#* @tag indices
#* @get /ms-index/json
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @param cache:bool Whether or not to use cached data. Cached data is used unless the cache does not exist or is invalid.<br>Using `cache=false` will check the cache validity but is rate limited to one check per day.
#* @response 200 A json array response
function(index, cache = "true", req) {

  cache <- match.arg(cache, c("true", "false"))
  use_cache <- switch(cache, true = TRUE, false = FALSE)

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for multi-species index of ", index)

  ms_index(index, use_cache, id)

}

#* Get data for a multi-species index as a CSV file
#* Gets the time series data for a given multi-species index as a CSV file.
#* <br>To list the multi-species indices available see
#* [/list/indices](#/lists/get_list_indices).
#* @tag indices
#* @get /ms-index/csv
#* @serializer csv
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @param cache:bool Whether or not to use cached data. Cached data is used unless the cache does not exist or is invalid.<br>Using `cache=false` will check the cache validity but is rate limited to one check per day.
#* @response 200 A csv file response
function(index, cache = "true", req) {

  cache <- match.arg(cache, c("true", "false"))
  use_cache <- switch(cache, true = TRUE, false = FALSE)

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "CSV request made for multi-species index of ", index)

  ms_index(index, use_cache, id)

}

#* Get data for a single-species index as JSON
#* Gets the time series data for a given species from a given multi-species
#* index in boxed JSON format<br>To list species see
#* [/list/spp](#/lists/get_list_spp).
#* @tag indices
#* @get /sp-index/json
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @param sp Shortcode for species (see [/list/spp](#/lists/get_list_spp)).
#* @param cache:bool Whether or not to use cached data. Cached data is used unless the cache does not exist or is invalid.<br>Using `cache=false` will check the cache validity but is rate limited to one check per day.
#* @response 200 A json array response
function(index, sp, cache = "true", req) {

  cache <- match.arg(cache, c("true", "false"))
  use_cache <- switch(cache, true = TRUE, false = FALSE)

  sp <- check_sp(index, sp)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "JSON request made for index of ", sp, " from ", index)

  sp_index(index, sp, use_cache, id)

}

#* Get data for a single-species index as CSV file
#* Gets the time series data for a given species from a given multi-species
#* index as a CSV file.<br>To list species see
#* [/list/spp](#/lists/get_list_spp).
#* @tag indices
#* @get /sp-index/csv
#* @serializer csv
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @param sp Shortcode for species (see [/list/spp](#/lists/get_list_spp)).
#* @param cache:bool Whether or not to use cached data. Cached data is used unless the cache does not exist or is invalid.<br>Using `cache=false` will check the cache validity but is rate limited to one check per day.
#* @response 200 A csv file response
function(index, sp, cache = "true", req) {

  cache <- match.arg(cache, c("true", "false"))
  use_cache <- switch(cache, true = TRUE, false = FALSE)

  sp <- check_sp(index, sp)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "CSV request made for index of ", sp, " from ", index)

  sp_index(index, sp, use_cache, id)

}

#* Get a plot of a multi-species index
#* Gets a time-series plot of data for a given multi-species index as an SVG
#* image.<br>To list the multi-species indices available see
#* [/list/indices](#/lists/get_list_indices).
#* @tag plots
#* @get /ms-plot
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @param cache:bool Whether or not to use cached data. Cached data is used unless the cache does not exist or is invalid.<br>Using `cache=false` will check the cache validity but is rate limited to one check per day.
#* @response 200 An svg file response
function(index, cache = "true", res, req) {

  cache <- match.arg(cache, c("true", "false"))
  use_cache <- switch(cache, true = TRUE, false = FALSE)

  index <- check_index(index)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "Request made for plot of ", index)

  res$setHeader("Content-Type", "image/svg+xml")
  res$setHeader("Content-Encoding", "gzip")
  res$setHeader("Content-Disposition", "inline")

  svg <- svg_ms_index(index, use_cache, id)

  promises::then(svg, ~{
    res$body <- .
    res
  })

}

#* Get a plot of a single-species index
#* Gets a time-series plot of data for a given species from a given
#* multi-species index as an SVG image.<br>To list species see
#* [/list/spp](#/lists/get_list_spp).
#* @tag plots
#* @get /sp-plot
#* @param index Shortcode for multi-species index (see [/list/indices](#/lists/get_list_indices)).
#* @param sp Shortcode for species (see [/list/spp](#/lists/get_list_spp)).
#* @param cache:bool Whether or not to use cached data. Cached data is used unless the cache does not exist or is invalid.<br>Using `cache=false` will check the cache validity but is rate limited to one check per day.
#* @response 200 An svg file response
function(index, sp, cache = "true", res, req) {

  cache <- match.arg(cache, c("true", "false"))
  use_cache <- switch(cache, true = TRUE, false = FALSE)

  sp <- check_sp(index, sp)

  id <- digest::digest(req)

  log_message(id, "===New Request===")
  log_message(id, "Request made for plot of ", sp, " from ", index)

  res$setHeader("Content-Type", "image/svg+xml")
  res$setHeader("Content-Encoding", "gzip")
  res$setHeader("Content-Disposition", "inline")

  svg <- svg_sp_index(index, sp, use_cache, id)

  promises::then(svg, ~{
    res$body <- .
    res
  })

}

#* @plumber
function(pr) {
  pr_set_api_spec(
    pr,
    function(spec) {

      set_200_only <- function(path) {
        spec$paths[[path]]$get$responses$`500` <<- NULL
        spec$paths[[path]]$get$responses$default <<- NULL
      }

      set_example <- function(
        path, example, status = "200", type = "application/json"
      ) {
        spec$paths[[path]]$get$responses[[status]]$content[[type]]$schema <<- NULL
        spec$paths[[path]]$get$responses[[status]]$content[[type]]$example <<- example
      }

      example_json <- list(
        list(year = 1990, index = 1, sd = 0),
        list(year = 1991, index = 1.1, sd = .1),
        list(year = 1992, index = 1.2, sd = .3)
      )

      example_csv <- "year,index,sd\n1990,1,0\n1991,1.1,0.1\n1992,1.2,0.3"

      example_svg <- readr::read_file(
        readr::read_file("/indicators/man/figures/graph.svg")
      )

      set_200_only("/list/indices")
      set_example("/list/indices", c("index1", "index2"))

      set_200_only("/list/spp")
      set_example("/list/spp", c("sp1", "sp2", "sp3"))

      set_200_only("/ms-index/json")
      set_example("/ms-index/json", example_json)

      set_200_only("/ms-index/csv")
      set_example("/ms-index/csv", type = "text/csv", example_csv)

      set_200_only("/sp-index/json")
      set_example("/sp-index/json", example_json)

      set_200_only("/sp-index/csv")
      set_example("/sp-index/csv", type = "text/csv", example_csv)

      set_200_only("/ms-plot")
      set_example("/ms-plot", type = "image/svg+xml", example_svg)

      set_200_only("/sp-plot")
      set_example("/sp-plot", type = "image/svg+xml", example_svg)

      spec

    }
  )
}
