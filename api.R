#* @apiTitle Finnish Biodiversity Indicators HTTP API
#* @apiDescription Finnish biodiversity indicators is a service providing time series of abundance indices and related metrics for Finland. The input data for the indices are provided by the [Finnish Biodiversity Information Facility](https://laji.fi "FinBIF").
#* @apiTOS https://laji.fi/en/about/845

#* @filter cors
cors <- function(req, res) {

  res[["setHeader"]]("Access-Control-Allow-Origin", "*")

  if (req[["REQUEST_METHOD"]] == "OPTIONS") {

    res[["setHeader"]]("Access-Control-Allow-Methods", "*")

    res[["setHeader"]](
      "Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )

    res[["status"]] <- 200L

    return(list())

  } else {

    plumber::forward()

  }

}

#* Check the liveness of the API
#* @head /healthz
#* @get /healthz
#* @tag status
#* @response 200 A json object
#* @serializer unboxedJSON
function() {

  ""

}

#* Get list of available indices
#* @tag list
#* @get /indices
#* @response 200 A json array response
#* @serializer unboxedJSON
function() {

  config::get("indices")

}

#* Get list of taxa available for an index
#* @tag list
#* @get /taxa/<index:str>
#* @param index:str Shortcode for multi-taxa index (see [/indices](#get-/indices)).
#* @response 200 A json array
#* @serializer unboxedJSON
function(index) {

  config::get("taxa", config = index)

}

#* Get list of taxa available for but not included in index
#* @tag list
#* @get /taxa-extra/<index:str>
#* @param index:str Shortcode for multi-taxa index (see [/indices](#get-/indices)).
#* @response 200 A json array
#* @serializer unboxedJSON
function(index) {

  config::get("extra_taxa", config = index)

}

#* Get data for an index
#* @tag data
#* @get /data/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param model:str Which model (trim, rbms, etc.)?
#* @param taxon:str Shortcode for a taxon (see [/taxa](#get-/taxa)).
#* @param region:str Which region, north, south or none (whole of Finland: default)?
#* @response 200 A json object
#* @response 400 A json object
#* @response 404 A json object
#* @serializer unboxedJSON
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output) {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("data", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    ans <- unserialize(ans)

  } else {

    res[["status"]] <- 400L

  }

  ans

}

#* Get data for an index as a CSV
#* @tag data
#* @get /csv/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param model:str Which model (trim, rbms, etc.)?
#* @param taxon:str Shortcode for taxon (see [/taxa](#get-/taxa)).
#* @param region:str Which region, north, south or none (whole of Finland: default)?
#* @response 200 A csv file
#* @response 400 A json object
#* @response 404 A json object
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output) {

    res[["serializer"]] <- plumber::serializer_unboxed_json()
    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("data_csv", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    res[["serializer"]] <- plumber::serializer_csv()
    ans <- unserialize(ans)

  } else {

    res[["serializer"]] <- plumber::serializer_unboxed_json()
    res[["status"]] <- 400L

  }

  ans

}

#* Get count summary for an index
#* @tag data
#* @get /count-summary/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param model:str Which model (trim, rbms, etc.)?
#* @param taxon:str Shortcode for taxon (see [/taxa](#get-/taxa)).
#* @param region:str Which region, north, south or none (whole of Finland: default)?
#* @response 200 A json object
#* @response 400 A json object
#* @response 404 A json object
#* @serializer unboxedJSON
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output) {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("count_summary", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    ans <- unserialize(ans)

  } else {

    res[["status"]] <- 400L

  }

  ans

}

#* Get trend summary for an index
#* @tag data
#* @get /trends/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param model:str Which model (trim, rbms, etc.)?
#* @param taxon:str Shortcode for taxon (see [/taxa](#get-/taxa)).
#* @param region:str Which region, north, south or none (whole of Finland: default)?
#* @response 200 A json object
#* @response 400 A json object
#* @response 404 A json object
#* @serializer unboxedJSON
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output) {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("trends", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    ans <- unserialize(ans)

  } else {

    res[["status"]] <- 400L

  }

  ans

}

#* Get svg for an index
#* @tag graphics
#* @get /svg/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param model:str Which model (trim, rbms, etc.)?
#* @param taxon:str Shortcode for taxon (see [/taxa](#get-/taxa)).
#* @param region:str Which region, north, south or none (whole of Finland: default)?
#* @response 200 An svg file
#* @response 400 A json object
#* @response 404 A json object
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output) {

    res[["serializer"]] <- plumber::serializer_unboxed_json()
    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("svg", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    res[["setHeader"]]("Content-Type", "image/svg+xml")
    res[["setHeader"]]("Content-Encoding", "gzip")
    res[["setHeader"]]("Content-Disposition", "inline")

    res[["body"]] <- ans

    res

  } else {

    res[["serializer"]] <- plumber::serializer_unboxed_json()
    res[["status"]] <- 400L
    ans

  }

}

#* @assets /usr/local/lib/R/site-library/finbif/help/figures
list()

#* @get /favicon.ico
#* @serializer contentType list(type="image/x-icon")
function() {

  readBin("favicon.ico", "raw", n = file.info("favicon.ico")$size)

}

#* @get /robots.txt
#* @serializer contentType list(type="text/plain")
function() {

  readBin("robots.txt", "raw", n = file.info("robots.txt")$size)

}

#* @get /
function(res) {

  res[["status"]] <- 303L
  res[["setHeader"]]("Location", "/docs/")

}

#* @assets ./fbi/docs /docs
list()

#* @plumber
function(pr) {

  version <- as.character(utils::packageVersion("fbi"))

  plumber::pr_set_api_spec(
    pr,
    function(spec) {

      spec$info$version <- version

      spec$info$contact$name <- "laji.fi support"
      spec$info$contact$email <- "helpdesk@laji.fi"

      spec$info$license$name <- "MIT"
      spec$info$license$url <- "https://opensource.org/licenses/MIT"

      spec$tags <- list(
        list(
          name = "list",
          description = "Endpoints to list available indices"
        ),
        list(
          name = "data",
          description = "Endpoints to get biodiversity index data"
        ),
        list(
          name = "graphics",
          description = "Endpoints to get biodiversity index graphics"
        )
      )

      spec$paths$`/healthz` <- NULL
      spec$paths$`/favicon.ico` <- NULL
      spec$paths$`/robots.txt` <- NULL
      spec$paths$`/` <- NULL

      set_description <- function(spec, path, description) {
        spec$paths[[path]]$get$description <- description
        spec
      }

      spec <- set_description(
        spec, "/indices", "Gets the available indices."
      )

      spec <- set_description(
        spec,
        "/taxa/{index}",
        paste(
          "Gets the taxa that make up a",
          "given index<br>To list the available indices see",
          "[/indices](#get-/indices)."
        )
      )

      spec <- set_description(
        spec,
        "/taxa-extra/{index}",
        paste(
          "Gets the taxa available for but not included in a",
          "given index<br>To list the available indices see",
          "[/indices](#get-/indices)."
        )
      )

      spec <- set_description(
        spec,
        "/data/{index}",
        paste(
          "Gets the time series data for a taxa or ",
          "multi-taxa index as JSON."
        )
      )

      spec <- set_description(
        spec,
        "/csv/{index}",
        paste(
          "Gets the time series data for a taxa or ",
          "multi-taxa index as a CSV."
        )
      )

      spec <- set_description(
        spec,
        "/count-summary/{index}",
        paste(
          "Gets a summary of the count data used as input for an index."
        )
      )

      spec <- set_description(
        spec,
        "/trends/{index}",
        paste(
          "Gets a summary of the trends for index."
        )
      )

      spec <- set_description(
        spec,
        "/svg/{index}",
        paste(
          "Gets an svg image for an index."
        )
      )

      spec

    }
  )

  pr$setDocs(
    "rapidoc",
    bg_color = "#2691d9",
    text_color = "#ffffff",
    primary_color = "#2c3e50",
    render_style = "read",
    slots = paste0(
      '<img ',
      'slot="logo" ',
      'src="../public/logo.png" ',
      'width=36px style=\"margin-left:7px\"/>'
    ),
    heading_text = paste("FBI", version),
    regular_font = "Roboto, Helvetica Neue, Helvetica, Arial, sans-serif",
    font_size = "largest",
    sort_tags = "false",
    sort_endpoints_by = "summary",
    allow_spec_file_load = "false",
    goto_path = "get-/indices",
    update_route = "false",
    allow_authentication = "false",
    allow_server_selection = "false"
  )

}
