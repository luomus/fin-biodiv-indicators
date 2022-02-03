#* @apiTitle Finnish Biodiversity Indicators HTTP API
#* @apiDescription Tracking biodiversity trends in Finland using single- and multi-species population index based indicators.
#* @apiTOS https://laji.fi/en/about/845
#* @apiContact list(name = "laji.fi support", email = "helpdesk@laji.fi")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")
#* @apiTag list Endpoints to list available indices.
#* @apiTag data Endpoints to get biodiversity index data in json or csv format.

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

#* Get list of available multi-species indices
#* @tag list
#* @get /indices
#* @response 200 A json array response
#* @serializer unboxedJSON
function() {

  config::get("indices")

}

#* Get list of species for an index
#* @tag list
#* @get /taxa/<index:str>
#* @param index:str Shortcode for multi-species index (see [/indices](#get-/indices)).
#* @response 200 A json array response
#* @serializer unboxedJSON
function(index) {

  vapply(config::get("taxa", config = index), getElement, "", "code")

}

#* Get list of extra species available but that are not included in the overall multi-species index
#* @tag list
#* @get /extra-taxa/<index:str>
#* @param index:str Shortcode for multi-species index (see [/indices](#get-/indices)).
#* @response 200 A json array response
#* @serializer unboxedJSON
function(index) {

  vapply(config::get("extra_taxa", config = index), getElement, "", "code")

}

#* Get data for an index
#* @tag data
#* @get /data/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param taxa:str Shortcode for taxa (see [/taxa](#get-/taxa)).
#* @response 200 A json array response
#* @serializer unboxedJSON
function(index, taxa = "none") {

  taxa <- switch(taxa, none = NULL, taxa)

  index <- paste(c(index, taxa), collapse = "_")

  ans <- dplyr::tbl(pool, "data")

  ans <- dplyr::filter(ans, .data[["index"]] == !!index)

  ans <- dplyr::select(ans, .data[["data"]])

  unserialize(dplyr::pull(ans)[[1L]])

}

#* Get svg for an index
#* @tag data
#* @get /svg/<index:str>
#* @param index:str Shortcode for index (see [/indices](#get-/indices)).
#* @param taxa:str Shortcode for taxa (see [/taxa](#get-/taxa)).
#* @response 200 An svg file response
function(index, taxa = "none", res) {

  res[["setHeader"]]("Content-Type", "image/svg+xml")
  res[["setHeader"]]("Content-Encoding", "gzip")
  res[["setHeader"]]("Content-Disposition", "inline")

  taxa <- switch(taxa, none = NULL, taxa)

  index <- paste(c(index, taxa), collapse = "_")

  ans <- dplyr::tbl(pool, "svg")

  ans <- dplyr::filter(ans, .data[["index"]] == !!index)

  ans <- dplyr::select(ans, .data[["data"]])

  res[["body"]] <- dplyr::pull(ans)[[1L]]

  res

}

#* @assets /usr/local/lib/R/site-library/finbif/help/figures
list()

#* @get /favicon.ico
#* @serializer contentType list(type="image/x-icon")
function() {

  readBin("favicon.ico", "raw", n = file.info("favicon.ico")$size)

}

#* @get /
function(res) {

  res[["status"]] <- 303L
  res[["setHeader"]]("Location", "/__docs__/")

}

#* @plumber
function(pr) {

  version <- as.character(utils::packageVersion("indicators"))

  plumber::pr_set_api_spec(
    pr,
    function(spec) {

      spec$info$version <- version

      spec$paths$`/healthz` <- NULL
      spec$paths$`/favicon.ico` <- NULL
      spec$paths$`/` <- NULL

      set_description <- function(spec, path, description) {
        spec$paths[[path]]$get$description <- description
        spec
      }

      spec <- set_description(
        spec, "/indices", "Gets a list of shortcodes for the available indices."
      )

      spec <- set_description(
        spec,
        "/taxa/{index}",
        paste(
          "Gets a list of taxon codes representing taxa that make up a",
          "given index<br>To list the available indices see",
          "[/indices](#get-/indices)."
        )
      )

      spec <- set_description(
        spec,
        "/data/{index}",
        paste(
          "Gets the time series data for a species or ",
          "multi-species index<br>To list species see [/taxa](#get-/taxa)."
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
    allow_spec_file_load = "false"
  )

}
