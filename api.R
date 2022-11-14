#* @apiTitle Finnish Biodiversity Indicators HTTP API
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
#* @tag status
#* @head /healthz
#* @get /healthz
#* @response 200 A json object response
#* @serializer unboxedJSON
function() {

  ""

}

#* Get list of available indicators
#* @tag lists
#* @get /indices
#* @response 200 A json array response
#* @serializer unboxedJSON
function() {

  config::get("indices")

}

#* Get list of taxa available for an indicator
#* @tag lists
#* @get /taxa/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @response 200 A json array response
#* @response 404 Not Found
#* @serializer unboxedJSON
function(index, res) {

  indices <- vapply(config::get("indices"), getElement, "", "code")

  if (index %in% indices) {

    ans <- config::get("taxa", config = index)

  } else {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans

}

#* Get list of taxa available for an indicator (but not included in multi-taxon indicator)
#* @tag lists
#* @get /taxa-extra/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @response 200 A json array response
#* @response 404 Not Found
#* @serializer unboxedJSON
function(index, res) {

  indices <- vapply(config::get("indices"), getElement, "", "code")

  if (index %in% indices) {

    ans <- config::get("extra_taxa", config = index)

  } else {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans

}

#* Get the configuration for an indicator
#* @tag config
#* @get /config/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @response 200 A json array response
#* @response 404 Not Found
#* @serializer unboxedJSON
function(index, res) {

  indices <- vapply(config::get("indices"), getElement, "", "code")

  if (index %in% indices) {

    ans <- config::get(config = index)

    ans[["indices"]] <- NULL
    ans[["taxa"]] <- NULL
    ans[["extra_taxa"]] <- NULL

    if (hasName(ans, "from")) {

      ans[["surveys"]] <- NULL
      ans[["counts"]] <- NULL
      ans[["filters"]] <- NULL

    }

  } else {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans

}

#* Get data for an indicator as JSON
#* @tag data
#* @get /data/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @param model:str Which model one of `trim`, `rbms`, etc. (`default` is first model declared in [configuration](#get-/config/-index- "Get the configuration of an indicator")).
#* @param taxon:str FinBIF MX code identifier for a taxon (see [/taxa](#get-/taxa/-index- "Get list of taxa available for an indicator") or [/taxa-extra](#get-/taxa-extra/-index- "Get list of extra taxa available for an indicator")).
#* @param region:str Which region: `north`, `south` or `none` (whole of Finland)?
#* @response 200 A json object response
#* @response 404 Not found
#* @serializer unboxedJSON
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output || !region %in% c("none", "south", "north")) {

    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("data", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    ans <- unserialize(ans)

  } else {

    res[["status"]] <- 500L

  }

  ans

}

#* Get data for an indicator as CSV
#* @tag data
#* @get /csv/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @param model:str Which model one of `trim`, `rbms`, etc. (`default` is first model declared in [configuration](#get-/config/-index- "Get the configuration of an indicator")).
#* @param taxon:str FinBIF MX code identifier for a taxon (see [/taxa](#get-/taxa/-index- "Get list of taxa available for an indicator") or [/taxa-extra](#get-/taxa-extra/-index- "Get list of extra taxa available for an indicator")).
#* @param region:str Which region: `north`, `south` or `none` (whole of Finland)?
#* @response 200 A csv file response
#* @response 404 Not found
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output || !region %in% c("none", "south", "north")) {

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
    res[["status"]] <- 500L

  }

  ans

}

#* Get count summary for an indicator
#* @tag statistics
#* @get /count-summary/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @param model:str Which model one of `trim`, `rbms`, etc. (`default` is first model declared in [configuration](#get-/config/-index- "Get the configuration of an indicator")).
#* @param taxon:str FinBIF MX code identifier for a taxon (see [/taxa](#get-/taxa/-index- "Get list of taxa available for an indicator") or [/taxa-extra](#get-/taxa-extra/-index- "Get list of extra taxa available for an indicator")).
#* @param region:str Which region: `north`, `south` or `none` (whole of Finland)?
#* @response 200 A json object response
#* @response 404 Not found
#* @serializer unboxedJSON
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output || !region %in% c("none", "south", "north")) {

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

#* Get trend summary for an indicator
#* @tag statistics
#* @get /trends/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @param model:str Which model one of `trim`, `rbms`, etc. (`default` is first model declared in [configuration](#get-/config/-index- "Get the configuration of an indicator")).
#* @param taxon:str FinBIF MX code identifier for a taxon (see [/taxa](#get-/taxa/-index- "Get list of taxa available for an indicator") or [/taxa-extra](#get-/taxa-extra/-index- "Get list of extra taxa available for an indicator")).
#* @param region:str Which region: `north`, `south` or `none` (whole of Finland)?
#* @response 200 An json object response
#* @response 404 Not found
#* @serializer unboxedJSON
function(index, model = "default", taxon = "none", region = "none", res) {

  has_output <- check_input(index, model, taxon)

  if (!has_output || !region %in% c("none", "south", "north")) {

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

#* Get image for an indicator as SVG
#* @tag graphics
#* @get /svg/<index:str>
#* @param index:str Shortcode for multi-taxon indicator (see [/indices](#get-/indices "Get list of available indicators")).
#* @param model:str Which model one of `trim`, `rbms`, etc. (`default` is first model declared in [configuration](#get-/config/-index- "Get the configuration of an indicator")).
#* @param taxon:str FinBIF MX code identifier for a taxon (see [/taxa](#get-/taxa/-index- "Get list of taxa available for an indicator") or [/taxa-extra](#get-/taxa-extra/-index- "Get list of extra taxa available for an indicator")).
#* @param region:str Which region: `north`, `south` or `none` (whole of Finland)?
#* @param fontsize:double Font size (in px) of the axis labels.
#* @param scale:double Scale of image (%).
#* @response 200 An svg file response
#* @response 404 Not found
function(
  index, model = "default", taxon = "none", region = "none", fontsize = 8.80,
  scale = 100, res
) {

  has_output <- check_input(index, model, taxon)

  if (!has_output || !region %in% c("none", "south", "north")) {

    res[["serializer"]] <- plumber::serializer_unboxed_json()
    res[["status"]] <- 404L
    return("Not found")

  }

  ans <- get_output("svg", index, model, taxon, region, pool)

  if (is.raw(ans)) {

    if (fontsize != 8.8 || scale != 100) {

      con <- rawConnection(ans)
      on.exit(close(con))

      gcon <- gzcon(con)
      ans <- readBin(gcon, "raw", n = 100000)
      ans <- rawToChar(ans)

      fontsize <- as.numeric(fontsize)
      fontsize <- sprintf("font-size: %spx", fontsize)
      ans <- gsub("font-size: 8\\.80px", fontsize, ans)

      scale <- as.numeric(scale)
      width <- sprintf("width=\"%spt\"", round(720 * scale / 100, 2))
      height <- sprintf("height=\"%spt\"", round(576 * scale / 100, 2))
      ans <- gsub("width='720\\.00pt'", width, ans)
      ans <- gsub("height='576\\.00pt'", height, ans)

      tmp <- tempfile(fileext = ".svgz")
      writeLines(ans, tmp)
      svglite::create_svgz(tmp)
      ans <- readBin(tmp, "raw", n = file.info(tmp)[["size"]])
      on.exit(unlink(tmp), add = TRUE)

    }

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

  path <- "/docs/"

  if (length(unlist(packageVersion("fbi"))) > 3L) {

    path <- "/docs/dev/index.html"

  }

  res[["status"]] <- 303L
  res[["setHeader"]]("Location", path)

}

#* @get /docs/__docs__
function(res) {

  res[["status"]] <- 303L
  res[["setHeader"]]("Location", "/__docs__/#overview")

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

      spec$info$description <- readChar("api.md", file.info("api.md")$size)

      spec$info$contact$name <- "laji.fi support"
      spec$info$contact$email <- "helpdesk@laji.fi"

      spec$info$license$name <- "MIT"
      spec$info$license$url <- "https://opensource.org/licenses/MIT"

      spec$tags <- list(
        list(
          name = "lists",
          description = "Endpoints to list available biodiversity indicators"
        ),
        list(
          name = "config",
          description = paste(
            "Endpoints to show how multi-taxon biodiversity indicators are",
            "configured"
          )
        ),
        list(
          name = "data",
          description = "Endpoints to get biodiversity indicator output data"
        ),
        list(
          name = "statistics",
          description = "Endpoints to get biodiversity indicator statistics"
        ),
        list(
          name = "graphics",
          description = "Endpoints to get biodiversity indicator graphics"
        )
      )

      spec$paths$`/healthz` <- NULL
      spec$paths$`/favicon.ico` <- NULL
      spec$paths$`/robots.txt` <- NULL
      spec$paths$`/` <- NULL
      spec$paths$`/docs/__docs__` <- NULL

      set_description <- function(spec, path, description) {
        spec$paths[[path]]$get$description <- description
        spec
      }

      set_response_null <- function(spec, path) {
        spec$paths[[path]]$get$responses$default <- NULL
        spec$paths[[path]]$get$responses$`500`$content <- NULL
        spec$paths[[path]]$get$responses$`404`$content <- NULL
        spec
      }

      set_schema <- function(spec, path, resp, schema, example) {
        content <- spec$paths[[path]]$get$responses[[resp]]$content
        content$`application/json`$schema <- schema
        content$`application/json`$example <- example
        spec$paths[[path]]$get$responses[[resp]]$content <- content
        spec
      }

      set_example <- function(spec, path, arg, example) {
        spec$paths[[path]]$get$parameters[[arg]]$example <- example
        spec
      }

      spec <- set_description(
        spec,
        "/indices",
        paste(
          "Gets a list of the available multi-taxon indicators. Returns both",
          "indicator names and short-codes that can be used as indicator",
          "identifiers for other endpoints."
        )
      )

      spec <- set_response_null(spec, "/indices")

      spec <- set_schema(
        spec,
        "/indices",
        "200",
        list(
          type = "array",
          items = list(
            type = "object",
            required = c("code", "name"),
            properties = list(
              code = list(
                type = "string",
                description = "Multi-taxon indicator shortcode."
              ),
              name = list(
                type = "string", description = "Multi-taxon indicator name."
              )
            )
          )
        ),
        data.frame(code = c("aa", "bb"), name = c("Indicator A", "Indicator B"))
      )

      spec <- set_description(
        spec,
        "/taxa/{index}",
        paste(
          "Gets the taxa that are available as single-taxon indices and that",
          "are included in a given multi-taxon indicator. To list the",
          "available indicators see [/indices](#get-/indices \"Get list of",
          "available indicators\")."
        )
      )

      spec <- set_example(spec, "/taxa/{index}", 1, "wb")

      spec <- set_response_null(spec, "/taxa/{index}")

      spec <- set_schema(
        spec,
        "/taxa/{index}",
        "200",
        list(
          type = "array",
          items = list(
            type = "object",
            required = c("code", "binomial"),
            properties = list(
              code = list(
                type = "string",
                description = "FinBIF taxon MX code identifier."
              ),
              extra_codes = list(
                type = "array",
                description = paste(
                  "Extra FinBIF taxon MX code identifiers. These extra taxa",
                  "will also be included in the single-taxon indicator."
                )
              ),
              subtaxa = list(
                type = "boolean",
                description = paste(
                  "Whether the subtaxa of the taxa or extra taxa (see",
                  "extra_codes) are included in the single-taxon indicator."
                )
              ),
              binomial = list(
                type = "string",
                description = "Scientific name of the taxon."
              ),
              start_year  = list(
                type = "integer",
                description = paste(
                  "The taxon-specific base year for calucating relative",
                  "abundance."
                )
              ),
              sti = list(
                type = "number",
                description = "Species temperature index."
              )
            )
          )
        ),
        list(
          list(
            code = "MX.123",
            extra_codes = c("MX.12", "MX.34"),
            subtaxa = FALSE,
            binomial = "Genus speciesa"
          ),
          list(
            code = "MX.456",
            binomial = "Genus speicesb",
            sti = 7.1
          )
        )
      )

      spec <- set_description(
        spec,
        "/taxa-extra/{index}",
        paste(
          "Gets the taxa that are available as single-taxon indices but",
          "are not included in a given multi-taxon indicator. To list the",
          "available indicators see",
          "[/indices](#get-/indices \"Get list of available indicators\")."
        )
      )

      spec <- set_example(spec, "/taxa-extra/{index}", 1, "wb")

      spec <- set_response_null(spec, "/taxa-extra/{index}")

      spec <- set_schema(
        spec,
        "/taxa-extra/{index}",
        "200",
        list(
          type = "array",
          items = list(
            type = "object",
            required = c("code", "binomial"),
            properties = list(
              code = list(
                type = "string",
                description = "FinBIF taxon MX code identifier."
              ),
              extra_codes = list(
                type = "array",
                description = paste(
                  "Extra FinBIF taxon MX code identifiers. These extra taxa",
                  "will also be included in the single-taxon indicator."
                )
              ),
              subtaxa = list(
                type = "boolean",
                description = paste(
                  "Whether the subtaxa of the taxa or extra taxa (see",
                  "extra_codes) are included in the single-taxon indicator."
                )
              ),
              binomial = list(
                type = "string",
                description = "Scientific name of the taxon."
              ),
              start_year  = list(
                type = "integer",
                description = paste(
                  "The taxon-specific base year for calucating relative",
                  "abundance."
                )
              ),
              sti = list(
                type = "number",
                description = "Species temperature index."
              )
            )
          )
        ),
        list(
          list(
            code = "MX.123",
            extra_codes = c("MX.12", "MX.34"),
            subtaxa = FALSE,
            binomial = "Genus speciesa"
          ),
          list(
            code = "MX.456",
            binomial = "Genus speicesb",
            sti = 7.1
          )
        )
      )

      spec <- set_description(
        spec,
        "/config/{index}",
        paste(
          "Gets the configuration of a multi-taxon indicator and the",
          "single-taxon indicators (if any) that belong to it."
        )
      )

      spec <- set_example(spec, "/config/{index}", 1, "wb")

      spec <- set_response_null(spec, "/config/{index}")

      spec <- set_schema(
        spec,
        "/config/{index}",
        "200",
        list(
          type = "object",
          required = c("combine", "use_data_after", "model"),
          properties = list(
            surveys = list(
              type = "object",
              description = paste(
                "Configuration for the survey site data of a multi-taxon",
                "indicator and the single-taxon indicators that belong to it."
              )
            ),
            counts = list(
              type = "object",
              description = paste(
                "Configuration for the abundance data of a multi-taxon",
                "indicator and the single-taxon indicators that belong to it."
              )
            ),
            filters = list(
              type = "object",
              description = paste(
                "The filters applied to the survey site and abundance data",
                "of a multi-taxon indicator and the single-taxon indicators",
                "that belong to it."
              )
            ),
            from = list(
              type = "string",
              description = paste(
                "The multi-taxon indicator from which the input data is",
                "derived for a multi-taxon indicator without any single-taxon",
                "indicators."
              )
            ),
            combine = list(
              type = "string",
              description = paste(
                "The method by which single-taxa data is combined to create a",
                "a multi-taxon indicator."
              )
            ),
            use_data_after  = list(
              type = "string",
              description = paste(
                "The date after which the current year's data is allowed to",
                "contribute to the indicator time-series. Format is 'MM-DD'."
              )
            ),
            model = list(
              type = "object",
              description = paste(
                "The configuration for the model(s) used to create the",
                "indicators."
              )
            )
          )
        ),
        list(
          surveys = list(
            selection = c("document_id", "location_id", "year", "month", "day"),
            has_value = c("document_id", "location_id", "year", "month", "day")
          ),
          counts = list(
            abundance = "pair_abundance",
            selection = c("document_id", "pair_abundance"),
            has_value = c("document_id", "pair_abundance")
          ),
          filters = list(
            date_range_ymd = c("1979-01-01", ""),
            location_tag = "farmland",
            collection = c("HR.157", "HR.61")
          ),
          combine = "geometric_mean",
          use_date_after = "10-01",
          model = list(
            trim = list(
              base_year = 2000L,
              surveys_process = c(
                "pick_first_survey_in_year", "require_two_years"
              ),
              counts_process = c(
                "zero_fill", "sum_by_event", "set_start_year",
                "remove_all_zero_locations"
              )
            )
          )
        )
      )

      spec <- set_description(
        spec,
        "/data/{index}",
        paste(
          "Gets the time series data for a single-taxon or ",
          "multi-taxon indicator as JSON."
        )
      )

      spec <- set_example(spec, "/data/{index}", 1, "wb")
      spec <- set_example(spec, "/data/{index}", 2, "trim")
      spec <- set_example(spec, "/data/{index}", 3, "none")
      spec <- set_example(spec, "/data/{index}", 4, "none")

      spec <- set_response_null(spec, "/data/{index}")

      spec <- set_schema(
        spec,
        "/data/{index}",
        "200",
        list(
          type = "object",
          required = c(
            "data", "pointStart", "pointInterval", "pointIntervalUnit"
          ),
          properties = list(
            data = list(
              type = "array",
              description = paste(
                "Indicator values in triplets with the first value the mean",
                "estimate and the following two values representing",
                "uncertainty of the indicator as lower and upper bounds -1 and",
                "+1 standard deviation from the mean value respectively."
              )
            ),
            pointStart = list(
              type = "integer",
              description = paste(
                "The starting point of the time-series of indicator values."
              )
            ),
            pointInterval = list(
              type = "integer",
              description = paste(
                "The interval length between indicator time-series values."
              )
            ),
            pointIntervalUnit = list(
              type = "string",
              description = paste(
                "The units of interval length between indicator time-series",
                "values."
              )
            )
          )
        ),
        list(
          data = list(c(1, 0, 0), c(.8, .6, 1)),
          pointStart = 2000L,
          pointInterval = 1L,
          pointIntervalUnit = "year"
        )
      )

      spec <- set_description(
        spec,
        "/csv/{index}",
        paste(
          "Gets the time series data for a single-taxon or multi-taxon",
          "indicator as CSV. The CSV fields include: `time` (the",
          "year), `mean` (the point estimate of the indicator), `sd` (the",
          "degree of uncertainty in the indicator estimate as standard",
          "deviations), `lower` (-1 standard deviation from the mean estimate)",
          "and `upper` (+1 standard deviation from the mean estimate)."
        )
      )

      spec <- set_response_null(spec, "/csv/{index}")

      spec <- set_example(spec, "/csv/{index}", 1, "wb")
      spec <- set_example(spec, "/csv/{index}", 2, "trim")
      spec <- set_example(spec, "/csv/{index}", 3, "none")
      spec <- set_example(spec, "/csv/{index}", 4, "none")

      spec <- set_description(
        spec,
        "/count-summary/{index}",
        paste(
          "Gets a summary of the count data used as input for an indicator.",
          "Summary depends on the type of indicator."
        )
      )

      spec <- set_response_null(spec, "/count-summary/{index}")

      spec <- set_schema(
        spec,
        "/count-summary/{index}",
        "200",
        list(
          type = "object",
          properties = list(
            taxa = list(
              type = "integer",
              description = paste(
                "The number of taxa included in a multi-taxon indicator."
              )
            ),
            sites = list(
              type = "integer",
              description = paste(
                "The number of sites included in a single-taxon indicator."
              )
            ),
            zero_counts = list(
              type = "integer",
              description = paste(
                "The number of zero count observations for a single-taxon",
                "indicator."
              )
            ),
            positive_counts = list(
              type = "integer",
              description = paste(
                "The number of positive count observations for a single-taxon",
                "indicator."
              )
            ),
            total_observed = list(
              type = "integer",
              description = paste(
                "The number of count observations for a single-taxon",
                "indicator."
              )
            ),
            missing_counts = list(
              type = "integer",
              description = paste(
                "The number of missing count observations for a single-taxon",
                "indicator."
              )
            ),
            total_counts = list(
              type = "integer",
              description = paste(
                "The number of observation events for a single-taxon",
                "indicator."
              )
            )
          )
        ),
        list(
          sites = 110L,
          zero_counts = 31743L,
          positive_counts = 3437L,
          total_observed = 35180L,
          missing_counts = 22473L,
          total_counts = 57653L
        )
      )

      spec <- set_example(spec, "/count-summary/{index}", 1, "wb")
      spec <- set_example(spec, "/count-summary/{index}", 2, "trim")
      spec <- set_example(spec, "/count-summary/{index}", 3, "none")
      spec <- set_example(spec, "/count-summary/{index}", 4, "none")

      spec <- set_description(
        spec,
        "/trends/{index}",
        paste(
          "Gets a summary of overall trend for an indicator. The overall trend",
          "summary is only available for single-taxon indicators calculated,
          using rtrim."
        )
      )

      spec <- set_response_null(spec, "/trends/{index}")

      spec <- set_schema(
        spec,
        "/trends/{index}",
        "200",
        list(
          type = "object",
          properties = list(
            from = list(
              type = "integer",
              description = "The starting year of the overall trend."
            ),
            upto = list(
              type = "integer",
              description = "The ending year of the overall trend."
            ),
            add = list(
              type = "number",
              description = paste(
                "The mean of the overall trend on the additive scale."
              )
            ),
            se_add = list(
              type = "number",
              description = paste(
                "The standard error of the mean of the overall trend on the",
                "additive scale."
              )
            ),
            mul = list(
              type = "number",
              description = paste(
                "The mean of the overall trend on the multiplicative scale."
              )
            ),
            se_mul = list(
              type = "number",
              description = paste(
                "The standard error of the mean of the overall trend on the",
                "multiplicative scale."
              )
            ),
            p = list(
              type = "number",
              description = paste(
                "The p-value of the overall trend on the multiplicative scale."
              )
            ),
            meanning = list(
              type = "string",
              description = paste(
                "An intepretation of the trend accounting for the sign of the",
                "trend and its statistical significance."
              )
            )
          )
        ),
        list(
          from = 2000L,
          upto = 2022L,
          add = 0.1503,
          se_add = 0.0297,
          mul = 1.1622,
          se_mul = 0.0345,
          p = 0.0001,
          meaning = "Strong increase (p<0.005)"
        )
      )

      spec <- set_example(spec, "/trends/{index}", 1, "wb")
      spec <- set_example(spec, "/trends/{index}", 2, "trim")
      spec <- set_example(spec, "/trends/{index}", 3, "none")
      spec <- set_example(spec, "/trends/{index}", 4, "none")

      spec <- set_description(
        spec,
        "/svg/{index}",
        paste(
          "Gets an svg image for an indicator."
        )
      )

      spec <- set_response_null(spec, "/svg/{index}")

      spec <- set_example(spec, "/svg/{index}", 1, "wb")
      spec <- set_example(spec, "/svg/{index}", 2, "trim")
      spec <- set_example(spec, "/svg/{index}", 3, "none")
      spec <- set_example(spec, "/svg/{index}", 4, "none")
      spec <- set_example(spec, "/svg/{index}", 5, 8.8)
      spec <- set_example(spec, "/svg/{index}", 6, 100)

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
    goto_path = "overview",
    update_route = "false",
    allow_authentication = "false",
    allow_server_selection = "false"
  )

}
