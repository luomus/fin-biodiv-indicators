#' @importFrom lubridate day month today

filter_gen <- function(index) {

  switch(
    index,
    wb = {
      begin_date <- "1958-12-01"
      end_date <- lubridate::today()
      lubridate::month(end_date) <- 1L
      lubridate::day(end_date) <- 31L

      list(
        date_range_ymd = list(begin_date, end_date),
        date_range_md = c("12-01", "01-31"),
        collection = "Winter Bird Census",
        quality_issues = "both"
      )
    }
  )

}
