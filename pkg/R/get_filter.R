get_filter <- function(index) {

  switch(
    index,
    wb = {
      begin_date <- "1958-12-01"
      list(
        date_range_ymd = list(begin_date, ""),
        date_range_md = c("12-01", "01-31"),
        collection = "Winter Bird Census",
        quality_issues = "both"
      )
    },
    bf = {
      begin_date <- "1999-05-16"
      list(
        date_range_ymd = list(begin_date, ""),
        date_range_md = c("05-16", "08-31"),
        collection = "HR.3431",
        quality_issues = "both"
      )
    }
  )

}
