% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_funs.R
\name{pick_first_survey_in_fortnight}
\alias{pick_first_survey_in_fortnight}
\title{Pick first survey in fortnight}
\usage{
pick_first_survey_in_fortnight(surveys, ...)
}
\arguments{
\item{surveys}{Survey data.}

\item{...}{Additional arguments.}
}
\description{
Pick first survey in each fortnight discarding subsequent surveys
}
\details{
This function groups surveys by \code{location_id}, \code{year} and
\code{fortnight}then orders them by date. All but the first survey in each group
is removed. If two or more surveys share the same date and \code{location_id}
then one is picked at random and the rest are removed. The function assumes
that the \code{surveys} data includes \code{day}, and \code{year} (as integers) and
\code{location_id}, and has been processed by the function
\code{require_seven_fortnights}.
}
