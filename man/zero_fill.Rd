% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_funs.R
\name{zero_fill}
\alias{zero_fill}
\title{Zero fill}
\usage{
zero_fill(counts, surveys, ...)
}
\arguments{
\item{counts}{Count data.}

\item{surveys}{Survey data.}

\item{...}{Additional arguments.}
}
\description{
Combine count data with survey data filling missing surveys in count data
with zero counts.
}
\details{
This function combines \code{counts} and \code{surveys} data. It performs a
right outer join of \code{counts} on \code{surveys} by \code{document_id}. Then all
surveys with no corresponding data for abundance are filled with zero. The
function assumes that both \code{counts} and \code{surveys} data include
\code{document_id} and that \code{counts} data includes \code{abundance}.
}
