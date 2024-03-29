% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_funs.R
\name{combine_with_surveys}
\alias{combine_with_surveys}
\title{Combine with surveys}
\usage{
combine_with_surveys(counts, surveys, ...)
}
\arguments{
\item{counts}{Count data.}

\item{surveys}{Survey data.}

\item{...}{Additional arguments.}
}
\description{
Combine count data with survey data
}
\details{
This function combines \code{counts} and \code{surveys} data. It performs an
inner join of \code{counts} on \code{surveys} by \code{document_id}. The function assumes
that both \code{counts} and \code{surveys} data include \code{document_id}.
}
