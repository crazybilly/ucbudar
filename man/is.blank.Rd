% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.blank.R
\name{is.blank}
\alias{is.blank}
\title{is.blank}
\usage{
is.blank(x)
}
\arguments{
\item{x}{a vector to test for blank values}
}
\value{
a vector of logical values, where TRUE indicates blank values
}
\description{
tests a vector, returning TRUE if values are NA, NULL, blank strings or NAN. Also returns \code{TRUE} for strings that are entirely a combinate of spaces and dashes, eg. "-" or " - ", or are "()-", often used as an empty phone number.
}
