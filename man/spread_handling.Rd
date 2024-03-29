% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_handling_for_unit.R
\name{spread_handling}
\alias{spread_handling}
\title{Spread Special Handling Codes}
\usage{
spread_handling(handling_data)
}
\arguments{
\item{handling_data}{a distinct list of handling codes with one line per entity id and handling code. Can be generated with \code{get_handling_for_unit()}}
}
\value{
a dataframe with one row per entity id
}
\description{
pivots a vertical list of special handling codes out into a wide vector with one column per code and one row per entity ID
}
