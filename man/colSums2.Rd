% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colSums2.R
\name{colSums2}
\alias{colSums2}
\title{Column Sums (safe-ish)}
\usage{
colSums2(df, remove_non_numeric = F, na.rm = T)
}
\arguments{
\item{df}{a data frame with columns to sum}

\item{remove_non_numeric}{a logical value determing whether or not NA values (ie. columns that cannot be cooreced to numeric and therefore won't sum) should be removed from the returned results}

\item{na.rm}{a logical value determining if NA values should be ignored when summing}
}
\value{
a one-row data frame with the sum of each column. Columns that cannot be summed return NA unless remove_non_numeric=T
}
\description{
calculate column sums for all columns, returning NA for columns that won't coerce to numeric
}
