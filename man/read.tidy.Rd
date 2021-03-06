% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.tidy.R
\name{read.tidy}
\alias{read.tidy}
\title{Read csvs into tidy format}
\usage{
read.tidy(x = "clipboard", sep = "\\t", ifexists = F, existsname, ...)
}
\arguments{
\item{x}{the filename to load. Should be a csv. Defaults to reading from the clipboard.}

\item{sep}{the field separator character. If x == 'clipboard', sep defaults to tab seperated files. Otherwise, it defaults to ',' to read csv data.}

\item{ifexists}{should the function check if the object exists. Default = F}

\item{existsname}{what object should the function look for, assuming ifexists = T. If ifexists == T and existname is not empty, the function will return the object with name = existsname.}

\item{...}{
  Arguments passed on to \code{\link[utils:read.table]{utils::read.csv}}
  \describe{
    \item{\code{header}}{a logical value indicating whether the file contains the
    names of the variables as its first line.  If missing, the value is
    determined from the file format: \code{header} is set to \code{TRUE}
    if and only if the first row contains one fewer field than the
    number of columns.}
    \item{\code{quote}}{the set of quoting characters. To disable quoting
    altogether, use \code{quote = ""}.  See \code{\link{scan}} for the
    behaviour on quotes embedded in quotes.  Quoting is only considered
    for columns read as character, which is all of them unless
    \code{colClasses} is specified.}
    \item{\code{dec}}{the character used in the file for decimal points.}
    \item{\code{fill}}{logical. If \code{TRUE} then in case the rows have unequal
    length, blank fields are implicitly added.  See \sQuote{Details}.}
    \item{\code{comment.char}}{character: a character vector of length one
    containing a single character or an empty string.  Use \code{""} to
    turn off the interpretation of comments altogether.}
  }}
}
\value{
Returns a data frame will lower cased names and all punctuation removed. If dplyr is already loaded, it returns a data frame as a tbl_df--otherwise, it returns a simple data frame.
}
\description{
A convenience wrapper for read.csv which lowercases column names and removes punctuation (but leaves underscores). It also has an option to only read the csv if the object does not exist.
}
