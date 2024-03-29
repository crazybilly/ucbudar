% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_do_assignments.R
\name{get_do_assignments}
\alias{get_do_assignments}
\title{Get Solicitation Plan Assignments}
\usage{
get_do_assignments(
  start_date = fyStartDate,
  end_date = fyEndDate,
  fundraiser_id,
  unit,
  include_post_commit_assignments = F,
  db = cdw
)
}
\arguments{
\item{start_date}{a YYYY-MM-DD date-like object (can be a string or date) for the date range when plan assignment should be active}

\item{end_date}{a YYYY-MM-DD date-like object (can be a string or date) for the date range when plan assignment should be active}

\item{fundraiser_id}{an optional vector of fundraiser entity IDs}

\item{unit}{an optional vector of assignment office codes}

\item{include_post_commit_assignments}{a logical indicating whether assignments which started after the proposal commit/turndown date should be include. TRUE to include stewardship and planned giving assignments. FALSE for standard gift cultivation-type assignments.}

\item{db}{an active database connection. Must be PROD (the data needed to build qualifications doesnt live in CDW2)}
}
\value{
a database tibble with one row per fundraiser/assignment/proposal/prospect/entity. Includes non-primary entities for all prospects.
}
\description{
Get Solicitation Plan Assignments
}
