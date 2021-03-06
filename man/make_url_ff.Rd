% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff-make_url.R
\name{make_url_ff}
\alias{make_url_ff}
\title{Make a url for fantasy football data}
\usage{
make_url_ff(leagueId, ..., year = format(Sys.Date(), "\%Y"),
  seasonId = NULL, matchupPeriodId = NULL)
}
\arguments{
\item{leagueId}{integer or integer-like character. Required.}

\item{...}{dots. Not currently used.}

\item{year}{integer or integer-like character. Equivalent to `seasonId` in the actual API.
Provided as an option because it is more intuitive.}

\item{seasonId}{See `year`.}

\item{matchupPeriodId}{integer or integer-like character. Code to handle a non-`NULL` value
is not currently implemented.}
}
\value{
character representing a url.
}
\description{
Make a url for fantasy football data.
}
\details{
This function--as well as the other `_ff()` functions--are
analogous to their `scores` equivalent. The major difference among the analogous
functions is that their is no `league` argument to be specified (and, consequently,
there are no suffixed `_[league]()` functions). Moreover, this
`make_url_ff()` functions has different possible query parameters, although
the other analogous `_ff()` functions have the same arguments as their
`scores` equivalents.

Note that the league must be public in order for these `_ff()` functions to work.
}
