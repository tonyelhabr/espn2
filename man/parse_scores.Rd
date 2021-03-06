% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scores-parse.R
\name{parse_scores}
\alias{parse_scores}
\alias{parse_scores_nfl}
\title{Parse scores}
\usage{
parse_scores(league, resp, as = "complete", ...)

parse_scores_nfl(resp, as = "complete", ...)
}
\arguments{
\item{league}{character. Name of league for which to get scores. One of `nfl` or `nba`.
For `_[league]()` functions, set to appropriate league by default.
(e.g. For `_nfl()`, set to `nfl`.)
Note that only `nfl` is currently implemented.}

\item{resp}{response object (from `{httr}` package). Required.}

\item{as}{character. Desired type of output. One of `complete` or `minimal`.
Inspired somewhat by the `httr::content()` analogue, although the implementation is different.}

\item{...}{dots. Passed to `httr::content()`.}
}
\value{
tibble
}
\description{
Parse scores.
}
\details{
Note that the underlying implementation of this function IS
vastly different across leagues (unlike the `request_scores*()` set of
functions.

Given the differences in underlying implementation for each league,
the dispatch methodology for parsing is the inverse of that
for `make_url_scores*()`, `request_scores*()`, and the high-level
 `get_scores*()` function.
That is, this generic `parse*()` function invokes the
appropriate `parse*[league]()` function. For the other sets of functions,
the league-specific functions serve as wrappers around the generic
function, either providing reasonable defaults (as is the
case with `make_url_scores_[league]()`) or convenience (as is the case with
`request_scores_[league]()`, which is really just the same
as `request_scores()`).
}
