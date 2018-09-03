

URL_BASE_FF <-
  "http://games.espn.com/ffl/api/v2"

# NOTE: Examples of other potention endpoints:
# + http://games.espn.com/ffl/scoreboard?leagueId=453218&scoringPeriodId=15
# + http://games.espn.com/ffl/tools/finalstandings?leagueId=453218&seasonId=2017.
# + http://games.espn.com/ffl/h2hplayoffs?leagueId=453218&seasonId=2017
# + http://games.espn.com/ffl/boxscorequick?leagueId=453218&teamId=2&scoringPeriodId=13&seasonId=2017&view=scoringperiod&version=quick
# + http://games.espn.com/ffl/boxscorefull?leagueId=453218&teamId=2&scoringPeriodId=13&seasonId=2017&view=scoringperiod&version=full

#' Make a url for fantasy football data
#'
#' @description Make a url for fantasy football data.
#' @details
#' This function--as well as the other `_ff()` functions--are
#' analogous to their `scores` equivalent. The major difference among the analogous
#' functions is that their is no `league` argument to be specified (and, consequently,
#' there are no suffixed `_[league]()` functions). Moreover, this
#' `make_url_ff()` functions has different possible query parameters, although
#' the other analogous `_ff()` functions have the same arguments as their
#' `scores` equivalents.
#'
#' Note that the league must be public in order for these `_ff()` functions to work.
#'
#' @param leagueId integer or integer-like character. Required.
#' @param ... dots. Not currently used.
#' @param year integer or integer-like character. Equivalent to `seasonId` in the actual API.
#' Provided as an option because it is more intuitive.
#' @param seasonId See `year`.
#' @param matchupPeriodId integer or integer-like character. Code to handle a non-`NULL` value
#' is not currently implemented.
#' @return character representing a url.
#' @export
#' @rdname make_url_ff
make_url_ff <-
  function(leagueId,
           ...,
           year = format(Sys.Date(), "%Y"),
           seasonId = NULL,
           matchupPeriodId = NULL) {

    # chkDots(...)

    # NOTE: This is different than some of the other query parameters--it cannot be `NULL`.
    leagueId <- .validate_leagueId(leagueId)
    # NOTE: This processing is exactly like that for `dates` with `make_url_scores()`.
    seasonId <- .convert_likeyear(seasonId, year = year)
    seasonId <- .validate_likeyear(seasonId)

    if(!is.null(matchupPeriodId)) {
      msg <-
        sprintf(
          paste0("Code to handle A non-`NULL` value for `matchupPeriodId` is not ",
                 "currently implemented. Setting to `NULL`."
          )
        )
      message(msg)
      matchupPeriodId <- NULL
    }

    url_base <-
      paste(
        URL_BASE_FF,
        "leagueSettings?",
        sep = "/"
      )

    args <-
      list(
        leagueId = leagueId,
        seasonId = seasonId
      )
    query <- .compose_query(args)
    url <- paste0(url_base, query)
    url <- .add_class(url, "ff", "url")
    url

  }

# NOTE: The `httr::print.url()` function cannot be used because this url
# is not structured like what that function expects.
# TODO: Make this url class exactly analogous to that from the `{httr}` package.
print.scores_url <-
  function(x, ...) {
    cat(x)
    invisible(x)
  }


