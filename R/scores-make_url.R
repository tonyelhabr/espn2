
URL_BASE_SPORTS <- "http://site.api.espn.com/apis/site/v2/sports"

# NOTE: Examples of other potention endpoints:
# + http://games.espn.com/ffl/scoreboard?leagueId=453218&scoringPeriodId=15

#' Make a url for scores
#'
#' @description Make a url for scores.
#' @details
#' This function--in addition to the `request_scores()`
#' and `parse_scores()` functions--is not really intended to be called directly.
#' Instead, it is expected that `get_scores()` will be called.
#' Nonetheless, this function is provided for debugging purposes (not just
#' for the developer, but for the user).
#'
#' Note that although the API requires `sport` as a predecesor to league, this package
#' abstracts this choice away from the user because `league` is more likely what the
#' user wants to specify. `sport` can be derived from `league` internally.
#'
#' @param ... dots. Not currently used.
#' @param league character. Name of league for which to get scores. One of `nfl` or `nba`.
#' For `_[league]()` functions, set to appropriate league by default.
#' (e.g. For `_nfl()`, set to `nfl`.)
#' Note that only `nfl` is currently implemented.
#' @param lang,region,calendartype character(s). Not technically
#' required. Defaults are provided.
#' @param limit integer or integer-like character. Not technically required,
#' Default is provided.
#' @param year character or integer-like character. Equivalent to `dates` in the actual API.
#' Provided as an option because it is more intuitive.
#' @param dates See `year`.
#' @param seasontype integer or integer-like character.
#' For `nfl` scores, any one of 1 (for pre-season), 2 (for regular season),
#' or 3 (for playoffs)
#' Alternatively, a character, such as "pre-season", "regular season",
#' or "playoffs", and an attempt is made to correctly identify the appropriate
#' seasontype number to use. Not required, although the `_[league]()` functions
#' define a default.
#' @param week integer or character-like character. Not required, although the `_[league]()` functions
#' define a default.
#' @return character represpenting a url.
#' @export
#' @rdname make_url_scores
make_url_scores <-
  function(league,
           ...,
           lang = "en",
           region = "us",
           calendartype = "blacklist",
           limit = 100,
           year = format(Sys.Date(), "%Y"),
           dates = NULL,
           seasontype = NULL,
           week = NULL) {

    # chkDots(...)
    league <- .validate_league(league)
    if(league != "nfl") {
      msg <- sprintf("Code has not yet been implemented for `league` = `%s`.", league)
      stop(msg, call. = FALSE)
    }

    # NOTE: Validate stuff first.
    seasontype <- .convert_seasontype(seasontype, league = league)
    seasontype <- .validate_seasontype(seasontype, league = league)
    week <- .validate_week(week, seasontype = seasontype, league = league)
    dates <- .convert_likeyear(dates, year = year)
    dates <- .validate_likeyear(dates)

    sport <- .get_sport_given_league(league)
    url_base <-
      paste(
        URL_BASE_SPORTS,
        sport,
        league,
        "scoreboard?",
        sep = "/"
      )

    args <-
      list(
        lang = lang,
        region = region,
        calendartype = calendartype,
        limit = limit,
        dates = dates,
        seasontype = seasontype,
        week = week
      )
    query <- .compose_query(args)
    url <- paste0(url_base, query)
    url <- .add_class(url, "scores", "url")

    # NOTE: In some packages reviewed, `httr::GET()` is called immediately
    # after composing the query and the base url, but this is not done here.
    # resp <- httr::GET(url_base, query = args)

    # NOTE: This is the old version, which is somewhat clever, but maybe
    # does not qualify as a "best practice".
    # query <-
    #   paste(
    #     .zip2_query(lang),
    #     .zip2_query(region),
    #     .zip2_query(calendartype),
    #     .zip2_query(limit),
    #     .zip2_query(dates),
    #     .zip2_query(seasontype),
    #     .zip2_query(week),
    #     sep = "&"
    #   )
    # query <- gsub("\\&$|", "", query)
    # query <- gsub("\\&+", "&", query)
    # url <-
    #   paste0(url_base, query)
    url
  }

#' @export
#' @rdname make_url_scores
make_url_scores_nfl <-
  function(league = "nfl",
           seasontype = 2,
           week = 1,
           ...) {
    make_url_scores(
      league = league,
      seasontype = seasontype,
      week = week,
      ...
    )
  }

print.scores_url <-
  function(x, ...) {
    cat(x)
    invisible(x)
  }
