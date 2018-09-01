
URL_BASE_SPORTS <- "http://site.api.espn.com/apis/site/v2/sports"

.convert_seasontype <-
  function(x, league) {
    if(is.wholenumber(x)) {
      return(x)
    }
    stopifnot(is.character(x))
    if(league == "nfl") {
      if(grepl("reg", x)) {
        2
      } else if(grepl("pre", x)) {
        1
      } else if(grepl("post|play", x)) {
        3
      } else {
        msg <- "Could not convert."
        stop(msg, call. = FALSE)
      }
    } else {
      msg <- sprintf("Don't know how to convert for league %s.", league)
      stop(msg, call. = FALSE)
    }
  }

# TODO!
.validate_seasontype <-
  function(x, league) {
    x <- .convert_seasontype(x = x, league = league)
    if(league == "nfl") {

    } else {
      msg <- sprintf("Don't know how to convert for league %s.", league)
      stop(msg, call. = FALSE)
    }
  }

# TODO!
.validate_week <-
  function(x, seasontype, league) {

  }

#' Build a url
#'
#' @description Build a url.
#' @details
#' Same details as the `request_scores()` function...
#'
#' Note that although the API requires `"sport"` as a predecesor to league, this package
#' abstracts this choice away from the user because `"league"` is more likely what the
#' user wants to specify. `"sport"` can be derived from `"league"` internally.
#' @param ... dots. Not currently used.
#' @param league character(s). Required for url.
#' @param lang,region,calendartype character(s). Required. Reasonable defaults are provided.
#' @param limit numeric or character. Part of query string. Required.
#' @param dates character. May or may not be required. Should be formatted as year.
#' @param year character. Equivalent to `dates` in the actual API.
#' Provided as an option because it is more intuitive.
#' @param seasontype integer or numeric. See details.
#' @param week integer. Only required for NFL scores.
#' @return character representing a url.
#' @export
#' @rdname make_url_scores
#'
make_url_scores <-
  function(league = c("nfl", "nba"),
           ...,
           lang = "en",
           region = "us",
           calendartype = "blacklist",
           limit = 100,
           year = format(Sys.Date() - 365, "%Y"),
           dates = NULL,
           seasontype = NULL,
           week = NULL) {

    league <- match.arg(league)
    if(league != "nfl") {
      msg <- sprintf("Code has not yet been implemented for league `%s`.", league)
      stop(msg, call. = FALSE)
    }

    sport <-
      switch(league,
             nfl = "football",
             nba = "basketball")

    # TODO: Transfer these checks to `.validate*()` functions.
    if(league == "nfl") {

      stopifnot(is.wholenumber(seasontype))
      stopifnot(is.wholenumber(week))
      stopifnot(seasontype >= 1, seasontype <= 3)
      stopifnot(week >= 1, week <= 17)
      if(seasontype == 1) {
        stopifnot(week <= 5)
      } else if (seasontype == 3) {
        # NOTE: Need to actually check how many seasontypes there are.
        # (`seasontypes` 1 and 2 have been verified.)
        # In the meantime assuming `seasontype == 3` is playoffs and maximum valid `week` is 5.
        stopifnot(week <= 5)
      }
    }

    url_base <-
      paste(
        URL_BASE_SPORTS,
        sport,
        league,
        "scoreboard?",
        sep = "/"
      )

    if(is.null(dates)) {
      dates <- year
    }

    # NOTE: Need to check this somehow.
    # NOTE: Check that seasonId could be converted to an integer (if it is a chraracter, which is fine).
    stopifnot(!is.na(as.integer(dates)))

    # NOTE: Could use `httr::modify_url()` or `purrr::compact()` here.
    query <-
      paste(
        .zip2_query(lang),
        .zip2_query(region),
        .zip2_query(calendartype),
        .zip2_query(limit),
        .zip2_query(dates),
        .zip2_query(seasontype),
        .zip2_query(week),
        sep = "&"
      )
    url <-
      paste0(url_base, query)
  }

#' Build a url for NFL scores
#'
#' @description Build a url for NFL scores.
#' @details This is a wrapper function for `make_url_scores()`.
#' @inheritParams make_url_scores
#' @param league character. `"nfl"`.
#' @param year character and/or numeric. Default is providied.
#' @param seasontype,week numberic(s). Required. Defaults are provided.
#' @param ... dots. Passed to wrapped function.
#' @return character representing a url.
#' @export
#' @rdname make_url_scores_nfl
make_url_scores_nfl <-
  function(league = "nfl",
           seasontype = 2,
           week = 1,
           year = format(Sys.Date() - 365, "%Y"),
           ...) {
    make_url_scores(
      league = league,
      seasontype = seasontype,
      week = week,
      year = year,
      ...
    )
  }


# Reference: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case/18509816.
.capitalize_first <-
  function(x) {
    stopifnot(is.character(x))
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

.parse_scores_nfl <-
  function(cont) {

    stopifnot("events" %in% names(cont))
    events <- cont$events

    # Debugging...
    # str(events[[1]], max.level = 1, list.len = 20)
    # events[[1]]$status

    # Non-list top-level element(s): id, uid, date, name, shortName
    # 1-element lists: competitions
    # multi-element lists: season, links, status

    # NOTE: `purrr::map_int()` returns an error with `.$id`
    id <- as.integer(purrr::map_chr(events, ~.$id))
    uid <- purrr::map_chr(events, ~.$uid)
    date <- purrr::map_chr(events, ~.$date)
    name <- purrr::map_chr(events, ~.$name)
    shortName <- purrr::map_chr(events, ~.$shortName)

    tbl_events <-
      tibble::tibble(
        id = id,
        uid = uid,
        date = date,
        name = name,
        shortName = shortName
      )
    names(tbl_events) <-
      paste0("event", .capitalize_first(names(tbl_events)))

    competitions <- purrr::map(events, ~.$competitions)

    # Debugging...
    # str(competitions[[1]][[1]], max.level = 1)
    # competitions[[1]][[1]]$status

    # Non-list top-level element(s):
    # id, uid, date, attendance, timeValid, neutralSite, conferenceCompetition,
    # recent, startDate
    # 1-element list element(s): notes, broadcasts, tickets, geoBroadcasts, odds(?)
    # multi-element list element(s): venue, competitors, staus

    venue <- purrr::map(competitions, ~.[[1]]$venue)
    # Debugging...
    # str(venue[[1]], max.level = 1)
    fullName <- purrr::map_chr(venue, ~ .$fullName)
    city <- purrr::map_chr(venue, ~.$address$city)
    state <- purrr::map_chr(venue, ~.$address$state)

    tbl_venue <-
      tibble::tibble(
        fullName = fullName,
        city = city,
        state = state
      )
    names(tbl_venue) <-
      paste0("venue", .capitalize_first(names(tbl_venue)))

    competitors <- purrr::map(competitions, ~.[[1]]$competitors)
    # Debugging...
    # str(competitors[[1]][[1]], max.level = 1)

    # Non-list top-level element(s): id, uid, type, order, homeAway, score,
    # records
    # 1-element list element(s): statistics
    # multi-element list element(s): team

    # Debuggning...
    # str(competitors[[1]][[1]]$team, max.level = 1)
    # Non-list element(s): id, uid, location, name, abbreviation, displayName,
    # shortDisplayName, color, alternateColor, isActive, logo
    # 1-element list element(s): venue
    # multi-element list element(s): links

    # NOTE: Don't split this up into teamHome and teamAway
    # with `competitors[[1]][[1]]` and `competitors[[1]][[2]]`
    nameHome <- purrr::map_chr(competitors, ~.[[1]]$team$name)
    abbreviationHome <- purrr::map_chr(competitors, ~.[[1]]$team$abbreviation)
    scoreHome <- as.integer(purrr::map_chr(competitors, ~.[[1]]$score))

    nameAway <- purrr::map_chr(competitors, ~.[[2]]$team$name)
    abbreviationAway <- purrr::map_chr(competitors, ~.[[2]]$team$abbreviation)
    scoreAway <- as.integer(purrr::map_chr(competitors, ~.[[2]]$score))

    competitors[[1]][[1]]$linescores[1]$value

    tbl_competitors <-
      tibble::tibble(
        nameHome = nameHome,
        abbreviationHome = abbreviationHome,
        scoreHome = scoreHome,
        nameAway = nameAway,
        abbreviationAway = abbreviationAway,
        scoreAway = scoreAway
      )

    names(tbl_competitors) <-
      paste0("team", .capitalize_first(names(tbl_competitors)))

    tibble::as_tibble(
      cbind(
        tbl_events,
        tbl_venue,
        tbl_competitors
      )
    )
  }

# NOTE: Abstracting this parsing away from the user.
.parse_scores <-
  function(league, cont) {
    f <- sprintf(".parse_scores_%s", league)
    purrr::invoke(f, cont = cont)
  }

.request <-
  function(url, ...) {
    res <- httr::GET(url)

    # Reference: https://github.com/mkearney/ig/blob/master/R/api.R.
    httr::warn_for_status(res)

    cont <- httr::content(res, ...)
    cont
  }

#' Retrieve scores
#'
#' @description Retrieve scores.
#' @details
#' Note that this functions does not come with suffixed league forms
#' (e.g. `request_scores_nfl()`).
#'
#' This function is not really intended to be called directly.
#' Instead, it is expected that `get_scores()` will be called.
#' Nonetheless, this function is provided for debugging purposes (not just
#' for the developer, but for the user).
#' @inheritParams get_scores
#' @return `tibble`
#' @export
#' @rdname reques_scores
request_scores <-
  function(url, ...) {
    .request(url = url, ...)
  }

#' Get scores
#'
#' @description Get scores.
#' @details
#' This functions calls `make_url_scores()` and `rqeuest_scores()`,
#' then provides the added functionality of parsing.
#' @param league character. Name of league for which to get scores. One of `nfl` or `nba`.
#' Only `nfl` is currently implemented.
#' @param url character. Not required. If provided,
#' it is used with priority over `...`. Passed to `request_scores()`.
#' Expects `"events"` element at the top level of the response.
#' @param ... dots. Arguments to pass to `make_url_scores()` to create `url`.
#' @param as character. desired type of output. One of `parsed` or `raw`.
#' Inspired somewhat by the `httr::content()` analogue, although the implementation is different.
#' @param depth character. Specifies which elements to return from parsed content.
#' Only specific values are allowed. Default is provided.
#' @param col_names character vector. Names of columns in returned `tibble`. Only used
#' if `depth` = `"custom"`.
#' @return `tibble`
#' @export
#' @rdname get_scores
get_scores <-
  function(league = c("nfl", "nba"),
           url = NULL,
           ...,
           as = c("parsed", "raw"),
           depth = c("all", "custom"),
           col_names = NULL) {

    league <- match.arg(league)
    as <- match.arg(as)
    depth <- match.arg(depth)

    if (is.null(url)) {
      # f <- sprintf("make_url_scores_%s", league)
      # url <- purrr::invoke(f, ...)
      url <- make_url_scores(league = league, ...)
    }

    cont <-
      request_scores(
        url = url,
        league = league,
        ...
      )


    if(as == "raw") {
      return(cont)
    }
    # NOTE: Could do something like
    # cont <- jsonlite::fromJSON(base::rawToChar(res$conten), ...)

    data <- .parse_scores(cont = cont, league = league)

    if(depth == "all") {
      col_names <- names(data)
    } else {
      stopifnot(!is.null(col_names))
      stopifnot(is.character(col_names))

      lgl <- all(col_names %in% names(data))
      if(!lgl) {
        col_names_miss <- setdiff(col_names, names(data))
        browser()
        msg <-
          sprintf("Not all `col_names` in requested data. Missing: %s",
                  paste(col_names_miss, sep = ", ", collapse = "")
          )
        stop(msg, call. = FALSE)
      }
    }

    data[,c(col_names)]
  }

#' Get NFL scores
#'
#' @description Get NFL scores.
#' @details This is a wrapper function for `get_scores()`.
#' @inheritParams get_scores
#' @param league character(s). Required. Default is provided.
#' @param col_names character vector. Required. Default is provided.
#' @param ... dots. Passed to wrapped function.
#' @return character representing a url.
#' @export
#' @rdname get_scores_nfl
get_scores_nfl <-
  function(league = "nfl",
           col_names = c("eventId",
                         "eventDate",
                         "venueFullName",
                         "venueCity",
                         "venueState",
                         "teamNameHome",
                         "teamAbbreviationHome",
                         "teamScoreHome",
                         "teamNameAway",
                         "teamAbbreviationAway",
                         "teamScoreAway"),
           ...) {
    get_scores(
      league = league,
      col_names = col_names,
      ...
    )

  }

# event.competitions.competitors.1.teamName.
