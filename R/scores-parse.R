

# Reference: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case/18509816.
.capitalize_first <-
  function(x) {
    stopifnot(is.character(x))
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }


.parse_scores_nfl_byyear_bywk <-
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

    data_events <-
      tibble::tibble(
        id = id,
        uid = uid,
        date = date,
        name = name,
        shortName = shortName
      )
    names(data_events) <-
      paste0("event", .capitalize_first(names(data_events)))

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

    data_venue <-
      tibble::tibble(
        fullName = fullName,
        city = city
      )
    names(data_venue) <-
      paste0("venue", .capitalize_first(names(data_venue)))

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

    data_competitors <-
      tibble::tibble(
        nameHome = nameHome,
        abbreviationHome = abbreviationHome,
        scoreHome = scoreHome,
        nameAway = nameAway,
        abbreviationAway = abbreviationAway,
        scoreAway = scoreAway
      )

    names(data_competitors) <-
      paste0("team", .capitalize_first(names(data_competitors)))

    data <-
      tibble::as_tibble(
        cbind(
          data_events,
          data_venue,
          data_competitors
        )
      )
    data
  }

#' Parse scores
#'
#' @description Parse scores.
#' @details
#' Note that the underlying implementation of this function IS
#' vastly different across leagues (unlike the `request_scores*()` set of
#' functions.
#'
#' Given the differences in underlying implementation for each league,
#' the dispatch methodology for parsing is the inverse of that
#' for `make_url_scores*()`, `request_scores*()`, and the high-level
#'  `get_scores*()` function.
#' That is, this generic `parse*()` function invokes the
#' appropriate `parse*[league]()` function. For the other sets of functions,
#' the league-specific functions serve as wrappers around the generic
#' function, either providing reasonable defaults (as is the
#' case with `make_url_scores_[league]()`) or convenience (as is the case with
#' `request_scores_[league]()`, which is really just the same
#' as `request_scores()`).
#'
#' @inheritParams make_url_scores
#' @param resp response object (from `{httr}` package). Required.
#' @param as character. Desired type of output. One of `complete` or `minimal`.
#' Inspired somewhat by the `httr::content()` analogue, although the implementation is different.
#' @param ... dots. Passed to `httr::content()`.
#' @return tibble
#' @export
#' @rdname parse_scores
parse_scores <-
  function(league, resp, as = "complete", ...) {

    # chkDots(...)
    as <- .validate_as(as)
    league <- .validate_league(league)
    f <- sprintf("parse_scores_%s", league)
    data <- purrr::invoke(f, resp = resp, as = as, ...)

    # NOTE: Class name is added in the `parse_scores_[league]()` function.
    data
  }

#' @export
#' @rdname parse_scores
parse_scores_nfl <-
  function(resp, as = "complete", ...) {

    as <- .validate_as(as)
    cont <- httr::content(resp, ...)

    if(as == "minimal") {

      # NOTE: Could do something like the following.
      # cont <- jsonlite::fromJSON(base::rawToChar(resp$content), ...)

      # TODO: Make this a tibble in order to make output consistent
      data <- tibble::enframe(unlist(cont))
    } else if(as == "complete") {
      data <- .parse_scores_nfl_byyear_bywk(cont)
    }

    # NOTE: Providing the name of the class here is done so that
    # the print method will work if this function is called directly
    # (instead of from `parse_scores()`.)
    data <- .add_class(data = data, "scores", as)
    data

  }

print.scores_minimal <-
  function(x, ...) {
    # msg <- sprintf("<Minimally parsed.>\n")
    # cat(msg, sep = "")
    # utils::str(x, max.level = 2, list.len = 10)
    x
    invisible(x)
  }

print.scores_complete <-
  function(x, ...) {
    # msg <- sprintf("<Completely parsed.>\n")
    # cat(msg, sep = "")
    x
    invisible(x)
  }
