

#' Get scores
#'
#' @description Get scores.
#' @details
#' This functions calls `make_url_scores()`, followed by`request_scores()`,
#' followed by `parse_scores()`.
#'
#' @inheritParams make_url_scores
#' @param url,resp Functionally equivalent to the arguments in the `make_url_scores()` and
#' `request_scores()` functions. However, not required for this function.
#' If provided, then used and function that would be called is skipped.
#' (e.g. If `resp` is provided, then `make_url_scores()` and `request_scores()`
#' are skipped. Instead, `parse_scores()` is called directly.)
#' @param ... dots. Passed to wrapped functions.
#' @return tibble, with the following columns: TBD.
#' @export
#' @rdname get_scores
get_scores <-
  function(league, url = NULL, resp = NULL, ...) {

    league <- .validate_league(league)

    if(is.null(url) & is.null(resp)) {
      url <- make_url_scores(league = league, ...)
    }

    if(is.null(resp)) {
      resp <- request_scores(league = league, url = url, ...)
    }

    data <- parse_scores(league = league, resp = resp, ...)
    data
  }

#' @export
#' @rdname get_scores
get_scores_nfl <-
  function(league = "nfl", ...) {
    get_scores(
      league = league,
      ...
    )

  }

# NOTE: The current implementation could be re-factored to use S3 methods
# in the following manner.
# get_scores <-
#   function(x, ...) {
#     UseMethod("get_scores")
#   }
#
# get_scores.default <-
#   function(x, league, ...) {
#     league <- .validate_league(league = league)
#     url <- make_url_scores(league = league, ...)
#     resp <- request_scores(league = league, url = url, ...)
#     data <- parse_scores(league = league, resp = resp, ...)
#     data
#   }
#
#
# get_scores.url <-
#   function(x, league, ...) {
#     resp <- request_scores(league = league, url = x, ...)
#     data <- parse_scores(league = league, resp = resp, ...)
#     data
#   }
#
# get_scores.response <-
#   function(x, league, ...) {
#     data <- parse_scores(league = league, resp = x, ...)
#     data
#   }
