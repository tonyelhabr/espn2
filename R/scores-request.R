

#' Retrieve scores
#'
#' @description Retrieve scores.
#' @details
#' Note that the underlying implementation of this function does not
#' differ accross leagues. Nonetheeless, suffixed version (i.e. `request_scores_[league]()`)
#' are provided in order to main consistency with the other classes of functions
#' (e.g. `mak_url_scores_[league]()`).
#'
#' @inheritParams make_url_scores
#' @param url character. Required.
#' @param ... dots. Not currently used.
#' @return response object.
#' @export
#' @rdname request_scores
request_scores <-
  function(league, url, ...) {

    # chkDots(...)
    league <- .validate_league(league)
    resp <- httr::GET(url)
    httr::warn_for_status(resp)

    resp <- .add_class(data = resp, "scores", "response")
    resp
  }

# NOTE: Allow the `httr::print.response()` function to be used.
# print.scores_response <-
#   function(x, ...) {
#     # msg <- sprintf("<Response from `url` %s>\n", x$request$url)
#     # cat(msg, sep = "")
#     str(x, max.depth = 2, list.len = 2)
#     invisible(x)
#   }

#' @export
#' @rdname request_scores
request_scores_nfl <-
  function(league = "nfl", ...) {
    request_scores(league = league, ...)
  }
