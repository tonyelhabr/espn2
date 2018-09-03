
#' Get fantasy football data
#'
#' @description Get fantasy football data.
#' @details
#' Like `get_scores()`, this functions calls
#' `make_url*()`, followed by `request*()`, followed by `parse*()`.
#'
#' @param url,resp Functionally, equivalent to the same arguments `get_scores()`.
#' @param ... dots. Passed to wrapped functions.
#' @return tibble, with the following columns: TBD.
#' @export
#' @rdname get_ff
get_ff <-
  function(url = NULL, resp = NULL, ...) {

    if(is.null(url) & is.null(resp)) {
      url <- make_url_scores(...)
    }

    if(is.null(resp)) {
      resp <- request_scores(url = url, ...)
    }

    data <- parse_scores(resp = resp, ...)
    data
  }
