
#' Retrieve fantasy football data
#'
#' @description Retrieve fantasy football data.
#' @details
#' See the `make_url*()` function for more details.s
#' @param url character. Required.
#' @param ... dots. Not currently used.
#' @return response object.
#' @export
#' @rdname request_ff
request_ff <-
  function(url, ...) {
    
    # chkDots(...)
    resp <- httr::GET(url)
    httr::warn_for_status(resp)
    
    resp <- .add_class(data = resp, prefix = "ff", suffix = "response")
    resp
  }

# print.ff_response <-
#   function(x, ...) {
#     msg <- sprintf("<Response from `url` %s>\n", x$request$url)
#     cat(msg, sep = "")
#     str(x)
#     invisible(x)
#   }
