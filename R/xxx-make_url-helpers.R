

.convert_likeyear <-
  function(x, year) {
    if(is.null(x)) {
      x <- year
    }
    x
  }

#  TODO: Implement a function like this to reduce some repetitiveness.
.validate_likeinteger <-
  function(x, nm = deparse(substitute(x))) {
    lgl <- !is.na(as.integer(x))
    if(!lgl) {
      msg <- sprintf("`%s` is not integer-like.", nm)
      stop(msg, call. = FALSE)
    }
    x
  }

.validate_likeyear <-
  function(x, nm = deparse(substitute(x))) {
    if(is.null(x)) {
      return(x)
    }
    lgl <- !is.na(as.integer(x))
    if(!lgl) {
      msg <- sprintf("`%s` is not integer-like.", nm)
      stop(msg, call. = FALSE)
    }
    x
  }

# .zip2_query <-
#   function(x, y = deparse(substitute(x)), sep = "=") {
#     if(is.null(x)) {
#       return("")
#     }
#     paste0(y, sep, x)
#   }

# NOTE: This is directly copied from `httr::has_name()`.
# (See https://github.com/r-lib/httr/blob/976289a3596dc01dc994f8fd743770a172bbecdb/R/utils.r.)
.has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms))
    return(rep(FALSE, length(x)))
  
  !is.na(nms) & nms != ""
}

# ..encode <- function(x) {
#   if (inherits(x, "AsIs")) return(x)
#   curl::curl_escape(x)
# }

# NOTE: This is a simplified version of `httr::compose_query()`.
# (See https://github.com/r-lib/httr/blob/976289a3596dc01dc994f8fd743770a172bbecdb/R/url-query.r.)
.compose_query <- function(elements) {
  if (length(elements) == 0) {
    return("")
  }
  
  if (!all(.has_name(elements))) {
    stop("All components of query must be named", call. = FALSE)
  }
  
  stopifnot(is.list(elements))
  elements <- purrr::compact(elements)
  
  # NOTE: Remove this in order to eliminate dependency on `{curl}`.
  # nms <- curl::curl_escape(names(elements))
  # values <- vapply(elements, ..encode, character(1))
  nms <- names(elements)
  values <- .unlistname(elements)
  
  paste0(nms, "=", values, collapse = "&")
}
