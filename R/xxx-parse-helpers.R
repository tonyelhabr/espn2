
.validate_as <-
  function(x, nm = deparse(substitute(x))) {
    # if(is.null(x)) {
    #   msg <- sprintf("`%s` cannot be `NULL`.", nm)
    #   stop(msg, call. = FALSE)
    # }
    # choices <- c("complete", "minimal")
    # if(length(intersect(x, choices)) == 1) {
    #   return(x)
    # }
    # msg <- sprintf("Not a valid value for `%s`.", nm)
    # stop(msg, call. = FALSE)
    choices <- c("complete", "minimal")
    x <- match.arg(x, choices)
    x
  }
