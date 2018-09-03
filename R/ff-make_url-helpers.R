
.validate_leagueId <-
  function(x, nm = deparse(substitute(x))) {
    if(is.null(x)) {
      msg <- sprintf("`%s` cannot be `NULL`.", nm)
      stop(msg, call. = FALSE)
    }
    if(!is.likeinteger(x)) {
      msg <- sprintf("`%s` must be integer-like.", nm)
      stop(msg, call. = FALSE)
    }
    x
  }

.validate_matchupPeriodId <-
  function(x, nm = deparse(substitute(x))) {
    if(is.null(x)) {
      return(x)
    }
    if(!is.likeinteger(x)) {
      msg <- sprintf("`%s` must be integer-like.", nm)
      stop(msg, call. = FALSE)
    }
    x
  }
