

.validate_league <-
  function(x, nm = deparse(substitute(x))) {
    # if(is.null(x)) {
    #   msg <- sprintf("`%s` cannot be `NULL`.", nm)
    #   stop(msg, call. = FALSE)
    # }
    # choices <- c("nfl", "nba")
    # if(length(intersect(x, choices)) == 1) {
    #   return(x)
    # }
    # msg <- sprintf("Not a valid value for `%s`.", nm)
    # stop(msg, call. = FALSE)
    choices <- c("nfl", "nba")
    x <- match.arg(x, choices)
    x
  }

.get_sport_given_league <-
  function(league) {
    switch(league,
           nfl = "football",
           nba = "basketball")
  }

.convert_seasontype <-
  function(x, league, nm = deparse(substitute(x))) {

    if(is.null(x)) {
      return(x)
    } else if (!is.character(x)) {
      if(is.likeinteger(x)) {
        return(x)
      } else {
        msg <- sprintf("`%s` must be integer-like.", nm)
        stop(msg, call. = FALSE)
      }
    }

    if(league == "nfl") {
      if(grepl("reg", x)) {
        res <- 2
      } else if(grepl("pre", x)) {
        res <- 1
      } else if(grepl("post|play", x)) {
        res <- 3
      } else {
        msg <- sprintf("Could not convert `%s`.", nm)
        stop(msg, call. = FALSE)
      }
    } else {
      msg <- sprintf("Don't know how to convert `%s` for `league` =  `%s`.", nm, league)
      stop(msg, call. = FALSE)
    }
    res
  }

.validate_seasontype <-
  function(x, league, nm = deparse(substitute(x))) {

    if(is.null(x)) {
      return(x)
    }
    if(league == "nfl") {
      if(!is.likeinteger(x)) {
        msg <- sprintf("`%s` must be integer-like.", nm)
        stop(msg, call. = FALSE)
      }
      if(x < 1 | x > 3) {
        msg <- sprintf("`%s` is not within valid range (between `1` and `3`).", nm)
        stop(msg, call. = FALSE)
      }
    } else {
      msg <- sprintf("Don't know how to validate `%s` for `league` = `%s`.", nm, league)
      stop(msg, call. = FALSE)
    }
    x
  }

.validate_week <-
  function(x, seasontype, league, nm = deparse(substitute(x))) {

    if(is.null(x)) {
      return(x)
    } else if (is.null(seasontype)) {
      msg <- sprintf("`seasontype` cannot be `NULL` if `%s` is not `NULL`.", nm)
      stop(msg, call. = FALSE)
    }
    if(!is.likeinteger(x)) {
      msg <- sprintf("`%s` must be integer-like.", nm)
      stop(msg, call. = FALSE)
    }

    if(league == "nfl") {

      if(seasontype == 2) {
        if(x < 1 | x > 17) {
          msg <- sprintf("`%s` is not within valid range (between `1` and `17`).", nm)
          stop(msg, call. = FALSE)
        }
      } else if(seasontype == 1) {
        if(x > 5) {
          msg <- sprintf("`%s` is not within valid range (between `1` and `5`).", nm)
          stop(msg, call. = FALSE)
        }
      } else if (seasontype == 3) {
        # NOTE: Need to actually check how many seasontypes there are.
        # (`seasontypes` 1 and 2 have been verified.)
        # In the meantime assuming `seasontype == 3` is playoffs and maximum valid `week` is 5.
        if(x > 5) {
          msg <- sprintf("`%s` is not within valid range (between `1` and `5`).", nm)
          stop(msg, call. = FALSE)
        }
      }
    } else {
      msg <- sprintf("Don't know how to validate `%s` for `league` = `%s`.", nm, league)
      stop(msg, call. = FALSE)
    }
    x
  }

