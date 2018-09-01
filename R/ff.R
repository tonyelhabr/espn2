
URL_BASE_FF <-
  "http://games.espn.com/ffl/api/v2"

# NOTE: Unlike the `scores` functions, this `make_url*()` functions is not exposed to the user.
make_url_ff <-
  function(leagueId,
           year = format(Sys.Date() - 365, "%Y"),
           seasonId = NULL) {

    stopifnot(!is.na(is.integer(seasonId)))
    if(is.null(seasonId)) {
      seasonId <- year
    }

    # NOTE: Check that seasonId could be converted to an integer (if it is a chraracter, which is fine).
    stopifnot(!is.na(is.integer(seasonId)))
    url_base <-
      paste(
        URL_BASE_FF,
        "leagueSettings?",
        sep = "/"
      )
    query <-
      paste(
        .zip2_query(leagueId),
        .zip2_query(seasonId),
        sep = "&"
      )
    url <-
      paste0(url_base, query)

  }

request_ff <-
  function(url, as = "raw", ...) {

    # NOTE: This isn't working...
    # cont_raw <- .request(url = url, as = as)

    # Or...
    res <- httr::GET(url)
    httr::warn_for_status(res)

    cont_raw <- base::rawToChar(res$content)
    cont <- jsonlite::fromJSON(cont_raw)
    cont
  }


get_ff <-
  function(url = NULL,
           ...,
           as = c("parsed", "raw")) {

    as <- match.arg(as)
    if(is.null(url)) {
      url <- make_url_ff(...)
    }
    cont <- request_ff(url)
    if(as == "raw") {
      return(cont)
    }
    cont
  }
