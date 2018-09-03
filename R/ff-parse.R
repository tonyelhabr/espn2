
.fix_wks_ff <-
  function(x, idx1, idx2, times) {
    x2 <- x[c(1:idx1)]
    idx_diff <- idx2 - idx1
    i <- 1
    while(i <= idx_diff) {
      x2_add <- rep(x[i], times = times)
      x2 <- c(x2, x2_add)
      i <- i + times
    }
    x2
  }

.parse_ff_byyear_bytm <-
  function(tm, n_wks_reg = 12, n_wks_plyffs = 2, n_wks_plyffs_rnd = 2) {

    matchups <- tm$scheduleItems$matchups

    awayTeamScores <- purrr::map_dbl(matchups, ~.$awayTeamScores[[1]][1])
    awayTeamScoresExra <- purrr::map_dbl(matchups, ~.$awayTeamScores[[1]][2])
    homeTeamScores <- purrr::map_dbl(matchups, ~.$homeTeamScores[[1]][1])
    homeTeamScoresExra <- purrr::map_dbl(matchups, ~.$awayTeamScores[[1]][2])


    awayTeam <- lapply(matchups, "[[", 4)
    awayTeamLocation <- lapply(awayTeam, "[", c("teamLocation"))
    # Or, like above...
    # awayTeamLocation <- purrr::map_chr(awayTeam, ~.$teamLocation)
    awayTeamNickname <- lapply(awayTeam, "[", c("teamNickname"))

    homeTeam <- lapply(matchups, "[[", 6)
    homeTeamLocation <- lapply(homeTeam, "[", c("teamLocation"))
    homeTeamNickname <- lapply(homeTeam, "[", c("teamNickname"))

    awayTeamScores <- .unlistname(awayTeamScores)
    homeTeamScores <- .unlistname(homeTeamScores)

    awayTeamLocation <- .unlistname(awayTeamLocation)
    awayTeamNickname <- .unlistname(awayTeamNickname)

    homeTeamLocation <- .unlistname(homeTeamLocation)
    homeTeamNickname <- .unlistname(homeTeamNickname)

    n_wks_extra <- sum(!is.na(awayTeamScoresExra))
    n_wks_plyffs_total <- n_wks_plyffs * n_wks_plyffs_rnd
    # stopifnot(n_wks_plyffs != n_wks_extra)
    if (n_wks_extra > 0) {
      # msg <- "This league has multi-week playoffs."
      # message(msg)
      n_wks_total <- n_wks_reg + n_wks_plyffs_total
      awayTeamScoresExra <- awayTeamScoresExra[!is.na(awayTeamScoresExra)]
      homeTeamScoresExra <- homeTeamScoresExra[!is.na(homeTeamScoresExra)]
      awayTeamScores <- c(awayTeamScores, awayTeamScoresExra)
      homeTeamScores <- c(homeTeamScores, homeTeamScoresExra)
      .fix_wks_ff_partial <-
        purrr::partial(
          .fix_wks_ff,
          idx1 = n_wks_reg,
          idx2 = n_wks_total,
          times = n_wks_plyffs_rnd
        )
      awayTeamLocation <- .fix_wks_ff_partial(awayTeamLocation)
      homeTeamLocation <- .fix_wks_ff_partial(homeTeamLocation)
      awayTeamNickname <- .fix_wks_ff_partial(awayTeamNickname)
      homeTeamNickname <- .fix_wks_ff_partial(homeTeamNickname)
    }

    data <-
      tibble::tibble(
        week = seq(1, n_wks_total, by = 1),
        seasontype = c(rep("regular", n_wks_reg), rep("playoffs", n_wks_plyffs_total)),
        awayTeamLocation = awayTeamLocation,
        awayTeamNickname = awayTeamNickname,
        homeTeamLocation = homeTeamLocation,
        homeTeamNickname = homeTeamNickname,
        awayTeamScore = awayTeamScores,
        homeTeamScore = homeTeamScores
      )

    data
  }

.parse_ff_byyear <-
  function(cont) {

    # n_tms <- sum(cont$leaguesettings$divisions$size)
    n_tms <- cont$leaguesettings$size
    # n_tms_plyffs <- cont$leaguesettings$playoffTeamCount
    wk_reg_last <- cont$leaguesettings$finalRegularSeasonMatchupPeriodId
    wk_plyffs_last <- cont$leaguesettings$finalScoringPeriodId
    n_wks_plyffs_rnd <- cont$leaguesettings$playoffMatchupLength

    n_wks_plyffs <- (wk_plyffs_last - wk_reg_last) / n_wks_plyffs_rnd

    data <-
      purrr::map_dfr(
      # map_dfr(
        cont$leaguesettings$teams,
        ~.parse_ff_byyear_bytm(
          tm = .x,
          n_wks_reg = wk_reg_last,
          n_wks_plyffs_rnd = n_wks_plyffs_rnd,
          n_wks_plyffs = n_wks_plyffs
          )
      )
    data

  }

#' Parse fantasy football data
#'
#' @description Parse fantasy football data.
#' @details
#' None.
#' @inheritParams parse_scores
#' @return tibble
#' @export
#' @rdname parse_ff
#' @seealso \url{https://dusty-turner.netlify.com/post/mathlete-fantasy-football-analysis/}.
parse_ff <-
  function(resp,
           as = c("complete", "minimal"),
           ...) {

    # chkDots(...)
    as <- .validate_as(as)
    cont_raw <- rawToChar(resp$content)
    cont <- jsonlite::fromJSON(cont_raw)

    if(as == "minimal") {
      data <- tibble::enframe(unlist(cont))
    } else if(as == "complete") {
      data <- .parse_ff_byyear(cont)
    }

    data <- .add_class(data, "ff", as)
    data
  }


print.ff_minimal <-
  function(x, ...) {
    # msg <- sprintf("<Minimally parsed.>\n")
    # cat(msg, sep = "")
    x
    invisible(x)
  }

print.ff_complete <-
  function(x, ...) {
    # msg <- sprintf("<Completely parsed.>\n")
    # cat(msg, sep = "")
    x
    invisible(x)
  }

