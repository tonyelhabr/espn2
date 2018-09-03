
testthat::context("test-scores")

testthat::test_that("NFL scores for week 1 in the 2017 season are retrieved.", {

  lg <- "nfl"
  wk <- 1
  stype <- 2
  yr <- 2017
  url1 <- make_url_scores_nfl(week = wk, seasontype = stype, year = yr)
  url1
  resp1 <- request_scores_nfl(url = url1)
  resp1
  class(resp1)
  scores_nfl1 <- parse_scores_nfl(resp = resp1)
  scores_nfl1

  # NOTE: Commenting out possible tests just to reduce "burden" of API calls.
  # scores_nfl2 <-
  #   get_scores(league = lg, url = url1)
  # testthat::expect_equal(scores_nfl1, scores_nfl2)
  # scores_nfl2 <- get_scores_nfl(url = url1)
  # testthat::expect_equal(scores_nfl1, scores_nfl2)
  # scores_nfl2 <-
  #   get_scores(league = lg, resp = resp1)
  # testthat::expect_equal(scores_nfl1, scores_nfl2)
  # scores_nfl2 <- get_scores_nfl(resp = resp1)
  # testthat::expect_equal(scores_nfl1, scores_nfl2)
  # scores_nfl2 <-
  #   get_scores(league = lg, week = wk, seasontype = stype, year = yr)
  # testthat::expect_equal(scores_nfl1, scores_nfl2)
  scores_nfl2 <- get_scores_nfl(week = wk, seasontype = stype, year = yr)

  testthat::expect_is(scores_nfl1, "tbl")
  testthat::expect_is(scores_nfl2, "tbl")
  testthat::expect_equal(scores_nfl1, scores_nfl2)

  scores_nfl1_min <- parse_scores_nfl(resp = resp1, as = "minimal")
  scores_nfl1_min
  testthat::expect_is(scores_nfl1_min, "tbl")
  testthat::expect_false(nrow(scores_nfl1) == nrow(scores_nfl1_min))
  testthat::expect_false(ncol(scores_nfl1) == ncol(scores_nfl1_min))

})

# testthat::test_that("NFL scores for 2018 are retrieved.", {
#
#   lg <- "nfl"
#   wk <- NULL
#   stype <- 2
#   yr <- 2018
#   url1 <- make_url_scores_nfl(seasontype = stype, year = yr, week = NULL, limit = 500)
#   resp1 <- request_scores_nfl(url = url1)
#   resp1
#   class(resp1)
#   scores_nfl1 <- parse_scores_nfl(resp = resp1)
#   scores_nfl1
#
# })
