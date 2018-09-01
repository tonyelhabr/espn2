
testthat::context("test-scores")

testthat::test_that("NFL scores are retrieved successfully", {

  wk <- 1
  stype <- 2
  yr <- 2017
  url1a <- make_url_scores_nfl(week = wk, seasontype = stype, year = yr)
  res1a <- request_scores(url = url1a)
  scores1a <- get_scores_nfl(url = url1a)
  scores1a

  scores1b <- get_scores_nfl(week = wk, seasontype = stype, year = yr)

  scores1c <-
    get_scores(league = "nfl", url = url1a, depth = "all")
  scores1d <-
    get_scores_nfl(url = url1a, depth = "all")
  scores1e <-
    get_scores(league = "nfl", week = wk, seasontype = stype, year = yr, depth = "all")

  testthat::expect_equal(scores1a, scores1b)
  testthat::expect_equal(scores1a, scores1c)
  testthat::expect_equal(scores1a, scores1d)
  testthat::expect_equal(scores1a, scores1e)

})
