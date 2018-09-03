
testthat::context("ff")

testthat::test_that("Fantasy football leagues data is retrieved.", {

  lid <- 453218
  yr <- 2017
  url_ff1 <- make_url_ff(leagueId = lid, year = yr)
  resp_ff1 <- request_ff(url = url_ff1)
  resp_ff1
  ff1 <- parse_ff(resp = resp_ff1)
  testthat::expect_is(ff1, "tbl")

  ff2 <- get_ff(leagueId = lid, year = yr)

  testthat::expect_is(ff1, "tbl")
  testthat::expect_is(ff2, "tbl")
  testthat::expect_equal(ff1, ff2)

  ff1_min <- parse_ff(resp = resp_ff1, as = "minimal")
  testthat::expect_is(ff1_min, "tbl")

  testthat::expect_false(nrow(ff1) == nrow(ff1_min))
  testthat::expect_false(ncol(ff1) == ncol(ff1_min))
})


