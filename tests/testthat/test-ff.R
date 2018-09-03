
testthat::context("ff")

testthat::test_that("Fantasy football leagues data is retrieved.", {

  lid <- 453218
  yr <- 2017
  url_ff <- make_url_ff(leagueId = lid, year = yr)
  resp_ff <- request_ff(url = url_ff)
  resp_ff
  ff <- parse_ff(resp = resp_ff)
  testthat::expect_is(ff, "tbl")

  ff_min <- parse_ff(resp = resp_ff, as = "minimal")
  testthat::expect_is(ff_min, "tbl")

  testthat::expect_false(nrow(ff) == nrow(ff_min))
  testthat::expect_false(ncol(ff) == ncol(ff_min))
})


