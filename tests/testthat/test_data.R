context("data")

test_that("states", {
  expect_equal( nrow(states) > 1, TRUE )
})

test_that("cblind", {
  expect_equal( length(cblind) > 1, TRUE )
})

