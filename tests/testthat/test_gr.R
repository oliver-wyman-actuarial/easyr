context("gr")

test_that("works as expected", {
  expect_equal( round(gr(),3) , 1.618 )
})