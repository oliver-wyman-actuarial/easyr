context("right")

test_that("works as expected", {
  expect_equal( right('leftright',5) , 'right' )
  expect_equal( right('four',5), 'four' )
})