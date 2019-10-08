context("left")

test_that("works as expected", {
  expect_equal( left('leftright',4) , 'left' )
  expect_equal( left('four',5), 'four' )
})