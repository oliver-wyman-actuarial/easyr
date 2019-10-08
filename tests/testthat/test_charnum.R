context("charnum")

test_that("works as expected", {

  expect_equal( charnum( c( '123', '305%' ) ), TRUE )

})