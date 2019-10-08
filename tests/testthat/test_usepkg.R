context("usepkg")

test_that("works as expected", {
  
  usepkg( c( 'plyr','lubridate' ) )
  expect_equal( TRUE, TRUE ) # test you get this far with out the prior line bugging out.
  
})