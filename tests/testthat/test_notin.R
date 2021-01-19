test_that("works as expected", {

  expect_equal( c(1,3,11) %ni% 1:10, c( FALSE, FALSE, TRUE ) )

})