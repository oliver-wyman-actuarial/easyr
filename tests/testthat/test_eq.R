test_that("works as expected", {

  expect_equal( eq( c(NA,'NA',1,2,'c'), c(NA,NA,1,2,'a') ), c( TRUE, FALSE, TRUE, TRUE, FALSE ) )
  expect_true( eq( NA, NULL ) )

  # different lenghts.
  expect_equal( eq( c('a', 'b'), c('a', 'b', 'c') ), FALSE)

})