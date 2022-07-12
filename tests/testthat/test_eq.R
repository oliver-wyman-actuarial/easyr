test_that("works as expected", {

  expect_equal( eq( c(NA,'NA',1,2,'c'), c(NA,NA,1,2,'a') ), c( TRUE, FALSE, TRUE, TRUE, FALSE ) )
  expect_true( eq( NA, NULL ) )
  
  #null check
  expect_equal( eq(NULL, NULL), TRUE)

  # different lengths.
  expect_error( eq( c('a', 'b'), c('a', 'b', 'c') ), 'Not a valid comparison.')

})
