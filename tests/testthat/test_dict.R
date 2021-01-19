test_that("works as expected", {
  
  t = capture.output( dict( iris,  ecopy = TRUE ) )
  expect_true( all( t != '' ) )

  expect_equal( nrow( dict( iris ) ), ncol( iris ) )
  
  t = capture.output( dict( iris, ecopy = TRUE ) )
  expect_true( all( t != '' ) )

  expect_equal( nrow( dict( iris ) ), ncol( iris ) )
  
})