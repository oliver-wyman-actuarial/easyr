test_that("works as expected", {

  x = data.frame( c1 = c( 1, 2, 4 ), c2 = c( 3, 5, 7 ), c3 = c( 'a', 'b', 'c' ), stringsAsFactors = TRUE )
  
  expect_equal( cc( x$c1, x$c2, sep = '-' ), c( "1-3", "2-5", "4-7" ) )
  expect_equal( cc( x$c1, x$c2 ), c( "13", "25", "47" ) )
  expect_equal( cc( 1, 2, 4 ), "124" )
  
  # Special case where a single vector is sent.
  expect_equal( cc( c( '1', '2', '3' ), sep = ', ' ), '1, 2, 3' )
  
  expect_equal( cc( c( '1', NA, '3' ), sep = ', ' ), NA )
  expect_equal( cc( c( '1', NA, '3' ), c( '1', NA, '3' ), sep = ', ' ), c( '1, 1', NA, '3, 3' ) )
  
  # NULL handling (fails currently, need to fix it..)
  #expect_equal( cc(NULL, 'hello', NULL, sep = '; '), 'hello' )
  
  rm(x)

})