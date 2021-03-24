test_that("works as expected", {

  expect_equal( TRUE, na(NA) )
  expect_equal( FALSE, na(1) )
  
  expect_equal( TRUE, null( NULL ) )
  expect_equal( FALSE, null(1) )
  
  expect_equal( TRUE, nan( NaN ) )
  expect_equal( FALSE, nan(1) )
  
  expect_equal( TRUE, isnum(1) )
  expect_equal( FALSE, isnum( factor( c( 'a', 'b', 'c' ) ) ) )
  
  expect_equal( TRUE, isdate( lubridate::mdy( '10/1/2014' ) ) )
  expect_equal( FALSE, isdate(1) )
  
  expect_equal( TRUE, isfac( factor( c( 'a', 'b', 'c' ) ) ) )
  expect_equal( FALSE, isfac(1) )
  
  expect_equal( TRUE, ischar( 'a character' ) )
  expect_equal( FALSE, ischar(1) )
  
  expect_equal( '1', tochar(1) )
  expect_equal( tochar( factor( c( 'a', 'b' ) ) ), c( 'a', 'b' ) )

})