test_that( "works as expected", {

  x = data.frame(
    c1 = c( 'a', 'b', 'b', 'c' ),
    stringsAsFactors = FALSE
  )

  expect_equal( is.character( x$c1 ), TRUE )

  x = char2fac(x)

  expect_equal( is.character( char2fac(x$c1 ) ), FALSE )
  expect_equal( is.factor( x$c1 ), TRUE )
  expect_equal( levels( x$c1 ), c( 'a', 'b', 'c' ) )

  x = fac2char(x)

  expect_equal( is.character( x$c1 ), TRUE )
  
  # Now test out-of-ordered.
  
  x1 = data.frame(
    c1 = c( 'a', 'c', 'b', 'b' ),
    stringsAsFactors = FALSE
  )
  
  x2 = char2fac(x1)
  
  expect_equal( levels( x2$c1 ), c( 'a', 'c', 'b') )
  
  x3 = char2fac( x1, sortlevels = TRUE )
  
  expect_equal( levels( x3$c1 ), c( 'a', 'b', 'c' ) )

  # NA level.

    x = char2fac( data.frame(
      c1 = c( 'a', 'b', NA, 'c' ),
      stringsAsFactors = FALSE
    ) )
    
    expect_equal( '(Missing)', levels(x$c1)[ x$c1[3] ] )

    x = char2fac( data.frame(
      c1 = c( 'a', 'b', NA, 'c' ),
      stringsAsFactors = FALSE
    ), na_level = '1236' )
    
    expect_equal( '1236', levels(x$c1)[ x$c1[3] ] )
    
    x = char2fac( data.frame(
      c1 = c( 'a', 'b', NA, 'c' ),
      stringsAsFactors = FALSE
    ), na_level = NULL )
    
    expect_equal( TRUE, is.na( x$c1[3] ) )

})
