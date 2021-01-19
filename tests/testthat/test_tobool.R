test_that( "works as expected", {
  
  x = c( 'TRUE', 'FALSE', '0', NA, 'not a boolean', 'NA' )
  
  expect_equal( tobool( x, verbose = FALSE ), x )
  
  expect_warning( asbool <- tobool( x, ifna = 'warning' ), 'NAs were created' )
  expect_equal( asbool, c( TRUE, FALSE, FALSE, NA, NA, NA ) )
  
  expect_error( asbool <- tobool( x, ifna = 'error' ),  'NAs were created' )
  
})