test_that( "works as expected", {
  
  x = dplyr::summarize_at( dplyr::group_by( iris, Species ), dplyr::vars( Sepal.Length, Sepal.Width ), list(sum) )
  tx = tcol( x, 'Species' )
  expect_equal( tx$col, c( 'Sepal.Length', 'Sepal.Width' ) )
  expect_equal( tx$setosa, c( 250.3, 171.4 ) )

})