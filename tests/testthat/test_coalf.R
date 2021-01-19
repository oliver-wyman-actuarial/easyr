test_that("works as expected", {

  x = data.frame( c3 = factor( c( 'A', NA, 'C' ) ), c4 = factor( c( 'B', 'B', 'B' ) ), stringsAsFactors = TRUE )
  
  expect_equal(
    as.character( coalf( x$c3, x$c4 ) ),
    c( 'A', 'B', 'C' )
  )

})
  