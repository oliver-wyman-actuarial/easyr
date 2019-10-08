context("spl")

test_that("works as expected", {
  
  expect_equal( nrow(spl(cars,10)), 10 )
  expect_equal( all(c('speed','dist') %in% colnames(spl(cars,30))), T )
  expect_equal( nrow( spl( iris ) ) > 0, TRUE )
  expect_equal( length( spl( c(1:100), 50 ) ), 50 )
  expect_equal( suppressWarnings( length( spl( c(1:100), 200 ) ) ), 100 )

  expect_equal( nrow( spl( cars, 0.1 ) ), ceiling( 0.1 * nrow(cars) ) )
  expect_equal( length( spl( cars$speed, 0.1 ) ), ceiling( 0.1 * nrow(cars) ) )
  
  expect_warning( spl( cars, 60 ), '[60]' )
  expect_warning( spl( cars$speed, 60 ), '[60]' )
  
})