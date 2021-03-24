test_that("works as expected", {

  # Duplicate the cars data.
  ddt = dplyr::bind_rows( cars, cars )
  
  # Add NA rows.
  for( i in c( 4, 6, 34, 15 ) ){
    ddt$speed[i] = NA
    ddt$dist[i] = NA
  }
  
  expect_equal( nrow( drows( ddt, ddt[[1]] ) ), 95 )
  expect_equal( nrow( drows( ddt, ddt[[1]], na = TRUE ) ), 95 + 4 )

})