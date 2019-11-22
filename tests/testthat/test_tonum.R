context("tonum")

test_that("tonum works as expected", {
  
  expect_equal( tonum( c('123', '$50.02', '30%', '(300.01)', NA, "3.7999999999999999E-2"), nazero = TRUE ), c(123.00, 50.02, 0.30, -300.01, 0.00, 0.038) )
  expect_equal( tonum( c('123', '$50.02', '30%', '(300.01)', NA), nazero = FALSE ), c(123.00, 50.02, 0.30, -300.01, NA) )
  expect_equal( tonum('1 230.4'), 1230.4)

  expect_equal( 
    is.integer(
      tonum( as.factor( c( "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "NA" ) ) )
    ),
    TRUE
  )
  
  # Strong conversion.
  expect_equal( tonum( c( 'Round 3', 'Number 2' ), remove.chars = TRUE ), c( 3, 2 ) )
  
  # Strong conversion still handles scientific notation.
  expect_equal( tonum( c( '1e2', '1e-2' ), remove.chars = TRUE ), c( 1e+02,  1e-02 ) )
  
  # Numbers that like to turn into dates.
  expect_equal( 
    tonum( c( '9976.7012', '9513.42502332815', '7672.45790613718', '5412.18875494071' ) ),
    c( 9976.7012, 9513.42502332815, 7672.45790613718, 5412.18875494071 )
  )
  
  expect_equal(
    tonum( c( '$(3,891)', '$(0)' ) ),
    c( -3891, 0 )
  )

  expect_equal( 
    is.integer( tonum( as.numeric( c( 1, 2, 4 ) ) ) ),
    TRUE
  )

  # multipliers  
  expect_equal(
    tonum( c( '$(3,891)M', '$4B', '3.41K', '30', '40%' ) ),
    c( -3891 * 1000^2, 4 * 1000^3, 3.41* 1000, 30, 0.40 )
  )
    
})