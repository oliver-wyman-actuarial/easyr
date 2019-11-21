context( "astext" )

test_that( "works as expected", {

  expect_equal(
    astext( c( 1, 2, 4 ) ),
    "c( 1, 2, 4 )"
  )

  expect_equal(
    astext( c( 'a', 'b', 'c' ) ),
    "c( 'a', 'b', 'c' )"
  )

})