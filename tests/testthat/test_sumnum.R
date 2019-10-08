context( "sumnum" )

test_that( "works as expected", {

  expect_equal(
    sumnum( iris, na.rm = TRUE ),
    dplyr::summarize_at( iris, c( 'Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) )
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), do.fun = sum, na.rm = TRUE ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) )
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), except = 'Sepal.Length' ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) )
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), except = 1 ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) )
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), except = c( TRUE, FALSE, FALSE, FALSE ) ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ),list(sum) )
  )

})