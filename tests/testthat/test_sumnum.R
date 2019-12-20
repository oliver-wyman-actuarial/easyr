context( "sumnum" )

test_that( "works as expected", {
  
  eps <- if (capabilities("long.double"))
    sqrt(.Machine$double.eps) else
      0.1

  expect_equal(
    sumnum( iris, na.rm = TRUE ),
    dplyr::summarize_at( iris, c( 'Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) ),
    tolerance = .0001
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), do.fun = sum, na.rm = TRUE ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) ),
    tolerance = .0001
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), except = 'Sepal.Length' ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) ),
    tolerance = .0001
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), except = 1 ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) ),
    tolerance = .0001
  )
  
  expect_equal(
    sumnum( dplyr::group_by( iris, Species ), except = c( TRUE, FALSE, FALSE, FALSE ) ),
    dplyr::summarize_at( dplyr::group_by( iris, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ),list(sum) ),
    tolerance = .0001
  )

})