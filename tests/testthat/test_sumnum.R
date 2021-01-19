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
  
  # round to prevent noLD errors.
  dt = iris
  for(i in 1:4) dt[[i]] = round(dt[[i]]*100,0)
  rm(i)
  expect_equal(
    sumnum( dplyr::group_by( dt, Species ), except = 'Sepal.Length' ),
    dplyr::summarize_at( dplyr::group_by( dt, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) ),
    tolerance = .01
  )
  
  # fails noLD:
  expect_equal(
    sumnum( dplyr::group_by( dt, Species ), except = 1 ),
    dplyr::summarize_at( dplyr::group_by( dt, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ), list(sum) ),
    tolerance = .01
  )
  
  # fails noLD:
  expect_equal(
    sumnum( dplyr::group_by( dt, Species ), except = c( TRUE, FALSE, FALSE, FALSE ) ),
    dplyr::summarize_at( dplyr::group_by( dt, Species ), c( 'Sepal.Width', 'Petal.Length', 'Petal.Width' ),list(sum) ),
    tolerance = .01
  )

})