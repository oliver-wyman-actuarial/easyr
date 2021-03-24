test_that("works as expected", {
  
  expect_equal(
    fmat( 1000, '$', with.unit = TRUE ),
    '$ 1 K'
  )
  
  expect_equal(
    fmat( 
      c( 
        NA,  7.51173709,  5.55555556,  3.96039604, 11.26760563,  NA,  0.00000000,  0.00556545,  5.00000000, 20.00000000,  0.66006601,
        3.92156863, 10.22333044,  6.62162162,  1.81818182,  3.35570470,  3.04347826, 20.38927130,  0.00000000,  1.40845070,  0.00000000,  5.23387595,
        5.10948905, 16.76646707,  1.43198091, 10.33634126,          NA, 10,.40988939,  6.87500000, 15.08553655,  4.40251572,  2.14424951, 11.93904762
      ), 
      '.', 
      digits = 4 
    )[2],
    ' 7.5117'
  )
  
  expect_equal(
    fmat( c( NA, 100 ), '$', with.unit = TRUE ),
    c( NA, '$ 100' )
  )
  
  expect_equal(
    fmat( 1000, '$', with.unit = TRUE, do.remove.spaces = TRUE ),
    '$1K'
  )

  expect_equal(
    fmat( c( 10, 100, 1000 ) , '$', digits = 0 ),
    c( '$    10', '$   100', '$ 1,000' )
  )
  
  expect_equal(
    fmat( 1000, '$' ),
    '$ 1,000'
  )
  
  expect_equal(
    fmat( 1000, 'dollars' ),
    '$ 1,000'
  )
  
  expect_equal(
    fmat( 1000, ',' ),
    '1,000'
  )
  
  expect_equal(
    fmat( 1000, 'dollar', digits = 2 ),
    '$ 1,000.00'
  )
  
  expect_equal(
    fmat( lubridate::mdy( '10/1/2018' ), 'mdy', do.date.sep = '-' ),
    '10-01-2018'
  )
  
  expect_equal(
    fmat( lubridate::mdy( '10/1/2018' ), 'ymd', do.date.sep = '/' ),
    '2018/10/01'
  )
  
  expect_equal(
    fmat( .15, '%' ),
    '15%'
  )

  # this caused an error in the past.
    
    expect_equal(
      fmat( 19790.06 ),
      '19,790'
    )

    expect_equal(
      fmat( '01/01/2018' ),
      '1/01/2018'
    )
    
    expect_equal(
      fmat( c( 19790.06, 19790.06 ) ),
      c( '19,790', '19,790' )
    )

    expect_equal(
      fmat( c( '01/01/2018', '01/01/2018' ) ),
      c( '1/01/2018', '1/01/2018' )
    )
    
  expect_equal(
    fmat( .1501, 'decimal' ),
    '0.1501'
  )
  
  expect_equal(
    fmat( .15, '.', digits = 3 ),
    '0.150'
  )
  
  expect_equal(
    fmat( c( 0.3852476, 0.4137442, 0.3816557 ), '.', digits = 2 ),
    c( '0.39', '0.41', '0.38' )
  )
  
  expect_equal(
    fmat( .1501 ),
    '15%'
  )

})
