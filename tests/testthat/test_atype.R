test_that( "works as expected", {

  t = data.frame(
    
    date = c( '2014-01-01', '1/1/2014', '20140101' ),
    character = c( 'A', 'B', 'C' ),
    numeric = c( '-13', '20%', '$30.00' ),
    logical = c( 'TRUE', 'FALSE', '0' ),
    
    stringsAsFactors = FALSE
    
  )
  
  at = atype( t )
  
  expect_equal( class(at$date), 'Date' )
  expect_equal( class(at$character), 'character' )
  expect_equal( class(at$numeric), 'numeric' )
  expect_equal( class(at$logical), 'logical' )
  
  at = atype( t, stringsAsFactors = TRUE )
  
  expect_equal( class(at$date), 'Date' )
  expect_equal( class(at$character), 'factor' )
  expect_equal( class(at$numeric), 'numeric' )
  expect_equal( class(at$logical), 'logical' )

  # exclude.
  
    at = atype( t, exclude = 'date' )
    
    expect_equal( class(at$date), 'character' )
    expect_equal( class(at$character), 'character' )
    expect_equal( class(at$numeric), 'numeric' )
    expect_equal( class(at$logical), 'logical' )

})
