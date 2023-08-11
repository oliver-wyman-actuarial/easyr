test_that( "works as expected", {

  t = data.frame(
    
    date = c( '2014-01-01', '1/1/2014', '20140101', NA ),
    character = c( 'A', 'B', 'C', NA ),
    numeric = c( '-13', '20%', '$30.00', NA ),
    logical = c( 'TRUE', 'FALSE', '0', NA ),
    na_column = c(NA, NA, NA, NA),
    
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
    
  large_matrix = matrix(0, 100000, 100)
  large_Dataset = as.data.frame(large_matrix)
  
  ptm = proc.time()
  at = atype(large_Dataset)
  expect_equal (nrow(at), nrow(large_Dataset) )
  expect_equal( class(large_Dataset$V1), 'numeric' )
  proc.time() - ptm

})

# find atype error when scan-data-lighthouse dict() calls atype()





