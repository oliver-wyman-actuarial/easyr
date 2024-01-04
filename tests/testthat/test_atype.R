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
    
  # test to ensure sampling makes the algorithm run faster. 
  large_Dataset = data.frame(
    num = as.character(1:100*1000),
    chars = cc('this is a character', 1:100*100),
    stringsAsFactors = FALSE
  )

  # get duration without sampling (all the rows).
  time0 = Sys.time()
  at = atype(large_Dataset, use_n_sampled_rows  = nrow(large_Dataset))
  time0 = Sys.time() - time0
  expect_equal (nrow(at), nrow(large_Dataset)) # verify row count did not change. 
  expect_equal(class(at$num), 'integer' ) # verify correct conversion.

  # get duration withsampling.
  time1 = Sys.time()
  at = atype(large_Dataset)
  time1 = Sys.time() - time1
  expect_equal (nrow(at), nrow(large_Dataset)) # verify row count did not change. 
  expect_equal(class(at$num), 'integer' ) # verify correct conversion.

  # verify time was reduced. 
  expect_condition((time0 - time1) / time0 > 0.05)  # reduction > 5%. 

  # numbers that should be converted to dates.
  x = data.frame(dateval = c(
      "44805", "45108", "45078", "44866", "44805", "45078", "45261", "45261", "45261", "45261", "45261", "45261", "45261",
      "45181", "45181", "45181", "45181", "45181", "45147", "45147", "45147", "45147", "45147"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(class(atype(x)$dateval), 'Date')

})

