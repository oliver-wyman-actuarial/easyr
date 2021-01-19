test_that("works as expected", {
  
  expect_equal( 
    xldate( c('42768', '42769', '47545', NA) ), 
    lubridate::ymd( c( '2017-02-02', '2017-02-03', '2030-03-03', NA ) )
  )
  
  # Mix of excel and string dates.
  expect_equal(
    xldate( 
      c( "42976", "42724", "42975", "42972", "42975", "1/6/2014", "1/3/2014", "1/23/2014", "12/6/2013", "1/13/2014"),
      ifna = 'return-na',
      verbose = FALSE
    ),
   lubridate::mdy( c( '8/29/2017', '12/20/2016', '8/28/2017', '8/25/2017', '8/28/2017', NA, NA, NA, NA, NA ) )
 )

  # Numeric dates should not be recognized as excel.
  expect_equal( 
    xldate( c( "19940121", "20020222", "20000127", "20010912" ), min.acceptable = NULL, max.acceptable = NULL,  ifna = 'return-unchanged', verbose = FALSE ),
    c( "19940121", "20020222", "20000127", "20010912" )
  )   
  
})
  