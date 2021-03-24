test_that("todate works as expected", {
  
    expect_equal( 
      todate( 
        c( "ABC Company 2-28-17.csv", "ABC Company Feb 2017 AIG.csv", "ABCCompany_053117.csv" ), 
        aggressive.extraction = TRUE, ifna = 'return-na', do.month.char = FALSE, verbose = FALSE 
      ),
      lubridate::mdy( '2-28-17', NA, '5-31-17' )
    )
    
    expect_equal( all(eq(
        todate( c( '20171124', '2017/12/24', NA, '12/24/2017', '5/11/2017 1:51PM' ) ), 
        c(lubridate::ymd("2017-11-24"),lubridate::ymd("2017-12-24"),NA,lubridate::ymd("2017-12-24"), lubridate::ymd( '2017-5-11') )
    )), T)
  
    # Time options.
    expect_equal( max( lubridate::hour( 
      todate(
        c("12/30/2017 10:09AM", "12/30/2017 11:10AM", "12/29/2017 2:25PM",  "12/29/2017 9:15AM",  "12/29/2017 3:00PM" ), 
        allow_times = TRUE 
      ) ) ), 15 
    )
    
    # Mix of excel and string dates.
    expect_equal(
      todate( c( "42976", "42724", "42975", "42972", "42975", "1/6/2014", "1/3/2014", "1/23/2014", "12/6/2013", "1/13/2014" ) ),
      lubridate::mdy( c( '8/29/2017', '12/20/2016', '8/28/2017', '8/25/2017', '8/28/2017', "1/6/2014", "1/3/2014", "1/23/2014", "12/6/2013", "1/13/2014" ) )
    )

    expect_equal( todate( c( '3312019', '40909' ) ), lubridate::mdy( c( '3-31-2019', '1/1/2012') ) )
    expect_equal( todate( '2019.1.3' ), lubridate::mdy('01-03-2019') )
    expect_equal( todate( '1.3.2019' ), lubridate::mdy('01-03-2019') )
    expect_equal( todate( '10/1/06' ), lubridate::mdy('10-01-2006') )
    
    # Try filename with a date in it.
    expect_equal( todate( 'ABC Company as of 2-28-18.xlsx' ), lubridate::ymd( '2018-02-28' ) )
    expect_equal( todate( 'ABC Company as of 2-28-18.xlsx', aggressive.extraction = FALSE, verbose = FALSE ), 'ABC Company as of 2-28-18.xlsx' )
      
    # Special inconsistent date format for ambiguous dates issue reported by Cal.
    expect_equal( 
      todate( c( '12-4-2018', '12-20-2018' ) ), # output for Cal was  "2018-04-12" "2018-12-20"
      lubridate::mdy( c( '12-4-2018', '12-20-2018' ) )
    )

    # Numeric dates should not be recognized as excel.
    expect_equal( 
      todate( c( "19940121", "20020222", "20000127", "20010912" ), min.acceptable = NULL, max.acceptable = NULL ),
      lubridate::ymd( c( "19940121", "20020222", "20000127", "20010912" ) )
    )    

})