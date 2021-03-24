test_that("works as expected", {

  expect_equal(
    ddiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2019' ) ),
    31
  ) 
  
  expect_equal(
    ddiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2019' ), unit = 'month' ),
    1
  ) 
  expect_equal(
    mdiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2019' ) ),
    1
  )  
  
  expect_equal(
    ddiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2020' ), unit = 'month' ),
    13
  )  
  expect_equal(
    mdiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2020' ) ),
    13
  )  
  
  expect_equal(
    ddiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2020' ), unit = 'year' ),
    1
  )  
  expect_equal(
   ydiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2020' ) ),
   1
  )  
  
  expect_equal(
    ddiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2020' ), unit = 'quarter' ),
    4
  )  
  
  expect_equal(
    qdiff( lubridate::mdy( '1/1/2019' ), lubridate::mdy( '2/1/2020' ) ),
    4
  )  

})