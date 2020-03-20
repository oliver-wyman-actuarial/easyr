context("nanull")

test_that("works as expected", {

  # TRUE tests.
  expect_equal( nanull(NULL) , TRUE )
  expect_equal(nanull(NA) , TRUE )
  expect_equal( nanull(c(NA,NULL)) , TRUE )
  expect_equal( nanull(c()) , TRUE )
  expect_equal( nanull(data.frame(stringsAsFactors = TRUE) ), TRUE )
  expect_equal( nanull(c(NA,2,3)) , TRUE )
  expect_equal( nanull( "#VALUE!" ) , TRUE )
  expect_equal( nanull( "NA" ) , TRUE )
  expect_equal( nanull( "" ) , TRUE )

  # FALSE tests.
  expect_equal( nanull(c(1,2,3)) , FALSE )
  expect_equal( nanull(c(1,2,NA)) , FALSE )
  expect_equal( nanull(c(NULL,2,3)) , FALSE )
  expect_equal( nanull( data.frame(v1=character(), v2=numeric(), stringsAsFactors = TRUE) ) , FALSE ) # in the past this has incorrectly categorized an empty data frame as null.
  expect_equal( nanull( dplyr::group_by( dplyr::select( cars, speed, dist ), speed ) ), FALSE ) # test a tibble.
  
  expect_equal( 
    nanull( 
      list( 
        data.frame( some.data = c( 1, 2, 3, NA ), stringsAsFactors = TRUE),
        data.frame( some.data = c( 1, 2, 3, NA ), stringsAsFactors = TRUE)
      )
    ), 
    FALSE 
  ) # 1 NA in a data frame shouldn't cause failure.

})