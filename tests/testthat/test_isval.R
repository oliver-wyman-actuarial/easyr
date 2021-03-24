test_that("works as expected", {

  # True tests.
  expect_equal( isval(NULL) , FALSE )
  expect_equal(isval(NA) , FALSE )
  expect_equal( isval(c(NA,NULL)) , FALSE )
  expect_equal( isval(c()) , FALSE )
  expect_equal( isval(data.frame(stringsAsFactors = TRUE) ), FALSE )
  expect_equal( isval(c(NA,2,3)) , FALSE )
  expect_equal( isval( "#VALUE!" ) , FALSE )
  expect_equal( isval( "NA" ) , FALSE )
  expect_equal( isval( "" ) , FALSE )

  # False tests.
  expect_equal( isval(c(1,2,3)) , TRUE )
  expect_equal( isval(c(1,2,NA)) , TRUE )
  expect_equal( isval(c(NULL,2,3)) , TRUE )
  expect_equal( isval( data.frame(v1=character(), v2=numeric(), stringsAsFactors = TRUE) ) , TRUE ) # in the past this has incorrectly categorized an empty data frame as null.
  expect_equal( isval(  dplyr::group_by( dplyr::select( cars, speed, dist ), speed ) ), TRUE ) # test a tibble.

})