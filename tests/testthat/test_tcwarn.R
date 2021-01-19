test_that("tcwarn works as expected", {
  
  expect_warning( tcwarn({ NULL = 1 },'Cannot assign to NULL','variable') , 'Cannot assign to NULL variable' )
  expect_warning(  tcwarn({ as.numeric('abc') },'Issue in as.numeric()') , 'Issue in as.numeric()' )

})