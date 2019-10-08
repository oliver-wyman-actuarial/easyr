context("tcmsg")

test_that("tcmsg works as expected", {
  
  expect_error( tcmsg({ NULL = 1 },'Cannot assign to NULL','variable') , 'Cannot assign to NULL variable' )
  expect_warning(  tcmsg({ as.numeric('abc') },'Issue in as.numeric()') , 'Issue in as.numeric()' )

})