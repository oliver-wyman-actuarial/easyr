test_that("checkeq works as expected", {
  
  expect_output( checkeq( expected = 100, actual = 100, desc = 'A Match' ) , 'success' )
  expect_output( checkeq( expected = 100, actual = 0, desc =' A Match', acceptable_pct_diff = 1 ) , 'success' )
  expect_error( checkeq( expected = 'hello',actual = 'hello',desc = 'A Match'), 'expected.+character' )
  expect_error( checkeq( expected = 0, actual = FALSE ,desc='A Match'), 'actual.+logical' )
  
  expect_error( checkeq( expected = 100 , actual = 0, desc = 'A Match' ), 'ERROR' )

})