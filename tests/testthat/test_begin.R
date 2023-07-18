test_that("works as expected", {

  # just test that it runs without error.
  expect_output( begin( wd = FALSE ), 'Disabled scientific notation' ) 
  
  expect_output( begin( wd = FALSE, runpath = test_file('script.R') ), 'Hello World!' )
  
  expect_output( begin( wd = FALSE, runpath = paste0(test_file(''), '/test-directory' )), 'Hello World!' )

})