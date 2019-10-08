context( "fldict" )

test_that("works as expected", {
  
  files = list.files( test_file( '' ), full.names = TRUE )[1:4]
  files = files[ !grepl( '~[$]', files ) ]
  t = fldict( file.list = files )
  
  expect_equal( length(t) > 0, TRUE )

})