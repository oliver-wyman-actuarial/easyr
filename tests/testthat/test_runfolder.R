test_that("works as expected", {

  t = capture.output( runfolder( test_file('') ) )
  expect_true( cc( t, sep = '' ) != '' )

  # test edit.on.err
  t = capture.output( runfolder( test_file(''), edit.on.err = FALSE ) )
  expect_true( cc( t, sep = '' ) != '' )
  
  expect_error(
    runfolder( '/this/does/not/exist' ),
    'not found'
  )

})