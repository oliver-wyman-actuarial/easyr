test_that( 'basic has works properly', {
  
  expect_equal(
    hashfiles( 
      test_file( 'null-columns.xlsx' ),
      full.hash = TRUE
    ),
    'c770eb0f9c224b9c342ee460154a2b09'
  )
  
})

test_that( 'missing files are handled properly', {
  
  expect_equal(
    hashfiles( c( test_file( 'null-columns.xlsx' ), 'this-file-doesnt-exist.R' ), skip.missing = TRUE, full.hash = TRUE ),
    'c770eb0f9c224b9c342ee460154a2b09'
  )
  
  expect_error(
    hashfiles( c( test_file( 'null-columns.xlsx' ), 'this-file-doesnt-exist.R' ), full.hash = TRUE ),
    'File not found:'
  )
  
})

test_that( "handle entire folder", {

    
  # folder. full hash.
      
    expect_equal(
      hashfiles( test_file(''), full.hash = TRUE ),
      'cd203cd34681657c280708c0bbf5b1c0'
    )
  
})

