test_that( 'basic has works properly', {
  
  expect_equal(
    hashfiles( 
      test_file( 'null-columns.xlsx' ),
      full.hash = TRUE
    ),
    '559904736dd3b0cb436d1ebbb7eb2657'
  )
  
})

test_that( 'missing files are handled properly', {
  
  expect_equal(
    hashfiles( c( test_file( 'null-columns.xlsx' ), 'this-file-doesnt-exist.R' ), skip.missing = TRUE, full.hash = TRUE ),
    hashfiles( 
      test_file( 'null-columns.xlsx' ),
      full.hash = TRUE
    )
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
      'cfb0402d201d6e513c362ba82031c1a7'
    )
  
})

