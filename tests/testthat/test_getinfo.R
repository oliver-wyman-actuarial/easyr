test_that("works as expected", {
  
  op = capture.output( getinfo( iris, 'Sepal.Length' )  )
  op = capture.output( t <- getinfo( iris, 'Sepal.Length', display = FALSE ) ) # runs as expected.
  expect_equal( length( t ) > 10, TRUE )
  expect_equal( nrow( t ), 1 )
  
  op = capture.output( t <- getinfo( iris, 'Species', display = FALSE ) )
  expect_equal( t$`Col #`, 5 )
  expect_equal( t$`# Distincts`, 3 )
  
})
