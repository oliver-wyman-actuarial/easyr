context("binbyvol")

test_that("works as expected", {
  
  # Errors.
  expect_error( { iris$bin <- binbyvol( iris, 'Sepal.Widt', 'Sepal.Length', 5 ) }, 'not found' )
  expect_error( { iris$bin <- binbyvol( iris, 'Sepal.Width', 'Sepal.Lengt', 5 ) }, 'not found' )
  expect_error( { iris$bin <- binbyvol( iris, 'Species', 'Sepal.Length', 5 ) }, 'column must be numeric' )
  expect_error( { iris$bin <- binbyvol( iris, 'Sepal.Width', 'Sepal.Length', 'hello' ) }, 'argument must be a whole number' )
  
  # Other tests.
  iris$bin <- binbyvol( iris, 'Sepal.Width', 'Sepal.Length', 5 ) # runs without error.
  expect_equal( length( unique( iris$bin ) ), 5 ) # number of buckets (unique values) should be the same.
  
})