test_that("works as expected", {
  
  # Errors.
  # No errors are currently implemented for this function.
  #expect_error( { ecopy( iris, showrowcolnames = "wrong", show = 'show' ) }, 'Value passed for .+ is not acceptable' )
  
  # Other tests.
  iris$bin <- binbyvol( iris, 'Sepal.Width', 'Sepal.Length', 5 )
  betterints = getbetterint( iris$bin ) # runs without error.
  expect_equal( length( iris$bin ), length( betterints ) ) # vector length should be the same.
  expect_equal( length( unique( iris$bin ) ), length( unique( betterints ) ) ) # number of buckets (unique values) should be the same.
  
})