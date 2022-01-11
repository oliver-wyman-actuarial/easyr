test_that("works as expected", {

  # data frame.
  ecopy( iris, showrowcolnames = "cols", show = 'show' )
  ecopy( iris )

  # string.
  ecopy( 'hello' )
   
  # reading from the clipboard is not always possible.
  result = tryCatch({
      readClipboard(format = 1, raw = FALSE)
    },
    error = function(e) {}
  )
  if(!is.null(result)) expect_equal(result, 'hello')
    
  # errors.
  expect_error( { ecopy( iris, showrowcolnames = "wrong", show = 'show' ) }, 'should be one of' )

})