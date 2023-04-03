test_that("without buffer", {

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
  if(!is.null(result) && result != '??'){ # r-devel system seems to return '??' from readClipboard.

    expect_equal(result, 'hello')    

    # errors.
    expect_error( { ecopy( iris, showrowcolnames = "wrong", show = 'show' ) }, 'should be one of' )
    
  }

})

test_that("with buffer", {
  
  # data frame.
  ecopy( iris, showrowcolnames = "cols", show = 'show', buffer = 5000 )
  ecopy( iris )
  
  # string.
  ecopy( 'hello' )
  
  # reading from the clipboard is not always possible.
  result = tryCatch({
    readClipboard(format = 1, raw = FALSE)
  },
  error = function(e) {}
  )
  if(!is.null(result) && result != '??'){ # r-devel system seems to return '??' from readClipboard.
    
    expect_equal(result, 'hello')    
    
    # errors.
    expect_error( { ecopy( iris, showrowcolnames = "wrong", show = 'show', buffer = 5000 ) }, 'should be one of' )
    
  }
  
})