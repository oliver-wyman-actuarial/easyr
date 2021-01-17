test_that("works as expected", {

  # data frame.
  ecopy( iris, showrowcolnames = "cols", show = 'show' )
  ecopy( iris )

  # string.
  ecopy( 'hello' )
   
  # writing to clipboard is not handled in Unix.
  if( .Platform$OS.type != "unix" ){

    expect_equal(
      readClipboard( format = 1, raw = FALSE ),
      'hello'
    )
    
    # errors.
    expect_error( { ecopy( iris, showrowcolnames = "wrong", show = 'show' ) }, 'should be one of' )

  }

})