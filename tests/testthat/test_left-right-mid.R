context( "left-right-mid")

test_that( "works as expected", {

  expect_equal( left('leftmidright',4) , 'left' )
  expect_equal( left('four',5), 'four' )
  
  expect_equal( right('leftmidright',5) , 'right' )
  expect_equal( right('four',5), 'four' )

  expect_equal( mid('leftmidright', 5, 3 ) , 'mid' )
  
})