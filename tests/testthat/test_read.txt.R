context("read.txt ")

test_that( "read text file", {

  expect_equal(
    read.txt( 
      test_file( 'some-text.txt' )
    ),
    'This is some text 530.'
  )

})