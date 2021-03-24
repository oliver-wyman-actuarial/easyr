test_that("works as expected", {
  expect_equal(  pad0( c(123,00123,5), len = 5 ), c("00123","00123", "00005") )
  expect_equal(  pad0( c(123,00123,5), len = 2 ), c("123","123", "05") )
  expect_equal(  pad0( c('abc','def'), len = 4 ),  c('0abc','0def') ) # make sure this works with strings.
})