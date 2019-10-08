context("sch")

test_that("works as expected", {
  
  expect_equal( nrow( sch(iris,'setosa') ), sum( iris$Species == 'setosa') )
  expect_equal( nrow( sch(iris,'.') ), sum( nrow(iris) ) )
  expect_equal( nrow( sch( iris, '..', fixed = TRUE) ), 0 )
  
})
  