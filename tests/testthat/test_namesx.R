context("namesx")

test_that("works as expected", {
  expect_equal( namesx(iris,'length'), c('Sepal.Length','Petal.Length') )
  expect_equal( length( namesx(iris,'length', ignore.case=F) ), 0 )
})