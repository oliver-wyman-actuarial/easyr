context("strx")

test_that("works as expected", {
  t = capture.output( strx(iris,'length') )
  expect_true( all( t!='' ) )
})