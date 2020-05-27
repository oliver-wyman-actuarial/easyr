context("sch")

test_that("works as expected", {

  # pluscols = all
  expect_equal( sch(iris,'seto', pluscols='all'), iris[iris$Species == 'setosa', ] )
  expect_equal( sch(iris,'.', pluscols='all' ), iris)
  suppressMessages(
    expect_equal( sch( iris, '..', fixed = TRUE, pluscols='all'), NULL )
  )
  
  # no pluscols
  expect_equal( sch(iris,'seto'), iris[iris$Species == 'setosa', 'Species', drop=FALSE] )
  expect_equal( sch(iris,'.'), iris)
  suppressMessages(
    expect_equal( sch( iris, '..', fixed = TRUE), NULL )
  )
  
  # pluscols something other than all.
  expect_equal( sch(iris,'seto', pluscols='Petal.Length'), iris[iris$Species == 'setosa', c('Petal.Length', 'Species') ] )
  expect_equal( sch(iris,'seto', pluscols=3), iris[iris$Species == 'setosa', c(3,5) ] )
  expect_equal( sch(iris,'.', pluscols=3 ), iris)
  suppressMessages(
    expect_equal( sch( iris, '..', fixed = TRUE, pluscols=3), NULL )
  )

  # exact.
  suppressMessages(
    expect_equal( sch(iris,'seto', pluscols='all', exact=TRUE), NULL)
  )

  # TODO: trim, different data types.
  
})
  