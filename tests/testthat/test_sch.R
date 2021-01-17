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

  # list.
    
    t = list(
      a = list('a', 'a', 'a'),
      b = list('b', 'b', 'b'),
      c = list('c', 'c', 'c')
    )
    expect_equal(sch(t, 'a'), list(a = list('a', 'a', 'a')))
    expect_equal(sch(t, 'a', spln = 3), list(a = list('a', 'a', 'a')))
    
    t = list(
      list('a', 'a', 'a'),
      list('b', 'b', 'b'),
      list('c', 'c', 'c')
    )
    expect_equal(sch(t, 'a'), list(list('a', 'a', 'a')))
    
    t = list(
      list('a', 'a', 'a'),
      list('ab', 'b', 'b'),
      list('c', 'c', 'c')
    )
    expect_equal(sch(t, 'a'), list(list('a', 'a', 'a'), list('ab', 'b', 'b')))
    
    t = list(
      a = list('a', 'a', 'a'),
      b = list('ab', 'b', 'b'),
      c = list('c', 'c', 'c')
    )
    expect_equal(sch(t, 'a'), list(a = list('a', 'a', 'a'), b = list('ab', 'b', 'b')))

  # TODO: trim, different data types.

  rm(t)
  
})
  