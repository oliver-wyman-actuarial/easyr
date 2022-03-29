test_that( "works as expected", {
  
  x = c( 'TRUE', 'FALSE', '0', NA, 'not a boolean', 'NA' )
  
  expect_equal( tobool( x, verbose = FALSE ), x )
  
  expect_warning( asbool <- tobool( x, ifna = 'warning' ), 'NAs were created' )
  expect_equal( asbool, c( TRUE, FALSE, FALSE, NA, NA, NA ) )
  
  expect_error( asbool <- tobool( x, ifna = 'error' ),  'NAs were created' )
  
  # test true and false.
  idt = dplyr::mutate(iris, binaryExample = ifelse(Species=="versicolor", "F", "M"))
  expect_equal(
      unique(tobool(idt$binaryExample, true.vals = "F", false.vals = "M")),
      c(FALSE, TRUE)
  )

# test all-true (this has failed in the past)
idt = iris
idt$newSpecies = toupper(idt$Species)
expect_equal(
    unique(tobool(idt$newSpecies, true.vals = unique(idt$newSpecies))),
    TRUE
)

})