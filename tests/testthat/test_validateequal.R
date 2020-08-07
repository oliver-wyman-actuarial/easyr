context( "validate.equal" )
 
test_that("works as expected", {
  
  idt = iris
  idt$id = 1:nrow(idt)
  
  t = capture.output( validate.equal( 
    idt, idt, id.column = 'id', do.set.NA = FALSE, match.round.to.digits = NULL, do.all.columns.before.err = TRUE,
    check.column.order = TRUE, acceptable.pct.rows.diff = .001, verbose = TRUE
  ) )
  expect_equal( any( grepl( 'All checks passed!', t ) ), TRUE )  
  
  idt1 = idt
  idt2 = idt
  
  idt2[ 2:13, 3 ] = 0
    
  t = suppressWarnings( validate.equal( 
    idt1, idt2, id.column = 'id', do.set.NA = FALSE, match.round.to.digits = NULL, do.all.columns.before.err = TRUE,
    check.column.order = TRUE, acceptable.pct.rows.diff = .001, verbose = FALSE
  )) 
  expect_equal( nrow(t), 12 )

  # acceptable pct diff 1 (fails)

    idt1 = data.frame(
      col1 = c( NA, 10, NA ),
      stringsAsFactors = TRUE
    )

    idt2 = data.frame(
      col1 = c( NA, 11, 15 ),
      stringsAsFactors = TRUE
    )
    
    t = suppressWarnings( validate.equal( 
      idt1, idt2, acceptable.pct.vals.diff = .05, verbose = FALSE
    ))

    expect_equal( is.null(t), FALSE )

  # acceptable pct diff 1 (passes)

    idt1 = data.frame(
      col1 = c( NA, 10, 15 ),
      stringsAsFactors = TRUE
    )

    idt2 = data.frame(
      col1 = c( NA, 11, 15 ),
      stringsAsFactors = TRUE
    )
    
    t = suppressWarnings( validate.equal( 
      idt1, idt2, acceptable.pct.vals.diff = .1, verbose = FALSE
    ))

    expect_equal( is.null(t), TRUE )
    
  # factor vs. ordered.
    
    idt1 = data.frame(
      col1 = ordered( c( NA, 'a', 'b' ) ),
      stringsAsFactors = TRUE
    )
    
    idt2 = data.frame(
      col1 = factor( c( NA, 'a', 'b' ) ),
      stringsAsFactors = TRUE
    )
    
    t = suppressWarnings( validate.equal( 
      idt1, idt2, verbose = FALSE
    ))
    
    expect_equal( is.null(t), TRUE )

})