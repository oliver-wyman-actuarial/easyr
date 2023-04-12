test_that("works as expected", {

    # duplication
      
      x = data.frame( 
        id = as.integer( c( 1, 2, 3 ) ),
        val = c( 1, 2, 3 ),
        stringsAsFactors = TRUE
      )
      
      y = data.frame(
        id = as.integer( c( 2, 1, 1 ) ),
        val = c( 1, 2, 3 ),
        stringsAsFactors = TRUE
      )
      
      expect_error( { t = jrepl( x, y, by = 'id', replace.cols = 'val' ) }, regexp = 'duplicated' )

  # other tests:
  df1 = utils::head(sleep)
  df1$group %<>% droplevels()
  group.reassign = data.frame( id.num = factor( c( 1, 3, 4 ) ), group.replace = factor( c( 99, 99, 99 ) ), stringsAsFactors = TRUE )
  
  expect_equal( 
    jrepl( x = df1, y = group.reassign, by = c( 'ID' = 'id.num' ), replace.cols = c( 'group' = 'group.replace' ) )[['group']],
    factor( c( 99, 1, 99, 99, 1, 1 ) )
  )
  
  expect_equal( 
    jrepl( x = df1, y = group.reassign, by = c( 'ID' = 'id.num' ), replace.cols = c( 'group' = 'group.replace' ), only.rows = c( 1, 4 ) )[['group']],
    factor( c( 99, 1, 1, 99, 1, 1 ) )
  )

  # doesn't affect since there are no NAs in group.
  expect_equal( 
    jrepl( df1, group.reassign, by = c( 'ID' = 'id.num' ), replace.cols = c( 'group' = 'group.replace' ), na.only = TRUE  )[['group']],
    factor( c( 1, 1, 1, 1, 1, 1 ) )
  )
  
  t = capture.output(
    df1.new = jrepl( df1, group.reassign, by = c( 'ID' = 'id.num' ), replace.cols = c( 'group' = 'group.replace' ), verbose = TRUE )
  )[1]
  expect_equal( t, 'jrepl: [ group ] values replaced: 50 % ' )

  # issue with new colum that doesn't exist yet.

    group.reassign = data.frame( id.num = factor( c( 1, 3, 4 ) ), group.replace = factor( c( 99, 99, 99 ) ), stringsAsFactors = TRUE )

    expect_equal( 
        jrepl( 
          df1,
          group.reassign, 
          by = c( 'ID' = 'id.num' ),
           replace.cols = c( 'newcol' = 'group.replace' ) 
        )[['newcol']],
      factor( c( 99, NA, 99, 99, NA, NA ) )
    )

  # issue with new FACTOR colum that doesn't exist yet.
    
    group.reassign = data.frame( id.num = factor( c( 1, 3, 4 ) ), group.replace = factor( c( 99, 99, 99 ) ), stringsAsFactors = TRUE )

    expect_equal( 
        jrepl( 
          df1,
          group.reassign, 
          by = c( 'ID' = 'id.num' ),
           replace.cols = c( 'newcol' = 'group.replace' ) 
        )[['newcol']],
      factor( c( 99, NA, 99, 99, NA, NA ) )
    )
  
  # test that a missing integer stays an integer in the final data.
  
    x = data.frame( id = as.integer( c( 1, 2, 3) ), stringsAsFactors = TRUE )
    
    y = data.frame(
      id = as.integer( c( 1, 2 ) ),
      int = as.integer( c( 1, 2 ) ),
      stringsAsFactors = TRUE
    )
    
    expect_equal(
      jrepl( x, y, by = 'id', replace.cols = 'int' )$int,
      as.integer( c( 1, 2, NA ) )
    )
    
  # test varying data types.
    
    # int to val
    
      x = data.frame( 
        id = as.integer( c( 1, 2, 3 ) ),
        val = as.integer( c( NA, NA, NA ) ),
        stringsAsFactors = TRUE
      )
      
      y = data.frame(
        id = as.integer( c( 1, 2 ) ),
        val = c( 1.5, 2.6 ),
        stringsAsFactors = TRUE
      )
      
      expect_warning( { t = jrepl( x, y, by = 'id', replace.cols = 'val' ) }, regexp = 'type changed' )
      expect_equal( sum( t$val, na.rm = TRUE ), 4.1 )
      expect_equal( class( t$val ), 'numeric' )
      
    # character to factor.
        
      x = data.frame( 
        id = as.integer( c( 1, 2, 3 ) ),
        val = as.character( c( 'a', 'b', 'c' ) ),
        stringsAsFactors = FALSE
      )
      
      y = data.frame(
        id = as.integer( c( 1, 2 ) ),
        val = factor( c( 'a', 'b' ) ),
        stringsAsFactors = TRUE
      )
      
      expect_warning( { t = jrepl( x, y, by = 'id', replace.cols = 'val' ) }, regexp = 'type changed' )
      expect_equal( t$val, factor( c( 'a', 'b', 'c' ) ) )
      
    # invalid comparison.
      
      x = data.frame( 
        id = as.integer( c( 1, 2, 3 ) ),
        val = c( 1, 2, 3 ),
        stringsAsFactors = TRUE
      )
      
      y = data.frame(
        id = as.integer( c( 1, 2 ) ),
        val = factor( c( 'a', 'b' ) ),
        stringsAsFactors = TRUE
      )
      
      expect_error( { t = jrepl( x, y, by = 'id', replace.cols = 'val' ) }, regexp = 'Please fix the data such that classes match before calling jrepl' )

      # test ordered factor.
        
        x = data.frame( 
          id = as.integer( c( 1, 2, 3 ) ),
          val = ordered( c( NA, NA, 'c' ) ),
          stringsAsFactors = TRUE
        )
        
        y = data.frame(
          id = as.integer( c( 1, 2 ) ),
          val = ordered( c( 'a', 'b' ) ),
          stringsAsFactors = TRUE
        )
      
        t = jrepl( x, y, by = 'id', replace.cols = 'val' )
        expect_equal( t$val, ordered( c( 'a', 'b', 'c' ) ) )

    # all-NA column.
      
      x = data.frame( 
        id = as.integer( c( 1, 2, 3 ) ),
        val = c(1, 2, 3)
      )
      
      y = data.frame(
        id = as.integer( c( 1, 2 ) ),
        val = as.numeric(c(NA, NA))
      )
      
      t = jrepl( x, y, by = 'id', replace.cols = 'val' )
      expect_equal( t$val, c(1, 2, 3) )


  
})
