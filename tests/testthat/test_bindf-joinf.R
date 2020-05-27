context( "bindf-joinf.R" )

test_that( "works as expected", {
  
  # Set up data.
  
    df1 = data.frame(
      factor1 = c( 'a', 'b', 'c' ),
      factor2 = c( 'high', 'medium', 'low' ),
      factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
      numeric = c( 1, 2, 3 ),
      logical = c( TRUE, TRUE, TRUE ),
      stringsAsFactors = TRUE
    )
    
    df2 = data.frame(
      factor1 = c( 'd', 'e', 'f' ),
      factor2 = c( 'low', 'medium', 'high' ),
      factor.join = c( '32532532536', '304359867893753', '32534745876' ),
      numeric = c( 4, 5, 6 ),
      logical = c( FALSE, FALSE, FALSE ),
      stringsAsFactors = TRUE
    )
  
  # Perform a bind.
    
    bind1 = bindf( df1, df2 )
    expect_equal( levels( bind1$factor1 ), c( 'a', 'b', 'c', 'd', 'e', 'f' ) )
    expect_equal( levels( bind1$factor2 ), c( 'high', 'low', 'medium' ) )
    expect_equal( levels( bind1$factor2 )[ bind1$factor2], c( 'high', 'medium', 'low', 'low', 'medium', 'high' ) )
    expect_equal( levels( bind1$factor1 )[ bind1$factor1], c( 'a', 'b', 'c', 'd', 'e', 'f' ) )
    
  # bind list 

    bind1 = bindf( list(df1, df2) )
    expect_equal( levels( bind1$factor1 ), c( 'a', 'b', 'c', 'd', 'e', 'f' ) )
    expect_equal( levels( bind1$factor2 ), c( 'high', 'low', 'medium' ) )
    expect_equal( levels( bind1$factor2 )[ bind1$factor2], c( 'high', 'medium', 'low', 'low', 'medium', 'high' ) )
    expect_equal( levels( bind1$factor1 )[ bind1$factor1], c( 'a', 'b', 'c', 'd', 'e', 'f' ) )
    
  # bind with NULL

    bind1 = bindf( df1, NULL )
    expect_equal( bind1, df1 )
    
  # Left join
    
    ljoin1 = ljoinf( df1, df2, by = 'factor.join', sort.levels = FALSE )
    expect_equal( levels( ljoin1$factor1.x ), c( 'a', 'b', 'c' ) )
    expect_equal( levels( ljoin1$factor.join ), c( '0349038u093843', '304359867893753', '3409783509735', '32532532536', '32534745876' ) )
    expect_equal( 
      levels( ljoin1$factor.join )[ ljoin1$factor.join], 
      levels( df1$factor.join)[ df1$factor.join ]  
    )
    expect_equal( levels( ljoin1$factor1.y )[ ljoin1$factor1.y ], c( '(Missing)', 'e', '(Missing)' ) )
    
  # Left join keep NA
    
    ljoin1 = ljoinf( df1, df2, by = 'factor.join', sort.levels = FALSE, na_level = NULL )
    expect_equal( levels( ljoin1$factor1.y )[ ljoin1$factor1.y ], c( NA, 'e', NA ) )
    
  # Inner join.
    
    ijoin1 = ijoinf( df1, df2, by = 'factor.join', sort.levels = FALSE, restrict.levels = TRUE )
    expect_equal( levels( ijoin1$factor.join ), '304359867893753' )
    expect_equal( levels( ijoin1$factor.join )[ ijoin1$factor.join], '304359867893753' )
    
  # Right Join has been removed. Left join should be used instead.
    
  # Full join
    
    fjoin1 = fjoinf( df1, df2, by = 'factor.join', sort.levels = FALSE, restrict.levels = TRUE )
    expect_equal( levels( fjoin1$factor1.x ), c( 'a', 'b', 'c', '(Missing)') )
    expect_equal( levels( fjoin1$factor.join ), c( '0349038u093843', '304359867893753', '3409783509735', '32532532536', '32534745876' ) )
    expect_equal( as.character( fjoin1$factor.join ), c( '0349038u093843', '304359867893753', '3409783509735', '32532532536', '32534745876' )  )
    expect_equal( as.character( fjoin1$factor1.y ), c( '(Missing)', 'e', '(Missing)', 'd', 'f' ) )
    expect_equal( as.character( fjoin1$factor1.x ), c( 'a', 'b', 'c', '(Missing)', '(Missing)' ) )
    
  # Left join with different factor names.
    
    df2 = dplyr::rename( df2, new.colname = factor.join )
    ijoin1 = ijoinf( df1, df2, by = c( 'factor.join' = 'new.colname' ), sort.levels = FALSE, restrict.levels = TRUE )
    expect_equal( levels( ijoin1$factor.join ), '304359867893753' )
    expect_equal( levels( ijoin1$factor.join )[ ijoin1$factor.join], '304359867893753' )
    
  # Factor + character.
    
    df2 = data.frame(
      factor1 = c( 'd', 'e', 'f' ),
      factor2 = c( 'low', 'medium', 'high' ),
      factor.join = c( '32532532536', '304359867893753', '32534745876' ),
      numeric = c( 4, 5, 6 ),
      logical = c( FALSE, FALSE, FALSE ),
      stringsAsFactors = FALSE
    )
    
    # Perform a bind.
      
      bind1 = bindf( df1, df2 )
      expect_equal( levels( bind1$factor1 ), c( 'a', 'b', 'c', 'd', 'e', 'f' ) )
      expect_equal( levels( bind1$factor2 ), c( 'high', 'low', 'medium' ) )
      expect_equal( levels( bind1$factor2 )[ bind1$factor2], c( 'high', 'medium', 'low', 'low', 'medium', 'high' ) )
      expect_equal( levels( bind1$factor1 )[ bind1$factor1], c( 'a', 'b', 'c', 'd', 'e', 'f' ) )
    
    # Left join
    
      ljoin1 = ljoinf( df1, df2, by = 'factor.join', sort.levels = FALSE )
      expect_equal( levels( ljoin1$factor1.x ), c( 'a', 'b', 'c' ) )
      expect_equal( levels( ljoin1$factor.join ), c( '0349038u093843', '304359867893753', '3409783509735', '32532532536', '32534745876' ) )
      expect_equal( levels( ljoin1$factor.join )[ ljoin1$factor.join], levels( df1$factor.join)[ df1$factor.join ]  )
      # this still has NAs because it is a character since it wasn't involved in the join.
      expect_equal( ljoin1$factor1.y, c( NA, 'e', NA ) )
    

})
    