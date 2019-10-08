
#' Bind Rows with Factors
#' 
#' dplyr's bind_rows doesn't work well when the data frame has factors. This function handles factors before applying bind rows.
#' Author: Bryce Chamberlain.
#'
#' @param data.top Data to be binded in the top rows.
#' @param data.bottom Data to be binded in the bottom rows.
#' @param sort.levels Sort the factor levels after combining them.
#' @param ... Other arguments passed to bind_rows.
#'
#' @return  Binded data, with any factors modified to contain all levels in the binded data.
#' @export
#'
#' @examples
#' 
#' # create data where factors have different levels.
#' df1 = data.frame(
#'   factor1 = c( 'a', 'b', 'c' ),
#'   factor2 = c( 'high', 'medium', 'low' ),
#'   factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
#'   numeric = c( 1, 2, 3 ),
#'   logical = c( TRUE, TRUE, TRUE )
#' )#' 
#' df2 = data.frame(
#'   factor1 = c( 'd', 'e', 'f' ),
#'   factor2 = c( 'low', 'medium', 'high' ),
#'   factor.join = c( '32532532536', '304359867893753', '32534745876' ),
#'   numeric = c( 4, 5, 6 ),
#'   logical = c( FALSE, FALSE, FALSE )
#' )
#' 
#' # bindf preserves factors but combines levels.
#' # factor-friendly functions default to ordered levels.
#' str( df1 )
#' str( bindf( df1, df2 ) )
bindf <- function ( data.top, data.bottom, sort.levels = TRUE, ... ){
  
  if( is.null( data.top ) ) return( data.bottom )
  if( is.null( data.bottom ) ) return( data.top )
  
  # Expand/match factors.
  imatch.factors = match.factors( df1 = data.top, df2 = data.bottom, sort.levels = sort.levels )
  
  # Now we are ready to proceed with bind_rows.
  return( dplyr::bind_rows( imatch.factors[[1]], imatch.factors[[2]], ... ) )
  
}


#' Left Join with Factors
#' 
#' dplyr's left_join doesn't work well when the data frame has factors. This function handles factors before applying left_join.
#' Author: Bryce Chamberlain.
#'
#' @param data.left Left data. All of this data will be preservered in the join (may still result in duplication).
#' @param data.right Right data. Only rows that matche the join will be included (may also result in duplication).
#' @param by Columns to join on. Passed to dplyr join.
#' @param sort.levels Sort the factor levels after combining them.
#' @param restrict.levels Often the joined data won't use all the levels in both datasets. Set to TRUE to remove factor levels that aren't in the joined data.
#' @param na_level dplyr doesn't like factors to have NAs so we replace NAs with this value for factors only. Set NULL to skip.
#' @param ... Other arguments passed to left_join.
#'
#' @return  Joined data, with any factors modified to contain all levels in the joined data.
#' @export
#'
#' @examples
#' 
#' df1 = data.frame(
#'   factor1 = c( 'a', 'b', 'c' ),
#'   factor2 = c( 'high', 'medium', 'low' ),
#'   factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
#'   numeric = c( 1, 2, 3 ),
#'   logical = c( TRUE, TRUE, TRUE )
#' )
#' 
#' df2 = data.frame(
#'   factor1 = c( 'd', 'e', 'f' ),
#'   factor2 = c( 'low', 'medium', 'high' ),
#'   factor.join = c( '32532532536', '304359867893753', '32534745876' ),
#'   numeric = c( 4, 5, 6 ),
#'   logical = c( FALSE, FALSE, FALSE )
#' )
#' 
#' ljoinf( df1, df2, by = 'factor.join' )
#' 
ljoinf <- function ( data.left, data.right, by, sort.levels = TRUE, restrict.levels = FALSE, na_level = '(Missing)', ... ){
  
  # Expand/match factors.
  imatch.factors = match.factors( df1 = data.left, df2 = data.right, by = by, sort.levels = sort.levels )
  
  # Now we are ready to proceed with left_join
  ijoin = dplyr::left_join( imatch.factors[[1]], imatch.factors[[2]], by = by, ... )
  
  # If requested, limit the factors to only those in the joined data.
  if( restrict.levels ) for( icol in c( names(by), by ) ) if( icol %in% colnames( ijoin ) ){
    ijoin[[ icol ]] <- droplevels( ijoin[[ icol ]] )
  }

  # Replace NA factors.
  ijoin = factorna( x = ijoin, na_level = na_level )
  
  return(ijoin)
  
}


#' Right Join with Factors
#' 
#' dplyr's right_join doesn't work well when the data frame has factors. This function handles factors before applying the join.
#' Author: Bryce Chamberlain.
#'
#' @param data.left Left data. Only rows that matche the join will be included (may still result in duplication).
#' @param data.right Right data. All of this data will be preservered in the join (may also result in duplication).
#' @param by Columns to join on. Passed to dplyr join.
#' @param sort.levels Sort the factor levels after combining them.
#' @param restrict.levels Often the joined data won't use all the levels in both datasets. Set to TRUE to remove factor levels that aren't in the joined data.
#' @param na_level dplyr doesn't like factors to have NAs so we replace NAs with this value for factors only. Set NULL to skip.
#' @param ... Other arguments passed to right_join.
#'
#' @return  Joined data, with any factors modified to contain all levels in the joined data.
#' @export
#'
#' @examples
#' 
#' df1 = data.frame(
#'   factor1 = c( 'a', 'b', 'c' ),
#'   factor2 = c( 'high', 'medium', 'low' ),
#'   factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
#'   numeric = c( 1, 2, 3 ),
#'   logical = c( TRUE, TRUE, TRUE )
#' )
#' 
#' df2 = data.frame(
#'   factor1 = c( 'd', 'e', 'f' ),
#'   factor2 = c( 'low', 'medium', 'high' ),
#'   factor.join = c( '32532532536', '304359867893753', '32534745876' ),
#'   numeric = c( 4, 5, 6 ),
#'   logical = c( FALSE, FALSE, FALSE )
#' )
#' 
#' rjoinf( df1, df2, by = 'factor.join' )
#' 
rjoinf <- function ( data.left, data.right, by, sort.levels = TRUE, restrict.levels = FALSE, na_level = '(Missing)', ... ){
  
  # Expand/match factors.
  imatch.factors = match.factors( df1 = data.left, df2 = data.right, by = by, sort.levels = sort.levels )
  
  # Now we are ready to proceed with right_join
  ijoin = dplyr::right_join( imatch.factors[[1]], imatch.factors[[2]], by = by, ... )
  
  # If requested, limit the factors to only those in the joined data.
  if( restrict.levels ) for( icol in c( names(by), by ) ) if( icol %in% colnames( ijoin ) ){
    ijoin[[ icol ]] <- droplevels( ijoin[[ icol ]] )
  }

  # Replace NA factors.
  ijoin = factorna( x = ijoin, na_level = na_level )
  
  return(ijoin)
  
}


#' Full Join with Factors
#' 
#' dplyr's join doesn't work well when the data frame has factors. This function handles factors before applying the join.
#' Author: Bryce Chamberlain.
#'
#' @param data.left Left data. Only rows that matche the join will be included (may still result in duplication).
#' @param data.right Right data. All of this data will be preservered in the join (may also result in duplication).
#' @param by Columns to join on. Passed to dplyr join.
#' @param sort.levels Sort the factor levels after combining them.
#' @param restrict.levels Often the joined data won't use all the levels in both datasets. Set to TRUE to remove factor levels that aren't in the joined data.
#' @param na_level dplyr doesn't like factors to have NAs so we replace NAs with this value for factors only. Set NULL to skip.
#' @param ... Other arguments passed to full_join.
#'
#' @return  Joined data, with any factors modified to contain all levels in the joined data.
#' @export
#'
#' @examples
#' 
#' df1 = data.frame(
#'   factor1 = c( 'a', 'b', 'c' ),
#'   factor2 = c( 'high', 'medium', 'low' ),
#'   factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
#'   numeric = c( 1, 2, 3 ),
#'   logical = c( TRUE, TRUE, TRUE )
#' )
#' 
#' df2 = data.frame(
#'   factor1 = c( 'd', 'e', 'f' ),
#'   factor2 = c( 'low', 'medium', 'high' ),
#'   factor.join = c( '32532532536', '304359867893753', '32534745876' ),
#'   numeric = c( 4, 5, 6 ),
#'   logical = c( FALSE, FALSE, FALSE )
#' )
#' 
#' fjoinf( df1, df2, by = 'factor.join' )
#' 
fjoinf <- function ( data.left, data.right, by, sort.levels = TRUE, restrict.levels = FALSE, na_level = '(Missing)', ... ){
  
  # Expand/match factors.
  imatch.factors = match.factors( df1 = data.left, df2 = data.right, by = by, sort.levels = sort.levels )
  
  # Now we are ready to proceed with right_join
  ijoin = dplyr::full_join( imatch.factors[[1]], imatch.factors[[2]], by = by, ... )
  
  # If requested, limit the factors to only those in the joined data.
  if( restrict.levels ) for( icol in c( names(by), by ) ) if( icol %in% colnames( ijoin ) ){
    ijoin[[ icol ]] <- droplevels( ijoin[[ icol ]] )
  }

  # Replace NA factors.
  ijoin = factorna( x = ijoin, na_level = na_level )
  
  return(ijoin)
  
}


#' Inner Join with Factors
#' 
#' dplyr's inner_join doesn't work well when the data frame has factors. This function handles factors before applying the join.
#' Author: Bryce Chamberlain.
#'
#' @param data.left Left data. Only rows that matche the join will be included (may still result in duplication).
#' @param data.right Right data. Only rows that matche the join will be included (may also result in duplication).
#' @param by Columns to join on. Passed to dplyr join.
#' @param sort.levels Sort the factor levels after combining them.
#' @param restrict.levels Often the joined data won't use all the levels in both datasets. Set to TRUE to remove factor levels that aren't in the joined data.
#' @param ... Other arguments passed to inner_join.
#'
#' @return  Joined data, with any factors modified to contain all levels in the joined data.
#' @export
#'
#' @examples
#' 
#' df1 = data.frame(
#'   factor1 = c( 'a', 'b', 'c' ),
#'   factor2 = c( 'high', 'medium', 'low' ),
#'   factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
#'   numeric = c( 1, 2, 3 ),
#'   logical = c( TRUE, TRUE, TRUE )
#' )
#' 
#' df2 = data.frame(
#'   factor1 = c( 'd', 'e', 'f' ),
#'   factor2 = c( 'low', 'medium', 'high' ),
#'   factor.join = c( '32532532536', '304359867893753', '32534745876' ),
#'   numeric = c( 4, 5, 6 ),
#'   logical = c( FALSE, FALSE, FALSE )
#' )
#' 
#' ljoinf( df1, df2, by = 'factor.join' )
#' 
ijoinf <- function ( data.left, data.right, by, sort.levels = TRUE, restrict.levels = FALSE, ... ){
  
  # Expand/match factors.
  imatch.factors = match.factors( df1 = data.left, df2 = data.right, by = by, sort.levels = sort.levels )
  
  # Now we are ready to proceed with inner_join
  ijoin = dplyr::inner_join( imatch.factors[[1]], imatch.factors[[2]], by = by, ... )
  
  # If requested, limit the factors to only those in the joined data.
  if( restrict.levels ) for( icol in c( names(by), by ) ) if( icol %in% colnames( ijoin ) ){
    ijoin[[ icol ]] <- droplevels( ijoin[[ icol ]] )
  }
  
  return(ijoin)
  
}

#' Match Factors.
#' 
#' Modifies two datasets so matching factor columns have the same levels. Typically this is used prior to joining or bind_rows in the easyr functions bindf, ijoinf, lfjoinf.
#'
#' @param df1 First data set.
#' @param df2 Second data set.
#' @param by Columns to join on, comes from the function using match.factors (ljoinf, fjoinf, ijoinf).
#' @param sort.levels Sort the factor levels after combining them.
#'
#' @return  List of the same data but with factors modified as applicable. All factors are checked if no 'by' argument is passed. Otherwise only the 'by' argument is checked.
#' @export
#'
#' @examples
#' 
#' df1 = data.frame(
#'   factor1 = c( 'a', 'b', 'c' ),
#'   factor2 = c( 'high', 'medium', 'low' ),
#'   factor.join = c( '0349038u093843', '304359867893753', '3409783509735' ),
#'   numeric = c( 1, 2, 3 ),
#'   logical = c( TRUE, TRUE, TRUE )
#' )
#' 
#' df2 = data.frame(
#'   factor1 = c( 'd', 'e', 'f' ),
#'   factor2 = c( 'low', 'medium', 'high' ),
#'   factor.join = c( '32532532536', '304359867893753', '32534745876' ),
#'   numeric = c( 4, 5, 6 ),
#'   logical = c( FALSE, FALSE, FALSE )
#' )
#' 
#' t = match.factors( df1, df2 )
#' levels( df1$factor1 )
#' levels( t[[1]]$factor1 )
#' levels( t[[2]]$factor1 )
#' 
match.factors = function( df1, df2, by = NA, sort.levels = TRUE ){
  
  # If no 'by' was passed, we'll check the shared columns.
  if( nanull( by ) ){ 
    
    shared.columns = intersect( colnames( df1 ), colnames( df2 ) ) 
    check.cols = data.frame( col1 = shared.columns, col2 = shared.columns, stringsAsFactors = FALSE )
    rm(shared.columns) 
    
  # Otherwise, get the names and values from by.
  } else { 
    
    check.cols = data.frame( 
      col1 = if( isval(names(by)) ){ names(by) } else { by }, 
      col2 = as.character(by), 
      stringsAsFactors = FALSE 
    )
    
  }
  
  # Expand column levels to match both datasets.
  for( icolrow in 1:nrow(check.cols) ){
    
    # Validate columns exist.
    if( check.cols[icolrow,1] %ni% colnames(df1) ) stop( 'Column [', check.cols[icolrow,1], '] not found in data.' )
    if( check.cols[icolrow,2] %ni% colnames(df2) ) stop( 'Column [', check.cols[icolrow,2], '] not found in data.' )
    
    newvars = match.levels( df1[[ check.cols[icolrow,1] ]], df2[[ check.cols[icolrow,2] ]], sort.levels = sort.levels )
    df1[[ check.cols[icolrow,1] ]] = newvars[[1]]
    df2[[ check.cols[icolrow,2] ]] = newvars[[2]]
    rm(newvars)
    
  }
  
  return( list( df1, df2 ) )
  
}

match.levels = function( factor1, factor2, sort.levels ){
  
  # If both are not factors, return the same.
  if( !is.factor(factor1) && !is.factor(factor2) ) return( list( factor1, factor2 ) )
  
  # If either is not a factor, make it a factor.
  if( !is.factor( factor1 ) ) factor1 = factor( factor1 )
  if( !is.factor( factor2 ) ) factor2 = factor( factor2 )
  
  # If the levels already match, skip to next.
  # First, check the same number of levels to avoid a warning when checking equal.
  if( length( levels(factor1) ) == length( levels(factor2) ) ){
    if( all( levels( factor1 ) == levels( factor2 ) ) ){
      
      # Apply sort.levels, otherwise you'll get mismatches between sorted and non-sorted levels.
      ilevels = levels( factor1 )
      if( sort.levels ) levels = sort( ilevels )
      factor1 = factor( factor1, levels = ilevels, ordered = sort.levels )
      factor2 = factor( factor2, levels = ilevels, ordered = sort.levels )
      
      return( list( factor1, factor2 ) )
      
    }
  }
  
  # Ensure both are factors.
  if( !is.factor( factor1 ) ) factor1 = factor( factor1 )
  if( !is.factor( factor2 ) ) factor2 = factor( factor2 )
  
  # Get all levels.
  ilevels = unique( c( levels( factor1 ), levels( factor2 ) ) )
  if ( sort.levels ) ilevels = sort( ilevels )

  # Add NA if applicable.
  # testing removal: NA factors are added when necessary in the separate join functions.
  #if( !is.null(na_level) ){
  #  na1 = which( is.na( factor1 ) )
  #  na2 = which( is.na( factor2 ) )
  #  if( ( length( na1 ) + length( na2 ) ) > 0  && na_level %ni% ilevels ) ilevels = c( ilevels, na_level )
  #}
  
  # Set the levels.
  factor1 = factor( levels( factor1 )[ factor1 ], levels = ilevels, ordered = sort.levels )
  factor2 = factor( levels( factor2 )[ factor2 ], levels = ilevels, ordered = sort.levels )

  # Set NA.
  # testing removal: NA factors are added when necessary in the separate join functions.
  #if( !is.null(na_level) && ( length( na1 ) + length( na2 ) ) > 0 ){
  #   factor1[ na1 ] <- na_level
  #   factor2[ na2 ] <- na_level
  #}
  
  # Return the new factors.
  return( list( factor1, factor2 ) )
  
}