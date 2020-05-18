#' Is Valid / Is a Value / NA NULL Check
#' 
#' Facilitates checking for missing values which may cause errors later in code. 
#' NULL values can cause errors on is.na checks, and is.na can cause warnings if it is inside if() and is passed multiple values.
#' This function makes it easier to check for missing values before trying to operate on a variable. 
#' It will NOT check for strings like ""  or "NA". Only NULL and NA values will return TRUE.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @param x Object to check. In the case of a data frame or vector, it will check the first (non-NULL) value.
#' @param na_strings (Optional) Set the strings you want to consider NA. These will be applied after stringr::str_trim on x.
#' @param do.test.each Return a vector of results to check each element instead of checking the entire object.
#'
#' @return True/false indicating if the argument is NA, NULL, or an empty/NA string/vector. For speect, only the first value is checked.
#' 
#' @export
#'
#' @examples
#' isval( NULL )
#' isval( NA )
#' isval( c( NA , NULL ) )
#' isval( c( 1, 2, 3 ) )
#' isval( c( NA, 2, 3 ) )
#' isval( c( 1, 2, NA ) ) # only the first values is checked, so this will come back FALSE.
#' isval( c( NULL, 2, 3 ) ) # NULL values get skipped in a vector.
#' isval( data.frame() )
#' isval( dplyr::group_by( dplyr::select( cars, speed, dist ), speed ) ) # test a tibble.
#' isval( "#VALUE!" ) # test an excel error code.
isval <- function( x, na_strings = easyr::nastrings, do.test.each = FALSE ){

  # Empty vectors/lists, or data frames with no columns.
  # Length of NULL is 0 so this also checks for null. Length of NA is 1.
  if( length(x) == 0 ) return(FALSE)
  
  # Default functionality - check the object, not the elements.
  if( !do.test.each ){
    
    # If a data frame made it this far, it is valid and can be worked on.
    if( is.data.frame(x) && ! do.test.each ) return( TRUE )
    
    # Get a single value to test, in case a data frame, tibble, or vector is passed.
    # Otherwise these checks can be very slow. In the future, I'd like to add an option to check all but for now we'll leave this as-is.
    icheck <- x[[1]][1]
  
    if( all( is.na( icheck ) ) ) return(FALSE) # it is NA; "any()" is necessary when checking tibbles (dplyr).
  
    if( is.character(icheck) ) if( stringr::str_trim(icheck) %in% na_strings ) return( FALSE ) # check for na strings, including the blank.
  
    return(TRUE) # If we made it this far, it passed all checks and will not be considered nanull.
   
  # Check the elements. 
  } else { 
    
    # Vector.
    return( !is.na(x) )
    
  }

}