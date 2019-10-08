#' Get Rows with Duplicates
#' 
#' Pulls all rows with duplicates in a column, not just the duplicate row.
#' Author: Bryce Chamberlain.
#' 
#' @param x Data frame.
#' @param c Column as vector or string.
#' @param na Consider multiple NAs as duplicates?
#' 
#' @return Rows from the data frame in which the column is duplicated.
#'
#' @export
#' 
#' @examples 
#' ddt = dplyr::bind_rows( cars, utils::head( cars, 10 ) )
#' drows( ddt, 'speed' )
drows <- function( x, c, na = FALSE ) {
  
  # If a string was passed, get the column.
  if( length(c) < nrow(x) && length(c) == 1 ){
    if( c %ni% names(x) ) stop( glue::glue( 'Column [{c}] not found.' ) )
    c = x[[c]]
  }
  
  # Get duplicated columns, ignoring or including NAs.
  if( na ){
    rows = which( c %in% c[ duplicated(c) ] )
  } else {
    rows = which( !is.na(c) & c %in% c[ duplicated(c) ] )
  }
  
  return( x[ rows, ] )

}