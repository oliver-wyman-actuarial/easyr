#' Search a Data Frame.
#' 
#' Searches all columns for a term and returns all rows with at least one match.
#' Author: Bryce Chamberlain.
#'
#' @param x Data to search.
#' @param pattern Regex patter to search. Most normal search terms will work fine, too.
#' @param ignore.case Ignore case in search (uses grepl).
#' @param fixed Passed to grepl to match string as-is instead of using regex. See ?grepl.
#'
#' @return Matching rows.
#' @export
#'
#' @examples
#' sch( iris, 'setosa' )
sch = function( x, pattern, ignore.case = FALSE, fixed = FALSE ){
  
  # Vector?
  if( is.null( nrow(x) ) ) return( x[ grepl( pattern, x, ignore.case = ignore.case, fixed = fixed ) ] )
  
  # Data frame?
  irows = integer()
  for( i in colnames(x) ) irows = c( irows, grep( pattern, x[[i]], ignore.case = ignore.case, fixed = fixed ) )
  irows = unique(irows)
  return( x[irows,] )
  
}
