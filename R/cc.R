#' Concatenate.
#' 
#' Shorthand function for paste.
#' Author: Bryce Chamberlain.
#'
#' @param ... Arguments to be passed to paste0. Typcially a list of vectors or values to be concatenated.
#' @param sep (Optional) Separator between concatenated items.
#'
#' @return Vector of pasted/concatenated values.
#' @export
#'
#' @examples
#' cc( 1, 2, 4 )
#' x = data.frame( c1 = c( 1, 2, 4 ), c2 = c( 3, 5, 7 ) )
#' cc( x$c1, x$c2 )
#' cc( x$c1, x$c2, sep = '-' )
cc = function( ..., sep = '' ){
  
  args = list(...)
  
  # Collapse only if a single vector argument was passed.
  # Minus 2, 1 for the function name and 1 for sep.
  # https://stackoverflow.com/questions/44011918/count-number-of-arguments-passed-to-function
  #collapse = length(match.call()) - 2 == 1
  if( length(args) == 1 ){
    
    if( any( is.na( args[[1]] ) ) ) return(NA)
    
    icc = paste( ..., collapse = sep )
    
  } else {
    
    icc = paste( ..., sep = sep )
    
    for( i in args ) icc[ is.na(as.character(i)) ] <- NA
    
  }
  
  return( icc )
  
}
