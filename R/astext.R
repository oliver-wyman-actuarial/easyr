#' As Text
#' 
#' Prints a vector as text you can copy and paste back into the code. Helpful for copying vectors into code for testing and validation.
#' Author: Bryce Chamberlain.
#'
#' @param x Vector to represent as text.
#'
#' @return Vector represented as a character.
#' @export
#'
#' @examples
#' astext( c( 1, 2, 4 ) )
#' astext( c( 'a', 'b', 'c' ) )
astext = function( x ) if( is.numeric(x) ){
  as.character(glue::glue( 'c( { paste0( x, collapse = ", " ) } )' ))
} else {
   as.character(glue::glue( "c( '{ paste0( x, collapse = \"', '\" ) }' )" ))
}
