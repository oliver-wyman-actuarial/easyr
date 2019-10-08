#' right
#' 
#' Behaves like Excel's left() and right() functions
#' Author: Dave. Tech review: Bryce Chamberlain.
#' 
#' @export
#' @param string String to process.
#' @param char Number of characters. 
#' 
#' @examples 
#' right("leftright",5)
right = function ( string, char ) substr( string, nchar( string ) - ( char - 1 ), nchar(string) )