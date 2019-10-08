#' left
#' 
#' Behaves like Excel's left() and right() functions
#' Author: Dave. Tech review: Bryce Chamberlain.
#' 
#' @export
#' @param string String to process.
#' @param char Number of characters. 
#' 
#' @examples 
#' left("leftright",4)
left = function ( string, char ) substr( string, 1, char )