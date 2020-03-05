#' left
#' 
#' Behaves like Excel's LEFT, RIGHT, and MID functions
#' Author: Dave. Tech review: Bryce Chamberlain.
#' 
#' @export
#' @param string String to process.
#' @param char Number of characters. 
#' 
#' @examples 
#' left( "leftmidright", 4 )
left = function ( string, char ) substr( string, 1, char )

#' right
#' 
#' Behaves like Excel's LEFT, RIGHT, and MID functions
#' Author: Dave. Tech review: Bryce Chamberlain.
#' 
#' @export
#' @param string String to process.
#' @param char Number of characters. 
#' 
#' @examples 
#' right( "leftmidright",5 )
right = function ( string, char ) substr( string, nchar( string ) - ( char - 1 ), nchar(string) )

#' mid
#' 
#' Behaves like Excel's LEFT, RIGHT, and MID functions
#' Author: Bryce Chamberlain.
#' 
#' @export
#' @param string String to process.
#' @param start Index (1-index) to start at.
#' @param nchars Number of characters to read in from start.
#' 
#' @examples 
#' mid( "leftmidright", 5, 3 )
mid = function ( string, start, nchars ) substr( string, start, start + nchars - 1 ) 
