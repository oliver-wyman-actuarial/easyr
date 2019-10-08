#' Names Like
#'
#' Get column names that match a pattern.
#' Author: Scott Sobel. Tech review: Bryce Chamberlain.
#'
#' @param df Object with names you'd like to search.
#' @param char Regex chracter to match to columns.
#' @param fixed Match as a string, not a regular expression.
#' @param ignore.case Ignore case in matches.
#'
#' @return Vector of matched names.
#' @export
#'
#' @examples
#' namesx( iris,'len' )
#' namesx( iris,'Len' )
namesx <- function (df, char, fixed = TRUE, ignore.case = TRUE ) { 
  
  inames = names( df )
  
  if( ignore.case ){
    inames = tolower(inames)
    char = tolower(char)
  }
  
  imatch = grepl( char, inames, fixed = fixed )
  
  names(df)[ imatch ]
  
}