#' Structure with Like
#' 
#' Runs str function but only for names matching a character value (regex).
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param df Object with names you'd like to search.
#' @param char Regex (character value) to match.
#' @param ignore.case (Optional) Ignore case when matching.
#' @export
#'
#' @examples
#' strx(iris,'length')
strx <- function (df, char, ignore.case=T) { 
  utils::str( df[, names(df) %in% namesx( df, char,ignore.case ) ] )
}