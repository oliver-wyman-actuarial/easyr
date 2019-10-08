#' NA-Friendly Equality Comparison
#'
#' Vectorized flexible equality comparison which considers NAs as a value. Returns TRUE if both values are NA, and FALSE when only one is NA. 
#' The standard == comparison returns NA in both of these cases and sometimes this is interpreted unexpectedly.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @param x First vector/value for comparison.
#' @param y Second vector/value for comparison. 
#' @param do.nanull.equal Return TRUE if both inputs are NA or NULL (tested via easyr::nanull).
#'
#' @return Boolean vector/value of comparisons.
#' @export
#'
#' @examples
#' c(NA,'NA',1,2,'c') == c(NA,NA,1,2,'a') # regular equality check.
#' eq(c(NA,'NA',1,2,'c'),c(NA,NA,1,2,'a')) # check with eq.
eq <- function( x, y, do.nanull.equal = TRUE ) {
  
  # if vectors differ in length, modify them.
  if( length(x) == 1 && length(y) > 1 ) x = rep( x = x, times = length(y) )
  if( length(y) ==1 && length(x) > 1 ) y = rep( x = y, times = length(x) )
  
  if( do.nanull.equal ) if( all( easyr::nanull( x, do.test.each = TRUE ) & easyr::nanull( y, do.test.each = TRUE ) ) ) return( TRUE )
  
  if( is.null(x) && is.null(y) ) return( TRUE )
  
  nanull.x = nanull( x, do.test.each = TRUE )
  nanull.y = nanull( y, do.test.each = TRUE )
  
  out = rep( FALSE, length( nanull.x ) )
  out[ nanull.x & nanull.y ] <- TRUE
  out[ !nanull.x & !nanull.y & x==y ] <- TRUE
  
  return(out)
  
}