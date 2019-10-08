#' Pad with Zeros
#' 
#' Adds leading zeros to a numeric vector to make each value a specific length. For values shorter than length passed, leading zeros are removed.
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param x Vector.
#' @param len Number of characters you want in each value.
#'
#' @return Character vector with padded values.
#' @export
#'
#' @examples
#' pad0( c(123,00123,5), len = 5 )
#' pad0( c(123,00123,5), len = 2 )
#' pad0( '1234', 5 )
pad0 <- function(x, len) stringr::str_pad( x, len, pad = "0" )
# sprintf doens't work with character vectors so we are using str_pad anyway.