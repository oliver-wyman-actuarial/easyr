#' Shorthand for as.character
#'
#' @param x Value to check.
#'
#' @return as.character result
#' @export
#'
#' @examples
#' tochar(NA)
#' tochar(1)
tochar = function(x){
  as.character(x)
}

#' Shorthand for is.na
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' na(NA)
#' na(1)
na = function(x){
  is.na(x)
}

#' Shorthand for is.null
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' null( NULL )
#' null(1)
null = function(x){
  is.null(x)
}

#' Shorthand for is.nan
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' nan( NaN )
#' nan(1)
nan = function(x){
  is.nan(x)
}

#' Shorthand for is.numeric
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' isnum(1)
#' isnum( factor( c( 'a', 'b', 'c' ) ) )
isnum = function(x){
  is.numeric(x)
}

#' Shorthand for lubridate::is.Date
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' isdate( lubridate::mdy( '10/1/2014' ) )
#' isdate(1)
isdate = function(x){
  lubridate::is.Date(x)
}

#' Shorthand for is.factor
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' isfac( factor( c( 'a', 'b', 'c' ) ) )
#' isfac(1)
isfac = function(x){
  is.factor(x)
}

#' Shorthand for is.character
#'
#' @param x Value to check.
#'
#' @return logical indicator
#' @export
#'
#' @examples
#' ischar( 'a character' )
#' ischar(1)
ischar = function(x){
  is.character(x)
}