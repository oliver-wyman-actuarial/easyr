#' Like Date
#'
#' Check if a column can be converted to a date. Helpful for checking a column before actually converting it.
#' Author: Bryce Chamberlain. Tech review: Dominic Dillingham.
#'
#' @param x Value or vector to check.
#' @param na_strings Vector of characters to consider NA. Like Date will treat these values like NA.
#' @param aggressive.extraction todate will take dates inside long strings (like filenames) and convert them to dates. This seems to be the preferred outcome, so we leave it as default (TRUE). However, if you want to avoid this you can do so via this option (FALSE).
#' @param run_unique Convert to unique variables before checking. In some cases, this can make it take longer than necessary. In most, it will make it faster.
#'
#' @return Boolean indicating if the entire vector can be converted to a date.
#' @export
#'
#' @examples
#' x <- c('20171124','2017/12/24',NA,'12/24/2017','March 3rd, 2015','Mar 3, 2016')
#' likedate(x)
#' likedate(c(123,456,NA))
#' if(likedate(x)) t <- todate(x)
#' likedate(lubridate::mdy('1-1-2014'))
#' likedate( '3312019' )
#' likedate( '2019.1.3' )
likedate <- function( 
  
  x, 
  na_strings = easyr::nastrings, 
  run_unique = TRUE,
  aggressive.extraction = TRUE
  
){
  
  if( run_unique ) x = unique(x)
  
  test.convert = todate( x, nastrings = na_strings, aggressive.extraction = aggressive.extraction, verbose = FALSE )
  
  return( lubridate::is.Date( test.convert ) )

}