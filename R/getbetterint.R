#' Get better Int
#' 
#' Takes bucket names of binned values such as [1e3,2e3) or [0.1234567, 0.2) and formats the values nicely into values such as 1,000-2,000 or 0.12-0.20
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param int Vector of character bucket names to transform.
#'
#' @return Vector of transformed values.
#' @export
#'
#' @examples
#' iris$bin <- binbyvol( iris, 'Sepal.Width', 'Sepal.Length', 5 )
#' getbetterint( iris$bin )
getbetterint <- function (int) {
  
  int = as.factor(int)
  
  ilocate_comma = stringr::str_locate(levels(int), ",")
  
  start <- as.numeric( substr( levels(int), 2,ilocate_comma - 1 ) )
  
  end <- as.numeric( substr( levels(int), ilocate_comma + 1, nchar( levels(int) ) - 1 ) )
  
  levels(int) <- paste0( 
    formatC( start, big.mark = ",", format = "f", drop0trailing = TRUE )
    , "-", 
    formatC( end, big.mark = ",", format = "f", drop0trailing = TRUE )
  )
  
  return(int)
}