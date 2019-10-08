#' Golden Ratio
#' 
#' Get the golden ratio.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @return The golden ratio: (1+sqrt(5)) / 2
#' @export
#'
#' @examples
#' gr()
gr <- function(){
  return( (1+sqrt(5)) / 2 ) # per https://en.wikipedia.org/wiki/Golden_ratio
}