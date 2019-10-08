#' Not-In
#'
#' Opposite of %in% operator. From https://stackoverflow.com/a/46867726/4089266.
#' Author: Bryce Chamberlain.
#'
#' @param needle Vector to search for.
#' @param haystack Vector to search in.
#'
#' @return Boolean vector/value of comparisons.
#' @export
#'
#' @examples
#' c(1,3,11) %ni% 1:10
'%ni%' = function( needle, haystack ) ! needle %in% haystack
