#' Factor-friendly Coalesce
#' 
#' Wrapper for the dplyr function "coalesce" which can handle factors appropriately. 
#' Checks each argument vector starting with the first until a non-null value is found.
#' Author: Bryce Chamberlain.
#'
#' @param ... Source vectors.
#'
#' @return Vector of values.
#' @export
#'
#' @examples
#' x <- sample(c(1:5, NA, NA, NA))
#' coalf(x, 0L)
coalf = function( ... ){
  
  args = list(...)
  
  # Check for factor.
  isfactor = any( sapply( args, is.factor ) )
  isordered = any( sapply( args, is.ordered ) )
  
  # Convert factors to strings.
  for( i in 1:length(args) ) if( is.factor( args[[i]] ) ) args[[i]] <- levels(args[[i]])[args[[i]]]
  
  # Apply coalesce.
  coalesce = dplyr::coalesce
  x = do.call( 'coalesce', args )
  
  # Apply factor.
  if( isfactor ) x = factor( x, ordered = isordered )
  
  return(x)
  
  
}
