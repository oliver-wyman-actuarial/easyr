#' Factor-friendly Coalesce
#' 
#' Coalesce function that matches and updates factor levels appropriately.
#' Checks each argument vector starting with the first until a non-NA value is found.
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
  if(length(args) == 1) return(args[[1]])
  
  # Check for factor.
  isfactor = any( sapply( args, is.factor ) )
  isordered = any( sapply( args, is.ordered ) )
  
  # Convert factors to strings.
  for( i in 1:length(args) ) if( is.factor( args[[i]] ) ) args[[i]] <- levels(args[[i]])[args[[i]]]

  # coalesce values.
  x = args[[1]]
  for(i in 1:length(args)){
    isna = which(is.na(x))
    if( length(isna) == 0 ){
      break
    } else {
      x[isna] = args[[i]][isna]
    }
  }
  
  # Apply factor.
  if( isfactor ) x = factor( x, ordered = isordered )
  
  return(x)  
  
}
