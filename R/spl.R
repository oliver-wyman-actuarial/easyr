#' Sample
#' 
#' Extracts a uniform random sample from a dataset or vector. Provides a simpler API than base R.
#' Author: Bryce Chamberlain. Tech Review: Maria Gonzalez.
#'
#' @param x Data to sample from.
#' @param n Number or percentage of rows/values to return. If less than 1 it will be interpreted as a percentage.
#' @param warn Warn if sampling more than the size of the data.
#' @param replace Whether or not to sample with replacement.
#' @param ... Other parameters passed to sample()
#'
#' @return Sample dataframe/vector.
#' @export
#'
#' @examples
#' spl( c(1:100) )
#' spl( c(1:100), n = 50 )
#' spl( iris )
spl <- function( x, n = 10, warn = TRUE, replace = FALSE, ... ){

  # Vectors.
  if( is.null( nrow(x) ) ){

    if( n < 1 ) n = ceiling( n * length(x) )

    if( !replace ){
      
      if( n > length(x) && warn ) warning( glue::glue( 
        'easyr::spl: You have sampled more [{n}] than the number of available items [{length(x)}] and chosen replace = FALSE. Returning the maximum available length instead.'
      ))

      n = min( c( n, length(x) ) )

    }

    return( 
      x[ sample( x = 1:length(x), size = n, replace = replace, ... ) ] 
    )

  # Data frames.
  } else {

    if( n < 1 ) n = ceiling( n * nrow(x) )

    if( !replace ){
      
      if( n > nrow(x) && warn ) warning( glue::glue(
        'easyr::spl: You have sampled more [{n}] than the number of available items [{nrow(x)}] and chosen replace = FALSE. Returning the maximum available rows instead.'
      ))

      n = min( c( n, nrow(x) ) )

    }

    return( 
      x[ sample( x = 1:nrow(x), size = n, replace = replace, ... ) , 1:ncol(x), drop = FALSE ]
    )
  }
  
}