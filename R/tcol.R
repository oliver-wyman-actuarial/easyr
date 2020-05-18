#' Transpose at Column.
#' 
#' Transpose operation that sets column names equal to a column in the original data.
#' Author: Bryce Chamberlain.
#'
#' @param x Data frame to be transposed.
#' @param header Column name/number to be used as column names of transposed data.
#' @param cols.colname Name to use for the column of column names in the transposed data.
#' @param do.atype Transpose convertes to strings, since data types are uncertain. Run atype to automatically correct variable typing where possible. This will slow the result a bit.
#'
#' @return Transposed data frame.
#' @export
#'
#' @examples
#'  # create a summary dataset from iris.
#'  x = dplyr::summarize_at( 
#'   dplyr::group_by( iris, Species ), 
#'   dplyr::vars( Sepal.Length, Sepal.Width ), list(sum) 
#'  )
#'  # run tcol
#'  tcol( x, 'Species' )
tcol <- function( x, header, cols.colname = 'col', do.atype = TRUE ){
  
  if( nrow(x) == 0 ) stop( 'Data has no rows and cannot be transposed with tcol. Error E1103 easyr::tcol' )
  
  inames = colnames(x)
  
  if( !is.numeric(header) ) header = which( inames == header )[1]
  
  if( length( header ) == 0 || header > ncol(x) ) stop( 'Invalid [header] arugment. Error E935 easyr::tcol')
  
  if( any( duplicated( x[ , header ] ) ) ) stop(
    'Header column must be unique. Error E932 easyr::tcol'
  )
  
  # convert to data frame and get the header. tibbles will break here.
  new.colnames = as.character( x[[ header ]] )
  
  x = as.data.frame( t(x), stringsAsFactors = FALSE )
  
  colnames( x ) = new.colnames
  
  x = cbind(
    data.frame( col = inames, stringsAsFactors = FALSE ),
    x
  )
  
  colnames(x)[1] = cols.colname
  
  x = x[ -header, ]
  
  if( do.atype ) x = atype(x)
  
  return( x )  
   
}