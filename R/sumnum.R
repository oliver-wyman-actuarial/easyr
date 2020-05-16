#' Summarize All Numeric Columns
#' 
#' Easily summarize at all numeric variables. Helpful for flexibly summarizing without knowing the columns. Defaults to sum but you can send a custom function through also. Typically pass in a data frame after group_by.
#'
#' @param x Grouped tibble to summarize.
#' @param do.fun Function to use for the summary. Passed to dplyr::summarize(). Can be a custom function. Defaults to sum().
#' @param except Columns names, numbers, or a logical vector indicating columns NOT to summarize.
#' @param do.ungroup Run dplyr::ungroup() after summarizing the prevent future issues with grouping.
#' @param ... Extra args passed to dplyr::summarize() which are applied as arguments to the function passed in do.fun.
#'
#' @return Summarized data frame or tibble.
#' @export
#'
#' @examples
#' 
#' require(dplyr)
#' require(easyr)
#' 
#' sumnum( group_by( cars, speed ) )
#' sumnum( group_by( cars, speed ), mean )
#' sumnum( cars )
#' 
sumnum = function( x, do.fun = NULL, except = c(), do.ungroup  = TRUE, ... ){

  # use as.numeric to avoid integer overflow issues.
  auto.fn = FALSE
  if( is.null( do.fun ) ){
    do.fun = function(x) sum( as.numeric(x), ... )
    auto.fn = TRUE
  }
  
  if( is.numeric( except ) | is.logical( except ) ) except = colnames(x)[ except ]
  if( is.factor( except ) ) except = as.character( except )
  
  grouped.cols = unlist( dplyr::groups(x) )
  
  sum.cols = setdiff( 
    colnames(x)[ sapply(x, is.numeric) & ! ( colnames(x) %in% except ) ],
    grouped.cols
  )
  
  if( auto.fn ){
    x = dplyr::summarize_at( .tbl = x, .vars = sum.cols, .funs = list(do.fun) )
  } else {
    x = dplyr::summarize_at( .tbl = x, .vars = sum.cols, .funs = list(do.fun), ... )
  }
  
  if( do.ungroup  ) x = dplyr::ungroup( x )
  
  return(x)  
  
}