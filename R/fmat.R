#' Number Formatter
#' 
#' Flexible number formatter for easier formatting from numbers and dates into characters for display.
#'
#' @param x Vector of values to convert. If retu
#' @param type Type of format to return. If do.return == 'highcharter' this is not required.
#' @param digits Number of digits for rounding. If left blank, the funtion will guess at the best digits.
#' @param with.unit For large numbers, choose to add a suffix for fewer characters, like M for million, etc.
#' @param do.return Information to return. "formatted" returns a vector of formatted values. 
#' @param do.date.sep Separator for date formatting.
#' @param do.remove.spaces Remove extra spaces in return.
#' @param digits.cutoff Amount at which to show 0 digits. Allows for flexibility of rounding.
#'
#' @return Information requested via do.return.
#' @export
#'
#' @examples
#' 
#' fmat( 1000, 'dollar', digits = 2 )
#' 
fmat = function( 
  x = NULL, 
  type = c( 'auto', ',', '$', '%', '.', 'mdy', 'ymd', 'date', 'dollar', 'dollars', 'count', 'percentage', 'decimal' ), 
  do.return = c( 'formatted', 'highcharter' ),
  digits = NULL, 
  with.unit = FALSE, 
  do.date.sep = '/',
  do.remove.spaces = FALSE,
  digits.cutoff = NULL
){

  # fix args.
  type = trimws(type)
  do.return = trimws(do.return)
  
  type = match.arg(type)
  do.return = match.arg(do.return)
  
  if( !null(x) ){
    
    if( all( na(x) ) ) return(x)
    
    do.date = ! is.numeric(x) && ! charnum(x) && likedate(x)
    if( do.date ) x = todate(x)
    
    absx = if( do.date ){ x } else { abs( x ) }
    mx = max( absx, na.rm = TRUE )
    mn = min( absx, na.rm = TRUE )
    rm(absx)

    # type to consistent value.
    if(type %in% c('$', 'dollars')) type = 'dollar'
    if(type %in% c('%')) type = 'percentage'
    if(type %in% c(',')) type = 'count'
    if(type %in% c('.')) type = 'decimal'
    if(type %in% c('mdy')) type = 'date'    
    
    if( type == 'auto' ){
      
      if( do.date ){ type = 'date' } else
      if( mx < .1 ){ type = 'decimal' } else
      if( mx < 1 ){ type = 'percentage' } else
      { type = 'count' }
      
    }
    
  }
  
  if( is.null(x) && do.return == 'formatted' ) stop( '[x] argument is required when do.return = "formatted". Error E906 fmat.' )

  if( !is.null(x) ) nas = which( is.na(x) )
  
  if(type == 'dollar'){
    
    if( do.return == 'formatted' ){

      if( !(is.null(digits)) && digits > 0){
        x = format(as.numeric(x), nsmall = digits, big.mark = ",", scientific = FALSE)
        x = stringr::str_pad( x, max( nchar(x) ), pad = ' ' )
        x = paste0( '$ ', x )
      }
      
      else{
        x = nSigFormatter( x = x, digits = digits, with.unit = with.unit, max = mx, digits.cutoff = digits.cutoff )
        x = stringr::str_pad( x, max( nchar(x) ), pad = ' ' )
        x = paste0( '$ ', x )
      }
      
    } else if( do.return == 'highcharter' ){
      
      return( list(
        tooltip = list( valueDecimals = digits, valuePrefix = '$' ),
        format = cc( "${value:,.", digits, "f}" ),
        pointFormat = cc( '{series.name}: ${point.y:,.', digits, 'f}' )
      ) )
      
    }
    
  } else if(type == 'percentage'){
    
    if( null( digits ) ){
      if( exists('mn') ){ 
        digits = ifelse( mn > if( !is.null(digits.cutoff) ){ digits.cutoff } else { .01 }, 0, 2 )
      } else {
        digits = 0
      }
    }
    
    if( do.return == 'formatted' ){
      
      x = paste( round( x * 100 , digits ), '%', sep = '' )

    } else if( do.return == 'highcharter' ){
      
      return( list( 
        tooltip = list( valueDecimals = digits, valueSuffix = '%' ),
        format = cc( "{value:.", digits, "f}%" ),
        pointFormat = cc( '{series.name}: {point.y:.', digits, 'f}%' )
      ) )
      
    }
    
  } else if(type == 'count'){
    
    if( do.return == 'formatted' ){
      
      x = nSigFormatter( x = x, digits = digits, with.unit = with.unit, max = mx, digits.cutoff = digits.cutoff )
      
    } else if( do.return == 'highcharter' ){
      
      return( list( 
        
        tooltip = list( valueDecimals = digits ),
        format = cc( "{value:,.", digits, "f}" ),
        pointFormat = cc( '{series.name}: {point.y:,.', digits, 'f}' )
        
      ) )
      
    }
    
  } else if(type == 'decimal'){

    if( null( digits ) ){
      if( exists('mn') ) {
        digits = ifelse( mn > if( !is.null(digits.cutoff) ){ digits.cutoff } else { 1 }, 2, 4 )
      } else {
        digits = 3
      }
    }
    
    if( do.return == 'formatted' ){
      
      x = format( round( x, digits ), nsmall = digits, scientific = FALSE )
      
    } else if( do.return == 'highcharter' ){
      
      return( list( 
        tooltip = list( valueDecimals = digits ),
        format = cc( "{value:.", digits, "f}" ),
        pointFormat = cc( '{series.name}: {point.y:.', digits, 'f}' )
      ) )
      
    }
    
  } else if(type %in% c('date', 'ymd')){
  
    if( do.return == 'formatted' ){
      
      iformat = cc( if( type == 'ymd' ){ c( '%Y', '%m', '%d' ) } else { c( '%m', '%d', '%Y' ) }, sep = do.date.sep )
      
      x = gsub( '^0', '', format( x, iformat ) )
  
      rm(iformat)
      
    } else if( do.return == 'highcharter' ){
      
      warning( 'fmat not set up for do.return "highcharter"' )
      
      return( list() )
      
    }
      
  }
  
  x[ sapply(x, nanull) | x %in% c( -Inf, Inf ) ] <- 'n/a'
  
  if( do.remove.spaces ) x = gsub( ' ', '', x )
  
  x[nas] <- NA
  
  return(x)
  
}

nSigFormatter <- function( x, digits, with.unit, allow.digit.override = TRUE, max, digits.cutoff ){
  
  if( !is.numeric(x) && !charnum(x) ) return(x) # in case a character is passed.
  
  # Get minimum value and determine the amount to divide x by.
  divxby = 1
  isuffix = ''
  
  if( with.unit ){
    if(max >= 1000000000){
      divxby = 1000000000
      isuffix = 'B'

    } else if(max >= 1000000){
      divxby = 1000000
      isuffix = 'M'

    } else if(max > 500){
      divxby = 1000
      isuffix = 'K'
      
    } else {
      divxby = 1
      isuffix = ''
    }
  }
  
  # Apply div by.
  x = x / divxby
  mn = min(x, na.rm = TRUE)
  
  # determine rounding.
  if(nanull(digits)) digits = ifelse(mn > if( !is.null(digits.cutoff) ){ digits.cutoff } else { 25 }, 0, 2)
  if(nanull(digits)) digits = 0 # this is the result if mn is NA

  # special adjustment for digits 0 which format will no accept.
  if(digits == 0){
    x = round(x, 0)
    digits = 1
  }
  
  # Divide and add the indicator (billion, million, thousand) and apply rounding.
  x = format(round(x, digits = digits), big.mark = ',', scientific = FALSE)
  if( with.unit ) x = paste0( x , ifelse( isuffix != '', paste0( ' ', isuffix ), '' ) )
  
  return(x)
  
}