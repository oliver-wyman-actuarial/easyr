#' Convert Excel Number to Date
#' 
#' Converts dates formatted as long integers from Excel to Date format in R, accounting for known Excel leap year errors.
#' Author: Bryce Chamberlain. Tech review: Dominic Dillingham.
#' 
#' @param x  Vector of values.
#' @param origin  Zero value to use in date conversion. Older version of excel might use a different value.
#' 
#' @param nastrings Vector of characters to be considered NAs. todate will treat these like NAs. Defaults to the easyr::nastrings list.
#' @param preprocessed.values Strings need to have NAs set, lowercase and be trimmed before they can be checked. To avoid doing this twice, you can tell the function that it has already been done.
#' @param ifna Action to take if NAs are created. 'return-unchanged'  returns the sent vector unchanged; 'warning' results in a warning and returns the converted vector with new NAs; 'error' results in an error.
#' @param verbose Choose to view messaging.
#' @param allow_times Return values with time, not just the date.
#' @param do.month.char Convert month character names like Feb, March, etc.
#' 
#' @param min.acceptable  Set NA if converted value is less than this value. Helps to prevent numbers from being assumed as dates. Set NULL to skip this check.
#' @param max.acceptable  Set NA if converted value is greater than this value. Helps to prevent numbers from being assumed as dates. Set NULL to skip this check.
#' 
#' @return Vector of converted values.
#' 
#' @export
#' @examples 
#' xldate( c('7597', '42769', '47545', NA ) )
xldate <- function( 

  x, 
  origin = "1899-12-30",

  nastrings = easyr::nastrings,
  preprocessed.values = NULL,
  ifna = c( 'return-unchanged', 'error', 'warning', 'return-na' ),
  verbose = TRUE,
  
  allow_times = FALSE,
  do.month.char = TRUE,
  min.acceptable = lubridate::ymd( '1920-01-01' ),
  max.acceptable = lubridate::ymd( '2050-01-01' )
  
) return( totype( 

  type = 'xldate',

  # generic arguments.  
  x = x,
  nastrings = nastrings,
  preprocessed.values = preprocessed.values,
  ifna = ifna,
  verbose = verbose,

  # functcions for conversion.
  type.preprocess = function(x){

      return(x)

  },
  
  na.value = as.Date( NA, origin = '1970-01-01' ),

  type.conversion = function(x){

      # Override NULL max acceptable. Even if we aren't using the default, we need to have some requirements.
      if( is.null( max.acceptable ) ) max.acceptable = lubridate::ymd( '9999-12-31' )
      if( is.null( min.acceptable ) ) min.acceptable = lubridate::ymd( '0000-1-1' )

      test.conversion =  suppressWarnings( as.POSIXct( as.numeric(x) * 60 * 60 * 24, origin = origin, tz = "UTC" ) )
    
      if( !allow_times ) test.conversion = as.Date( test.conversion )
  
      # Certain dates that don't make sense should be replaced by NAs.
        
        if( any( !is.na( test.conversion ) ) ){
          
          if( isval( max.acceptable ) ) test.conversion[ which( test.conversion > max.acceptable ) ] <- NA
          if( isval( min.acceptable ) ) test.conversion[ which( test.conversion < min.acceptable ) ] <- NA
          
        }

      return( test.conversion )

  }

) )