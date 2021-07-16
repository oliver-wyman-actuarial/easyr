#' Convert to Date
#'
#' Flexible date conversion function using lubridate. Works with dates in many formats, without needing to know the format in advance. 
#' Only use this if you don't know the format of the dates before hand. Otherwise, lubridate functions parse_date_time, mdy, etc. should be used.
#' Author: Bryce Chamberlain. Tech review: Dominic Dillingham.
#'
#' @param x Value or vector to be converted.
#' @param allow_times Set to TRUE to allow DateTimes as output, otherwise this will always convert to Dates (losing time information). This is better for binding data, hence the default FALSE.
#' @param nastrings Vector of characters to be considered NAs. todate will treat these like NAs. Defaults to the easyr::nastrings list.
#' @param aggressive.extraction todate will take dates inside long strings (like filenames) and convert them to dates. This seems to be the preferred outcome, so we leave it as default (TRUE). However, if you want to avoid this you can do so via this option (FALSE).
#' 
#' @param preprocessed.values Strings need to have NAs set, lowercase and be trimmed before they can be checked. To avoid doing this multiple times, you can pass these processed values to the function.
#' @param ifna Action to take if NAs are created. 'return-unchanged'  returns the sent vector unchanged; 'warning' results in a warning and returns the converted vector with new NAs; 'error' results in an error; 'return-na' returns new NAs without a warning.
#' @param verbose Choose to view messaging.
#'
#' @param do.month.char Attempt to convert month names in text. lubridate does this by default, but sometimes it can result in inaccurate dates. For example, "Feb 2017" is converted to 2-20-2017 even though no day was given.
#' @param do.excel Check for excel-formatted numbers.
#' @param min.acceptable  Set NA if converted value is less than this value. Helps to prevent numbers from being assumed as dates. Set NULL to skip this check. Does not affect character conversions.
#' @param max.acceptable  Set NA if converted value is greater than this value. Helps to prevent numbers from being assumed as dates. Set NULL to skip this check. Does not affect character conversions.
#' 
#' @return Converted vector using lubridate::parse_date_time(x,c('mdy','ymd','dmy'))
#' @export
#'
#' @examples
#' x <- c( '20171124', '2017/12/24', NA, '12/24/2017', '5/11/2017 1:51PM' ) 
#' x2 <- todate(x)
#' x2
todate <- function(
  
  x, 

  nastrings = easyr::nastrings,
  aggressive.extraction = TRUE,
  
  preprocessed.values = NULL,
  ifna = c( 'return-unchanged', 'error', 'warning', 'return-na' ),
  verbose = TRUE,
  
  allow_times = FALSE,
  do.month.char = TRUE,
  do.excel = TRUE,
  min.acceptable = lubridate::ymd( '1920-01-01' ),
  max.acceptable = lubridate::ymd( '2050-01-01' )
  
) return( totype( 

  type = 'date',

  # generic arguments.  
  x = x,
  nastrings = nastrings,  
  preprocessed.values = preprocessed.values,
  ifna = ifna,
  verbose = verbose,

  # functcions for conversion.
  type.preprocess = function(x){
      
      # Remove month names if they are there and we have chosen to not include them.
      if( ! do.month.char ) x = gsub( 'jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec', '', x, ignore.case = TRUE )

      # Process troublesome characters.
      if( aggressive.extraction ) x = gsub( '[_]', ' ', x )

      return(x)

  },
  
  na.value = as.Date( NA, origin = '1970-01-01' ),

  type.conversion = function(x){
    
    # Try excel date conversion. If we get no new NAs then we are done.
      if( ! do.excel ){

        test.conversion.xl = as.Date( rep( NA, length(test.conversion.xl) ) )

      } else {
        
        test.conversion.xl = xldate(
          x = x, 
          preprocessed.values = x,
          ifna = 'return-na',
          verbose = FALSE,
          allow_times = TRUE,
          min.acceptable = min.acceptable,
          max.acceptable = max.acceptable
        )
        
        # If the result is a character, the date conversion failed.
        if( class( test.conversion.xl )[1] %ni% c( 'Date', 'POSIXct', 'POSIXt' ) ){
          
          # Set all to NAs for checking later.
          test.conversion.xl = as.Date( rep( NA, length(test.conversion.xl) ) )
          
        # Otherwise, round to dates if necessary and return the result if no new NAs were created.
        } else {
          
          if( ! allow_times ) test.conversion.xl = as.Date( test.conversion.xl )
          
        }
        
      }
      
    # Try lubridate (character) conversion.
    
      # Handle special case like 3312019: Dates should have the same length unless the month doesn't have leading zeros.
      # Lubridate can't read in this case, for example mdy('3312019') gives NA but mdy('03312019') converts ok. 
      # So, in this special case if they max length of any character is 8 (or 7 for 0s missing), we pad 0s so everything is length 8. 
      # This should not affect other formats.
      do.pad = which( nchar(x) %in% 7:8 & ! grepl( '^0|[^0-9]', x ) )
      x[ do.pad ] <- pad0( x[ do.pad ], 8 )
      rm(do.pad)
    
      # Do date conversion. 
      # parse_date_time allows reading in dates without knowing the format.
      # Try without dmy first (since this is rare in U.S.). 
      # For some reason parse_date_time wants to pick it though, even when we give it mdy as an option.
        
        timeformats_primary = c( 'mdy HM', 'mdy IMp', 'ymd HM', 'ymd IMp', 'dmy HM', 'dmy IMp' , 'mdy', 'ymd', 'dmy' )
        
        # If there are lots of excel dates this will cause conversion to fail. 
        # If we were fairly successful with excel conversion, remove any succesful excel conversions.
        char.convert = x
        if( sum( !is.na(test.conversion.xl) ) / sum( !is.na(x) ) > 0.25 ) char.convert[ ! is.na( test.conversion.xl ) ] <- NA
        
        test.conversion.char = suppressWarnings( lubridate::parse_date_time( char.convert, timeformats_primary ) ) # we'll check for NAs manually.
        rm(char.convert) 
        
        if( !is.null(min.acceptable) ) test.conversion.char[ test.conversion.char < min.acceptable ] <- NA
        if( !is.null(max.acceptable) ) test.conversion.char[ test.conversion.char > max.acceptable ] <- NA
      
        # Did we create NAs?
        if( any( !is.na(x) & is.na( test.conversion.char ) & is.na( test.conversion.xl ) ) ){
          
          # Try dmy.
          timeformat_fallback = c( 'dmy', 'dmy HM' )
          test.conversion.char = coalf( test.conversion.char, suppressWarnings( lubridate::parse_date_time( x, timeformat_fallback ) ) )
          if( !is.null(min.acceptable) ) test.conversion.char[ test.conversion.char < min.acceptable ] <- NA
          if( !is.null(max.acceptable) ) test.conversion.char[ test.conversion.char > max.acceptable ] <- NA
          
        }
        
      # Check for aggressive conversion.
      if( ! aggressive.extraction ){
        
        # Compare change in character length during date conversion. If we have significantly reduced any character length then we have aggressively converted.
        # Replace any aggressive conversions with NA.
        
        test.conversion.char[ !is.na( test.conversion.char ) & !is.na( x ) & nchar( test.conversion.char ) / nchar( x ) < .7 ] <- NA
        
      }
      
      # as.Date is necessary to turn time into date which allows dplyr::bind_rows with dates read in by read.xlsx.
      # without it, it'll return a datetime, not a date, and this can be problematic.
      # However, we allow the option to return times if needed.
      if( ! allow_times ) test.conversion.char = as.Date( test.conversion.char )
        
    # Now, coalesce the 2 attempts to try and fill in all the values.
      
      test.conversion = if( exists( 'test.conversion.xl' ) ){ 
        
        if( class( test.conversion.char )[1] == 'POSIXct' ) test.conversion.xl = as.POSIXct( test.conversion.xl, tz = 'UTC' )
        
        coalf( test.conversion.char, test.conversion.xl )
        
      } else {
        test.conversion.char
      }

      return( test.conversion )

  }

) )