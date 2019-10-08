#' Auto-Type
#'
#' Use easyr date and number and conversion functions to automatically convert data to the most useful type available.
#' 
#' Author: Bryce Chamberlain.
#'
#' @param x Data to auto-type.
#' @param auto_convert_dates Choose to convert dates.
#' @param check_numbers Choose to convert numbers.
#' @param nazero Convert NAs in numeric columns to 0.
#' @param check_logical Choose to convert numbers.
#' @param allow_times Choose if you want to get times. Only use this if your data has times, otherwise there is a small chance it will prevent proper date conversion.
#' @param isexcel By default, we assume this data may have come from excel. This is to assist in date conversion from excel integers. If you know it didn't and are having issues with data conversion, set this to FALSE.
#' @param stringsAsFactors Convert strings/characters to factors to save compute time, RAM/memory, and storage space.
#' @param nastrings Strings to consider NA.
#' @param exclude Column name(s) to exclude.
#'
#' @return Data frame with column types automatically converted.
#' @export
#'
#' @examples
#' # create some data in all-characters.
#' x = data.frame(
#'      char = c( 'abc', 'def' ),
#'      num = c( '1', '2' ),
#'      date = c( '1/1/2018', '2018-2-01' ),
#'      na = c( NA, NA ),
#'      bool = c( 'TRUE', 'FALSE' ),
#'      stringsAsFactors = FALSE
#' )
#' 
#' # different atype options. Note how the output types change.
#' str( atype( x ) )
#' str( atype( x, exclude = 'date' ) )
#' str( atype( x, auto_convert_dates = FALSE ) )
#' str( atype( x, check_logical = FALSE ) )
atype = function( 
  
  x, 
  auto_convert_dates = TRUE,
  allow_times = FALSE,
  check_numbers = TRUE, 
  nazero = FALSE,
  check_logical = TRUE,
  isexcel = TRUE,
  stringsAsFactors = FALSE,
  
  nastrings = easyr::nastrings,
  exclude = NULL
  
){

  do.cols = setdiff( colnames(x), exclude )
  
  # Auto-convert dates and numbers.
  if( auto_convert_dates || check_numbers || check_logical ) for(i in do.cols ){
    
    tryCatch({

      # Only type characters, numbers - otherwise we'll assume typing has already happened.
      # This is because numeric might be able to be converted to integer.
      if( !is.character( x[[i]] ) && !is.factor( x[[i]] ) && !is.numeric( x[[i]] )  ) next
      
      # Start a value map we can use to track values back to the original.
      valmap = data.frame(
        init = x[[i]],
        mod = x[[i]],
        stringsAsFactors = FALSE
      )
      
      # Convert factors to characters
      valmap$mod = preprocess.for.type( valmap$mod, nastrings = nastrings )

      # Get a unique dataset of values we can use.
      uvals = data.frame(
        mod = unique( valmap$mod ),
        stringsAsFactors = FALSE
      )
      
      # Check logical.
      if( check_logical ){
        
        # Attempt conversion. If it was successful, this is a logical vector.
        test.conversion = tobool( uvals$mod, preprocessed.values = uvals$mod, verbose = FALSE, ifna = 'return-unchanged' )

        if( is.logical( test.conversion ) ){
          
          uvals$final = test.conversion
          x[[i]] <- ( dplyr:: inner_join( valmap, uvals, by = 'mod' ) )[['final']]

          rm(test.conversion, valmap, uvals)

          next

        } 

        rm(test.conversion)
        
      }

      # Check date. Must check dates before numeric since sometimes dates can be misinterpreted as numbers.
      if( auto_convert_dates ){
        
        # Attempt conversion. If it was successful, this is a logical vector.

        test.conversion = todate( uvals$mod, preprocessed.values = uvals$mod, verbose = FALSE, ifna = 'return-unchanged', allow_times = allow_times )
        if( class( test.conversion )[1] %in% c( 'Date', 'POSIXct', 'POSIXt' ) ){
          
          uvals$final = test.conversion
          x[[i]] <- ( dplyr:: inner_join( valmap, uvals, by = 'mod' ) )[['final']]

          rm(test.conversion, valmap, uvals)

          next
        } 
        
        rm(test.conversion)

      }

      # Check numeric.
      if( check_numbers ){
        
        # Attempt conversion. If it was successful, this is a logical vector.
        # checkdate = FALSE since we have already attempted date conversion.
        test.conversion = tonum( uvals$mod, preprocessed.values = uvals$mod, verbose = FALSE, ifna = 'return-unchanged', checkdate = FALSE, nazero = nazero )

        if( is.numeric( test.conversion ) ){
          
          uvals$final = test.conversion
          x[[i]] <- ( dplyr:: inner_join( valmap, uvals, by = 'mod' ) )[['final']]

          rm(test.conversion, valmap, uvals)

          next

        } 

        rm(test.conversion)

      }
      
      # If you made it this far (no successfull type conversions), convert to character, unless we are using stringsAsFactors.
      if( !stringsAsFactors ) x[[i]] <- as.character( x[[i]] )
    
    },
    error = function(e) stop( 'easyr::atype error at column [', i, '] : ', e ),
    warning = function(w) warning( 'easyr::atype warning at column [', i, '] : ', w )
    )
    
  }
  
  # Strings as factors.
  if( stringsAsFactors ) for( i in do.cols ) if( is.character( x[[i]] ) ) x[[i]] <- factor( x[[i]] )
  
  return(x)
}