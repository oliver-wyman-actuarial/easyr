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
#' @param use_n_sampled_rows Used on large data sets.
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
  exclude = NULL,
  use_n_sampled_rows = min(nrow(x), 10000)
  
){

  do.cols = setdiff( colnames(x), exclude )
  
  # Auto-convert dates and numbers.
  # run it on a sample. 
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

      # remove NAs
      if(use_n_sampled_rows >= nrow(uvals)){
        uvals_sample = stats::na.omit(uvals)
      } else{ 
        uvals_sample = spl(stats::na.omit(uvals), n = use_n_sampled_rows , warn = FALSE)
      }
      
      # Check date. Must check dates before numeric since sometimes dates can be misinterpreted as numbers.
      if( auto_convert_dates && nrow(uvals_sample) > 0 ){
        
        # Attempt conversion. If it was successful, this is a logical vector.
        test.conversion = todate( uvals_sample$mod, preprocessed.values = uvals_sample$mod, verbose = FALSE, ifna = 'return-unchanged', allow_times = allow_times )
        if( class( test.conversion )[1] %in% c( 'Date', 'POSIXct', 'POSIXt' ) ){
          
          uvals$final = todate( uvals$mod, preprocessed.values = uvals$mod, verbose = FALSE, ifna = 'return-unchanged', allow_times = allow_times )
          x[[i]] <- ( emerge( x = valmap, y = uvals, by = 'mod', type = 'inner' ) )[['final']]
          
          rm(test.conversion, valmap, uvals)
          
          next
        } 
        
        rm(test.conversion)
        
      }

      # Check numeric.
      if( check_numbers && nrow(uvals_sample) > 0 ){
        
        # Attempt conversion. If it was successful, this is a numeric vector.
        # checkdate = FALSE since we have already attempted date conversion.
        test.conversion = tonum( uvals_sample$mod, preprocessed.values = uvals_sample$mod, verbose = FALSE, ifna = 'return-unchanged', checkdate = FALSE, nazero = nazero )

        if( is.numeric( test.conversion ) ){
          
          uvals$final = tonum( uvals$mod, preprocessed.values = uvals$mod, verbose = FALSE, ifna = 'return-unchanged', checkdate = FALSE, nazero = nazero )
          x[[i]] <- ( emerge( x = valmap, y = uvals, by = 'mod', type = 'inner' ) )[['final']]

          rm(test.conversion, valmap, uvals)

          next

        } 

        rm(test.conversion)

      }
      
      # Check logical.
      if( check_logical && nrow(uvals_sample) > 0 ){
        
        # Attempt conversion. If it was successful, this is a logical vector.
        test.conversion = tobool( uvals_sample$mod, preprocessed.values = uvals_sample$mod, verbose = FALSE, ifna = 'return-unchanged' )

        if( is.logical( test.conversion ) ){
          
          uvals$final = tobool( uvals$mod, preprocessed.values = uvals$mod, verbose = FALSE, ifna = 'return-unchanged' )
          x[[i]] <- ( emerge( x = valmap, y = uvals, by = 'mod', type = 'inner' ) )[['final']]

          rm(test.conversion, valmap, uvals)

          next

        } 

        rm(test.conversion)
        
      }
      
      # If you made it this far (no successfull type conversions), convert to character, unless we are using stringsAsFactors.
      if( !stringsAsFactors && nrow(uvals_sample) > 0 ) x[[i]] <- as.character( x[[i]] )
    
    },
    error = function(e) stop( 'easyr::atype error at column [', i, '] : ', e ),
    warning = function(w) warning( 'easyr::atype warning at column [', i, '] : ', w )
    )
    
  }
  
  # Strings as factors.
  if( stringsAsFactors ) for( i in do.cols ) if( is.character( x[[i]] ) ) x[[i]] <- factor( x[[i]] )
  
  return(x)
}