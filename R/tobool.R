#' Convert to Logical/Boolean
#'
#' Flexible boolean conversion. 
#' Author: Bryce Chamberlain.
#'
#' @param x Value or vector to be converted.
#' @param preprocessed.values Strings need to have NAs set, lowercase and be trimmed before they can be checked. To avoid doing this multiple times, you can pass these processed values to the function.
#' @param nastrings Vector of characters to be considered NAs. todate will treat these like NAs. Defaults to the easyr::nastrings list.
#' @param ifna Action to take if NAs are created. 'return-unchanged'  returns the sent vector unchanged; 'warning' results in a warning and returns the converted vector with new NAs; 'error' results in an error.
#' @param verbose Choose to view messaging.
#' @param true.vals Values to consider as TRUE.
#' @param false.vals Values to consider as FALSE.
#'
#' @return Converted logical vector.
#' @export
#'
#' @examples
#' tobool( c( 'true', 'FALSE', 0, 1, NA, 'yes', 'NO' ) )
tobool <- function( 

  # generic arguments.
  x,
  preprocessed.values = NULL,
  nastrings = easyr::nastrings,
  ifna = c( 'return-unchanged', 'error', 'warning', 'return-na' ),
  verbose = TRUE,

  # specific to booleans.
  true.vals = c( 'true',  '1', 't', 'yes' ),
  false.vals = c( 'false', '0', 'f', 'no' )

  
) return( totype( 

  type = 'bool',

  # generic arguments.  
  x = x,
  nastrings = nastrings,  
  preprocessed.values = preprocessed.values,
  ifna = ifna,
  verbose = verbose,

  # functions for conversion.
  type.preprocess = function(x){

    return(x)

  },

  na.value = as.logical(NA),

  type.conversion = function(x){
    
    # If all 0s, or all 1s, we don't want to assume. return NAs to cancel the conversion attempt.
    notna = x[ !is.na(x) ]
    if( all( notna == '0') || all( notna == '1') ) return( rep( NA, length(x) ) )
    rm(notna)

    test.conversion = x
  
    # Set identified true and false.
    test.conversion[ test.conversion %in% tolower(true.vals) ] <- TRUE
    test.conversion[ test.conversion %in% tolower(false.vals) ] <- FALSE

    test.conversion = suppressWarnings( as.logical( test.conversion ) )
    return( test.conversion )

  }

))