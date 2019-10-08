#' Convert to Number
#' 
#' Flexible number conversion for converting strings to numbers. Handles $ , ' and spaces.
#' Author: Bryce Chamberlain. Tech review: Dominic Dillingham.
#'
#' @param x Vector to convert.
#' @param nazero (Optional) Convert NAs to 0. Defaults to TRUE, if FALSE NAs will stay NA.
#' @param preprocessed.values Strings need to have NAs set, lowercase and be trimmed before they can be checked. To avoid doing this multiple times, you can pass these processed values to the function.
#' @param checkdate Check if the column is a date first. If this has already been done, set this to FALSE so it doesn't run again.
#' @param nastrings Vector of characters to be considered NAs. todate will treat these like NAs. Defaults to the easyr::nastrings list.
#' @param ifna Action to take if NAs are created. 'return-unchanged'  returns the sent vector unchanged; 'warning' results in a warning and returns the converted vector with new NAs; 'error' results in an error; return-na returns data with new NAs and prints via cat if verbose.
#' @param verbose Choose to view messaging.
#' @param remove.chars Remove characters for aggressive conversion to numbers.
#' @param do.logical Check for logical-form vectors.
#' @param do.try.integer Return an integer if possible. Integers are a more compact data type and should be used whenever possible.
#'
#' @return Converted vector.
#' @export
#'
#' @examples
#' tonum( c('123','$50.02','30%','(300.01)',NA,'-','') )
#' tonum( c('123','$50.02','30%','(300.01)',NA,'-',''), nazero = FALSE )
tonum <- function(

  # generic arguments.
  x,
  preprocessed.values = NULL,
  nastrings = easyr::nastrings,
  ifna = c( 'return-unchanged', 'error', 'warning', 'return-na' ),
  verbose = TRUE,

  # specific to numbers.
  nazero = FALSE,
  checkdate = TRUE,
  remove.chars = FALSE,
  do.logical = TRUE,
  do.try.integer = TRUE

) return( totype(

  type = 'num',

  # generic arguments.  
  x = x,
  nastrings = nastrings,  
  preprocessed.values = preprocessed.values,
  ifna = ifna,
  verbose = verbose,

  # specific arguments
  do.try.integer = do.try.integer,

  # functions for conversion.
  type.preprocess = function(x){
    
      # Remove any characters that don't contribute to numeric values if chosen.
      if( remove.chars ){
        
        # Special handling of eE, remove when not surrounded by numbers on both sides ( not a scientific number ).
        x = gsub( '[^0-9][eE][^0-9]', '', x )
        
        # Remove the rest.
        x = gsub( '[^0-9()%+-.,eE]', '', x )
        
      }

      return(x)

  },

  na.value = as.integer(NA),

  type.conversion = function(x){

    # boolean.
    if( do.logical && is.logical(x) ) 

    # Try date conversion. Dates will be considered as numbers.
    if( checkdate ){
      
      # If the vector can be converted to a date, return it unchanged since it is not a number.
      isdate = lubridate::is.Date( todate( x, preprocessed.values = x, ifna = 'return-unchanged', verbose = FALSE ) )
      
      if( isdate ){

        if( verbose ) cat( 'easyr::tonum( checkdate = TRUE ) is returning values unchanged since the vector looks like a date. \n' )

        # return NAs which will result in the unchanged vector being returned.
        return( rep( NA, length(x) ) )
      }

      rm(isdate)

    }

    test.conversion = x
  
    # Remove any unnecessary parts.
    test.conversion = gsub( "[,' $]",'', test.conversion )

    # identify and adjust percentages.
    ipcts <- grepl( '%', test.conversion )
    test.conversion[ ipcts ] <- suppressWarnings( as.numeric( gsub( "[%,' ]", '', test.conversion[ ipcts ] ) ) / 100 )

    # replace ( ) with negative.
    test.conversion <- gsub( '^[(]([^)]+)[)]$', '-\\1', test.conversion )

    # replace 0 strings with 0.
    test.conversion[ !is.na(test.conversion) & grepl( '^-?$', test.conversion ) ] <- 0

    # If applicable, replace NAs with 0.
    if( nazero && !all( is.na(test.conversion)) ) test.conversion[ is.na(test.conversion) ] <- 0

    # Remove remaining unnecessary parts. and convert to numeric.
    test.conversion <- suppressWarnings( as.numeric(test.conversion) )

    # Try integer conversion.
    if( do.try.integer ){
      test.conversion.int = suppressWarnings( as.integer( test.conversion ) )
      if( all( eq( test.conversion.int, test.conversion ) ) ) test.conversion = test.conversion.int
    }

    return( test.conversion )

  }

) )