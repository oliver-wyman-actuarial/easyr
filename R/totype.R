# these are supporting functions for todate, tobool, tonum.

skip.classes = list(
    date = c( 'Date', 'POSIXct', 'POSIXt' ),
    num = c( 'integer' ),
    bool = c( 'logical' )
)

totype <- function(

  type = c( 'date', 'num', 'bool', 'xldate' ),
  
  # generic arguments.
  x, 
  nastrings,  
  preprocessed.values,
  ifna = c( 'return-unchanged', 'error', 'warning', 'return-na' ),
  verbose,

  # functions for conversion.
  type.preprocess,
  na.value,
  type.conversion,

  # tonum
  do.try.integer = NULL,
  multipliers = NULL
  
) {
  
  # check if the current var type is acceptable.
  if( type == 'num' && ! do.try.integer ) skip.classes$num = c( skip.classes$num, 'numeric' )
  if( 
      class(x)[1] %in% skip.classes[[type]] ||
      length(x) == 0
  ) return(x)

  # Validate inputs.
  
    type = match.arg(type)
    ifna = match.arg(ifna)
  
  # Modify values for better checking and get unique values, if this hasn't already been done.

    # if x has row names it may cause a warning.
    rownames(x) <- NULL

    # Standard processing:
    if( !is.null(preprocessed.values) ){

      # A value mapping we can use later to map processed values back to the original vector.
      imap = data.frame(
        oldval = x,
        matchval = preprocessed.values,
        stringsAsFactors = FALSE
      )

    } else {

      imap = data.frame(
        oldval = x,
        matchval = preprocess.for.type( x, nastrings = nastrings ),
        stringsAsFactors = FALSE
      )

    }
  
    # Processing unique to data type:
    imap$matchval = type.preprocess( imap$matchval )

  # Get just the uniqe values for speed.
  uvals = data.frame(
    matchval = unique(imap$matchval),
    stringsAsFactors = FALSE
  )
  uvals$midval = uvals$matchval
  uvals$finalval = NA

  # If the full vector is NA, return the same but as a date type.
  if( all( is.na(uvals$matchval) ) ) return( rep( na.value, length(x) ) )

  # Test conversion for type.
  uvals$finalval = type.conversion( uvals$midval )
  
  # If NAs were created, handle them.
  could.not.convert = uvals$matchval[ is.na( uvals$finalval ) & !is.na(uvals$matchval) ]
  could.not.convert.unprocessed = imap$oldval[ imap$matchval %in% could.not.convert ]
  
  if( length(could.not.convert) > 0 ){
    
    if( ifna == 'return-unchanged' ){ 
      if( verbose ) cat( glue::glue( 'easyr::to{type}: NAs were created. Returning the vector unchanged. \n   Problematic values include: [ { cc( utils::head(could.not.convert.unprocessed), sep = ", " ) } ] \n \n' ) )
      return(x) 
      
    } else if( ifna == 'warning' ){ 
      warning( glue::glue( 'easyr::to{type}: NAs were created. Returning data with NAs. \n   Problematic values include: [ { cc( utils::head(could.not.convert.unprocessed), sep = ", " ) } ] \n' ) )
      
    } else if( ifna == 'return-na' ){ 
      if( verbose ) cat( glue::glue( 'easyr::to{type}: NAs were created. \n   Problematic values include: [ { cc( utils::head(could.not.convert.unprocessed), sep = ", " ) } ] \n' ) )
      
    } else {
      stop( glue::glue( 'easyr::to{type}: NAs were created. \n   Problematic values include: [ { cc( utils::head(could.not.convert.unprocessed), sep = ", " ) } ] \n' ) )
      
    }
    
  }

  # Join back to the original data to get values.
  imap = ijoinf( imap, uvals, by = 'matchval' )

  return( imap$finalval )

}