#' Get Data Dictionary
#' 
#' Get information about a Data Frame or Data Table. Use getinfo to explore a single column instead.
#' If you like, use ecopy function or agument to copy to the clipboard so that it can be pasted into Excel. Otherwise it returns a data frame.
#' Author: Scott Sobel. Tech Review & Modifications: Bryce Chamberlain.
#'
#' @param x Data Frame or Data Table.
#' @param topn Number of top values to print.
#' @param botn Number of bottom values to print.
#' @param na.strings Strings to consider NA.
#' @param do.atype Auto-determine variable types. If your data already has types set, skip this to speed up the code.
#' @param ecopy Use ecopy function or agument to copy to the clipboard so that it can be pasted into Excel.
#'
#' @export
#'
#' @examples
#' dict(iris)
dict <- function( 
  x, topn = 5, botn = 5, na.strings = easyr::nastrings, do.atype = TRUE, ecopy = FALSE
) {

  # Apply auto-type to correctly pick data types.
  x = easyr::atype(x)
  
  idts = NULL
  
  for( icol in colnames(x) ) {
    
    # Convert factors to strings (easier to work with) and NA strings to NA.
    if( is.factor(x[[icol]]) ) x[[icol]] <- as.character(x[[icol]])
    if( is.character(x[[icol]]) ) x[[icol]][ x[[icol]] %in% na.strings ] <- NA
    
    uvals = table( sort( x[[icol]] ) )
    num.na = sum( is.na( x[[icol]] ) )
    
    idt = data.frame(
      
      Name = icol,
      `Class` = class( x[[icol]] )[1],
      `# Distinct` = nrow(uvals),
      `Top Values` = paste( utils::head( names(uvals), topn ), collapse = ', ' ),
      `Bottom Values` = paste( utils::tail( names(uvals), botn ), collapse = ', ' ),
      
      `# Missing` = num.na,
      `% Missing` = num.na / nrow(x),
      
      stringsAsFactors = FALSE, check.names = FALSE
      
    )
    
    if( is.numeric( x[[icol]] ) ){

      # integers will cause warnings. convert them to numeric.
      x[[icol]] = as.numeric( x[[icol]] )
      
      num.0 = sum( !is.na(x[[icol]]) & x[[icol]] == 0 )
      num.neg = sum( !is.na(x[[icol]]) & x[[icol]] < 0 )
      
      idt = dplyr::mutate( idt,
        
        `# =0` = num.0,
        `% =0` = num.0 / nrow(x),
        
        `# <0` = num.neg,
        `% <0` = num.neg / nrow(x),
        
        `Mode` = utils::head( names(uvals), 1 ),
        Average = mean( x[[icol]], na.rm = TRUE ),
        Min = min( x[[icol]], na.rm = TRUE ),
        Max = max( x[[icol]], na.rm = TRUE ),
        Sum = sum( x[[icol]], na.rm = TRUE )
        
      )
      
      rm( num.0, num.neg )
      
    }
    
    idts = dplyr::bind_rows( idts, idt )
    
    rm( idt, uvals, num.na, icol )
    
  }
  
  for( i in colnames(idts) ){
    
    if( is.numeric( idts[[i]] ) ){
      
      if( grepl( '%', i, ) ){
        idts[[i]] = fmat( idts[[i]], '%' )
      } else { 
        idts[[i]] = fmat( idts[[i]] )
      }
      
    }
    
    idts[[i]] = stringr::str_trunc( stringr::str_trim( as.character( idts[[i]] ) ), 50 )
    
    idts[[i]][ is.na(idts[[i]]) | idts[[i]] %in% c( 'NA', 'NA%' ) ] <- ''
    
  }
  
  # Copy to the clipboard, so you can paste into excel/notepad.
  if( ecopy ){
    easyr::ecopy( idts, "cols" )
    cat( glue::glue( '\n Data dictionary copied to clipboard. Ready to paste to Excel. {nrow(x)} Rows x {ncol(x)} Columns. \n' ) )
  } else {
    return( idts )
  }
  
}