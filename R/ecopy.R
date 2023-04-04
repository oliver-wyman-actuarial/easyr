#' Copy to Clipboard
#' 
#' Copies a data.frame or anything that can be converted into a data.frame. After running this, you can use ctrl+v or Edit > Paste to paste it to another program, typically Excel.
#' A simple use case would be ecopy(names(df)) to copy the names of a data frame to the clipboard to paste to Excel or Outlook.
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param x Object you'd like to copy to the clipboard.
#' @param showrowcolnames (Optional) Show row and column names. Choose 'none', 'cols', 'rows', or 'both'.
#' @param show (Optional Boolean)  Set to 'show' if you want to also print the object to the console.
#' @param buffer (Optional) Set clipboard buffer size.
#' 
#' @export
#'
#' @examples
#' ecopy( iris, showrowcolnames = "cols", show = 'show' )
#' ecopy(iris)
# # Now paste the data into Excel, email, etc.
ecopy <- function( 
  x, 
  showrowcolnames = c( 'cols', 'rows', 'both', 'none' ), 
  show = FALSE,
  buffer = 1024 
) { 

  # writing to clipboard is not handled in Unix.
  if( .Platform$OS.type == "unix" ) {
    cat( 'easyr::ecopy does not work on unix. \n' )
    return()
  }
  
  # Valicate showrowcolnames.
  
    showrowcolnames = match.arg( showrowcolnames )
  
  # Write to clipboard.
    
    # for a talbe, use write.table.
    if( !is.null( ncol(x) ) ){
      
      utils::write.table( x,
        file = paste0("clipboard-", buffer), sep="\t", 
        row.names = showrowcolnames == "rows" || showrowcolnames == "both", 
        col.names = showrowcolnames == "cols" || showrowcolnames == "both"
      )

    # otherwise, write to text.
    } else {

      # see https://stackoverflow.com/questions/17140424/placing-a-string-on-the-clipboard-without-a-newline
      # needs an extra character which gets removed.
      utils::writeClipboard( charToRaw( paste0( x, ' ' ) ) )

    }
  
  # Return the data if 'show'.
    
    if (show == "show") { return(x) }
  
}