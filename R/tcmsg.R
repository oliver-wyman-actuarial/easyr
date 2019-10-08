#' tryCatch with Message
#'
#' Easy Try/Catch implementation to return the same message on error or warning. Makes it easier to write tryCatches.
#' Author: Bryce Chamberlain. Tech review: Lindsay Smelzter.
#'
#' @param code_block Code to run in Try Catch.
#' @param ... Strings to concatenate to form the message that is returned.
#'
#' @export
#'
#' @examples
#' tryCatch({ 
#'    tcmsg({ NULL = 1 }, 'Cannot assign to NULL','variable' ) 
#'  }, 
#'  error = function(e) print( e ) 
#'  )
#'
#' tryCatch({ 
#'    tcmsg({ as.numeric('abc') },'Issue in as.numeric()') 
#'   }, 
#'   warning = function(e) print( e ) 
#' )
tcmsg <- function( code_block, ... ){
  message = paste( ... , collapse = ' ' )
  tryCatch(
    code_block,
    error = function(e) stop( e, message ),
    warning = function(w) warning( w, message )
)}