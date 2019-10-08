#' Concatenate and run.
#' 
#' Concatenate arguments and run them as a command. Shorthand for eval( parse( text = paste0( ... ) ) ).
#' Consider also using base::get() which can be used to get an object from a string, but only if it already exists.
#' Author: Bryce Chamberlain.
#'
#' @param ... Character(s) to be concatenated and run as a command.
#'
#' @export
#'
#' @examples
#' crun( 'print(', '"hello world!"', ')')
#' crun('T', 'RUE')
crun = function( ... ) eval.parent( parse( text = paste0( ... ) ) )

# http://earlh.com/blog/2009/06/30/eval-in-r-running-code-from-a-string/