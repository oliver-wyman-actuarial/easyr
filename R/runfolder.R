#' Run Folder
#' 
#' Run all the R scripts in a folder.
#' Author: Bryce Chamberlain.
#'
#' @param path Folder to run.
#' @param recursive Run all folder children also.
#' @param is.local Code is running on a local machine, not a Shiny server. Helpful for skipping items that can be problematic on the server. In this case, printing to the log.
#' @param check.fn Function to run after reach file is read-in.
#' @param run.files Optionally pass the list of files to run. Otherwise, list.files will be run on the folder.
#' @param verbose Print names of files and run-time via cat.
#' @param edit.on.err Open the running file if an error occurs.
#' @param pattern Passed to list.files. Pattern to match/filter files.
#'
#' @export
#'
#' @examples
#' # runfolder( 'R' )
runfolder = function( 

  path, 
  recursive = FALSE, 
  is.local = TRUE, 
  check.fn = NULL, 
  run.files = NULL, 
  verbose = TRUE, 
  edit.on.err = TRUE,
  pattern = '[.][Rr]$'

){
  
  if( !dir.exists(path ) ) stop( 'Path [', path, '] not found.' )
  
  if( is.null( run.files ) ) run.files = list.files( path, full.names = TRUE, recursive = recursive, pattern = pattern )

  if( verbose ) cat( '\n' )
  
  for( i in run.files ){
    
    if( is.local && verbose ) cat( '\t start \t', i, ' \n' )
    
    start = Sys.time()
    
    withCallingHandlers( tryCatch({
      
      source( i, keep.source = FALSE )
      
    },
    error = function(e){
      if(edit.on.err) tryCatch(
        rstudioapi::navigateToFile(i),
        # do nothing, this will happen if rstudio is not available.
        error=function(e) NULL
      )
      stop('
        At [', i, ']: \n \n \t [', e, '] \n
        Optionally: open this file and run it to determine which line the error occured at. 
        Sometimes you get a more helpful error message if you run from the file itself instead of from easyr::runfolder.
      ')
    }
    ),
    warning = function(w) cat( '\n \t \t ', as.character( w ), '\n' )
    )
    
    if( is.local && verbose ) cat( '\t end\t', i, ' \t [', format( Sys.time() - start, digits = 2 ), '] \n' )
    
    # Run the check function.
    if( !is.null( check.fn ) ) check.fn()
    
    rm( i, start )
  }
  
}