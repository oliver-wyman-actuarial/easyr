#' Begin
#' 
#' Perform common operations before running a script. Includes clearing environment variables, disabling scientific notation, loading common packages, and setting the working directory to the location of the current file.
#'
#' @param wd Path to set as working directory. If blank, the location of the current file open in RStudio will be used if available. If FALSE, the working directory will not be changed.
#' @param load Packages to load. If not available, they'll be installed.
#' @param keep Environment objects to keep. If blank, all user-defined objects will be removed from the environment.
#' @param scipen Do scientific notation in output?
#' @param verbose Print information about what the function is doing?
#' @param repos choose the URL to install from.
#'
#' @export
#'
#' @examples
#' 
#' begin()
begin = function(
  wd = NULL, 
  load = c( 'magrittr', 'dplyr' ),
  keep = NULL,
  scipen = FALSE,
  verbose = TRUE,
  repos = 'http://cran.us.r-project.org'
){
  
  # clear workspace.
  envobjs = ls( all.names = TRUE, envir = parent.frame() )
  drop = setdiff( envobjs, keep )
  # check if attempting to keep any objects not in environment
  if( length( keep ) > 0 ){
    missing = setdiff( keep, envobjs )
    if( length( missing ) > 0 ){ 
      stop( 
        paste0( 'Attempting to keep objects not in environment: [', 
        paste0( missing, collapse = ', ' ), ']. ' 
      )) 
    }
  }
  # clear vars not set to keep
  rm( list = drop, envir = parent.frame() )
  if( verbose ) cat( 
    'Cleared variables from environment: [', 
    paste0( drop, collapse = ', ' ), ']. \n' 
  )
  
  # set working directcory.
  
    # use tryCatch since this will error out if it isn't being run in RStudio.
    if( is.null( wd ) ) wd = tryCatch(
      dirname( rstudioapi::getSourceEditorContext()$path ),
      error = function(e) FALSE
    )
    
    if( !is.logical(wd) || wd == TRUE ){
      setwd( wd )
      if( verbose ) cat( 'Set workspace to [', wd, ']. \n' )
    }
  
  # no scientific notation
  if( ! scipen ){
    options( scipen = 999 )
    if( verbose ) cat( 'Disabled scientific notation. \n' )
  }
  
  # load common packages.
  if( length( load ) > 0 ){
    easyr::usepkg( load, repos = repos)
    if( verbose ) cat( 'Loaded: [', paste0( load, collapse = ', ' ), ']. \n' )
  }
  
  # run functions.
  if( dir.exists( 'functions' ) ){
    easyr::runfolder( 'functions', verbose = verbose )
  }
  if( dir.exists( 'fun' ) ){
    easyr::runfolder( 'fun', verbose = verbose )
  }

}