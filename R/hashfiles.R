#' Hash Files
#' 
#' Get a hash value representing a list of files. Useful for determining if files have changed in order to reset dependent caches.
#'
#' @param x Input which specifies which files to hash. This can be a vector mix of paths and files.
#' @param skip.missing Skip missing files. Default is to throw an error if a file isn't found.
#' @param full.hash By default we just hash the file info (name, size, created/modified time). Set this to TRUE to read the file and hash the contents.
#' @param verbose Print helpful messages from code.
#'
#' @return String representing hash of files.
#' @export
#'
#' @examples
#' hashfiles( '.' )
hashfiles = function( x, skip.missing = FALSE, full.hash = FALSE, verbose = FALSE ){
  
  hash.out = ''
  
  for( i in x ){
    
    if( dir.exists(i) ){
      ifiles = list.files( i, full.names = TRUE, recursive = TRUE )
    } else{
      ifiles = c( i ) 
    }
    
    for( j in ifiles ){
      
      if( !file.exists(j) ){
        if( !skip.missing ) {
          stop( 'easyr::hashfiles File not found: [', j, ']. Error E954 hashfiles' )
        } else if( verbose ) {
          cat( 'easyr::hashfiles File not found: [', j, ']. Error E954 hashfiles \n' )
        }
      }      
      
      if( file.exists(j) ){

        if( full.hash ){
            
          # Try a standard digest.
          jdigest = tryCatch({ digest::digest( file = j ) }, error = function(e) return(NULL) )
          
          # If that doesn't work, try reading and digesting.
          if( is.null(jdigest) ) jdigest = tryCatch({ digest::digest( readChar(j, file.info(j)$size) ) }, error = function(e) return(NULL) )
          if( is.null(jdigest) ) jdigest = tryCatch({ digest::digest( read.any( filename = j ) ) }, error = function(e) return(NULL) )
          
          if( is.null(jdigest) ) stop( glue::glue( "Error at digest::digest for [{j}] Error E1140 hashfiles" ) )
          
        } else {
          
          # get file info and hash it. atime is the current time so remove that, it'll always change.
          jinfo = base::file.info(j)
          jdigest = digest::digest( c( rownames(jinfo), jinfo$size ) )
          
          rm(jinfo)
          
        }
        
        # add to the running hash.
        hash.out = openssl::md5( cc( hash.out, jdigest ) )[1]
        
        rm( jdigest )
      
      }
      
      rm(j)
      
    }
    
  }
  
  return( hash.out )
  
}