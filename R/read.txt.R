#' Read File as Text
#'
#' @param filename File path and name for the file to be read in.
#' @param folder Folder path to look for the file in.
#'
#' @return Character variable containing the text in the file.
#' @export
#'
#' @examples
#' 
#' # write a files.
#' path = tempfile()
#' cat( "some text", file = path )
#' 
#' # read the file.
#' read.txt( path )
#' 
#' # cleanum.
#' file.remove( path )
#' 
read.txt = function( filename, folder = NA ){
    
    filename = filename.helper( filename, folder )
    rm(folder)
    
    x = readChar( filename, file.info(filename)$size ) 

    return(x)

}
