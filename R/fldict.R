#' Get Data Dictionary for Files in Folder
#' 
#' Get information about data files in a folder path. Use dict() on a single data frame or getinfo(0) to explore a single column.
#' Author: Bryce Chamberlain.
#'
#' @param folder File path of the folder to create a dictionary for. Pass either this or file.list. file.list will override this argument.
#' @param file.list List of files to create a combined dictionary for. Pass either this or folder. This will ovveride folder. 
#' @param pattern Pattern to match files in the folder. By default we use a pattern that matches read.any-compatible data files and skips temporary Office files. Passed to list.files.
#' @param ignore.case Ignore case when checking pattern. Passed to list.files.
#' @param recursive Check files recursively. Passed to list.files.
#' @param ... Other arguments to read.any for reading in files. Consider using a first_column_name vector, etc.
#'
#' @export
#'
#' @return List with the properties:
#' \item{s}{Summary data of each dataset.}
#' \item{l}{Line data with a row for each column in each dataset.}
#'
#' @examples
#' 
#' folder = system.file('extdata', package = 'easyr')
#' fl = fldict(folder)
#' names(fl)
#' 
#' fl$sheets
#' fl$columns
#'
fldict = function( folder = NULL, file.list = NULL, pattern = '^[^~]+[.](xls[xmb]?|csv|rds|xml)', ignore.case = TRUE, recursive = TRUE, ...  ){

    if( is.null(file.list) ) if( is.null(folder) ){
        stop( 'Both folder and file.list have been omitted. Please pass one or the other.' )
    } else {
        file.list = list.files( folder, pattern = pattern, ignore.case = ignore.case, recursive = recursive )
    }

    if( length( file.list ) == 0 && !is.null( folder ) ){
        warning( glue::glue( 'fldict: No files found at path: [ {folder} ].' ) )
        return(NULL)
    }

    dl = NULL

    for( ifile in file.list ){
        
        # handle excel
        if( grepl( '[.](xls[xmb]?|xml)', ifile, ignore.case = TRUE ) ){
            
            # get sheets.

                filename = filename.helper( ifile, folder )
            
                # attempt standard excel read.
                sheets = tryCatch( 
                    readxl::excel_sheets( filename ),
                    error = function(e) NULL
                )
                
                # convert character "NULL" to NULL 
                if(is.null(sheets) || sheets[1] == "NULL") sheets = NULL
                    
                # attempt HTML/XML saved as XLS read.
                if( is.null(sheets) && grepl( '[.]xls$', ifile, ignore.case = TRUE ) ) sheets = 1:length( XML::readHTMLTable( filename ) )
                
                # convert character "NULL" to NULL 
                if(is.null(sheets) || sheets[1] == "NULL") sheets = NULL
                
                if( is.null(sheets) ) stop( glue::glue( 'Could not find sheets for Excel file at [ {ifile} ].' ) )
                    
            for( isheet in sheets ){
                
            idt = tryCatch({
                
                x = read.any( ifile, folder = folder, sheet = isheet, ... )

                d = dict(x)
                d$rows = nrow(x)
                d$cols = ncol(x)

                d
                
            }, error = function(e) data.frame( err = as.character(e) )
            )
            
            idt$file = ifile
            idt$sheet = isheet
            
            dl = bindf( dl, idt )
            
            rm( isheet, idt )
            if( exists('x') ) rm(x)
            
            }
            
        } else {
            
            idt = tryCatch({
                
                x = read.any( ifile, folder = folder, ... )
                
                d = dict(x)
                d$rows = nrow(x)
                d$cols = ncol(x)
                
                d
            
            }, error = function(e) data.frame( err = as.character(e) )
            )
            
            idt$file = ifile
            
            dl = bindf( dl, idt )
            
            rm( idt )
            if( exists('x') ) rm(x)
            
        }
        
        rm(ifile)
    
    }
    
    for( icol in intersect( c( 'rows', 'cols' ), colnames(dl) ) ) dl[[icol]] = fmat( dl[[icol]] )

    dl = dl[ , unique( c(
        intersect( c( 'file', 'sheet', 'err', 'rows', 'cols' ), colnames(dl) ),
        colnames(dl)
    )) ]
    
    return(list(
        sheets = distinct( dl[ , intersect( c( 'file', 'sheet', 'err', 'rows', 'cols' ), colnames(dl) ) ] ),
        columns = dl
    ))
    
}

