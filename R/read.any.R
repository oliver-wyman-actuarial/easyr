#' Read Any File
#'
#' Flexible read function to handle many types of files. Currently handles CSV, TSV, DBF, RDS, XLS (incl. when formatted as HTML), and XLSX.
#' Also handles common issues like strings being read in as factors (strings are NOT read in as factors by this function, you'd need to convert them later). 
#' Author: Bryce Chamberlain. Tech Review: Dominic Dillingham.
#'
#' @param filename File path and name for the file to be read in.
#' @param folder Folder path to look for the file in.
#' @param sheet The sheet to read in.
#' @param file_type Specify the file type (CSV, TSV, DBF). If not provided, R will use the file extension to determine the file type. Useful when the file extension doesn't indicate the file type, like .rpt, etc.

#' @param first_column_name Define headers location by providing the name of the left-most column. Alternatively, you can choose the row via the [headers_on_row] argument.
#' @param header Choose if your file contains headers.
#' @param headers_on_row Choose a specific row number to use as headers. Use this when you want to tell read.any exactly where the headers are.
#' @param nrows Number of rows to read. Leave blank/NA to read all rows. This only speeds up file reads (CSV, XLSX, etc.), not compressed data that must be read all at once. This is applied BEFORe headers_on_row or first_column_name removes top rows, so it should be greater than those values if headers aren't in the first row.
#' 
#' @param row.names.column Specify the column (by character name) to use for row names. This drops the columns and lets rows be referenced directly with this id. Must be unique values.
#' @param row.names.remove If you move a column to row names, it is removed from the data by default. If you'd like to keep it, set this to FALSE.
#' 
#' @param make.names Apply make.names function to make column names R-friendly (replaces non-characters with ., starting numbers with x, etc.)
#' @param field_name_map Rename fields for consistency. Provide as a named vector where the names are the file's names and the vector values are the output names desired. See examples for how to create this input.
#' @param require_columns List of required columns to check for. Calls stop() with helpful message if any aren't found.
#' 
#' @param all_chars Keep all column types as characters. This makes using bind_rows easer, then you can use atype() later to set types.
#' @param auto_convert_dates Identify date fields and automatically convert them to dates
#' @param allow_times imes are not allowed in reading data in to facilitate easy binding. If you need times though, set this to TRUE.
#' @param check_numbers Identfy numbers formatted as characters and convert them as such.
#' @param nazero Convert NAs in numeric columns to 0.
#' @param check_logical Identfy logical columns formatted as characters (Yes/No, etc) or numbers (0,1) and convert them as such.
#' @param stringsAsFactors Convert characters to factors to increase processing speed and reduce file size.
#' 
#' @param na_strings Strings to treat like NA. By default we use the easyr NA strings.
#' @param na_level dplyr doesn't like factors to have NAs so we replace NAs with this value for factors only. Set NULL to skip.
#' @param ignore_rows_with_na_at Vector or value, numeric or character, identifying column(s) that require a value. read.any will remove these rows after colname swaps and read, before type conversion. Especially helpful for removing things like page numbers at the bottom of an excel report that break type discovery. Suggest using the claim number column here.
#' @param drop.na.cols Drop columns with only NA values.
#' @param drop.na.rows Drop rows with only NA values.
#' @param fix.dup.column.names Adds 'DUPLICATE #' to duplicated column names to avoid issues with multiple columns having the same name.
#' @param do.trim.sheetname read.any will trim sheet names to get better matches. This will cause an error if the actual sheet name has spaces on the left or right side. Disable this functionality here.
#'
#' @param x If you want to use read.any functionality on an existing data frame, pass it with this argument.
#' @param isexcel If you want to use read.any functionality on an existing data frame, you can tell read.any that this data came from excel using isexcel manually. This comes in handy when excel-integer date conversions are necessary.
#' @param encoding Encoding passed to fread and read.csv.
#' @param verbose Print helpful information via cat.
#'
#' @return Data frame with the data that was read.
#' @export
#'
#' @examples
#' \donttest{
#' read.any( 'path/to/file.extension' )
#' 
#' # if dates are being converted incorrectly, disable date conversion:
#' read.any( 'path/to/file.extension', auto_convert_dates = FALSE )
#' 
#' # to handle type conversions manually:
#' read.any( 'path/to/file.extension', all_chars = TRUE )
#' }
read.any <- function(
  
    filename = NA, 
    folder = NA,
    sheet = 1, 
    file_type = '', 
    
    first_column_name = NA, 
    header = TRUE, 
    headers_on_row = NA,  
    nrows = -1L,
    
    row.names.column = NA,
    row.names.remove = TRUE,
    
    make.names = FALSE, 
    field_name_map = NA, 
    require_columns = NA,
    
    all_chars = FALSE,
    auto_convert_dates = TRUE,
    allow_times = FALSE,
    check_numbers = TRUE,
    nazero = FALSE,
    check_logical = TRUE,
    stringsAsFactors = FALSE,
    
    na_strings = easyr::nastrings, 
    na_level = '(Missing)',
    ignore_rows_with_na_at = NA, 
    drop.na.cols = TRUE,
    drop.na.rows = TRUE,
    fix.dup.column.names = TRUE,
    do.trim.sheetname = TRUE,
    
    x = NULL, 
    isexcel = FALSE,
    encoding = 'unknown',
    verbose = TRUE
    
  ){

  # Argument validation.
  if( is.na(filename) && is.null(x) ) stop( 'You must pass an argument for either [filename] or [data].' )
  
  # Corrections to inputs.
  
    # Folder.
    # filename.helper is in utils.R
    filename = filename.helper( filename, folder )

    # Headers on row.
    if( easyr::isval( headers_on_row ) ){
      headers_on_row = tryCatch({
        easyr::tonum( headers_on_row, ifna = "error" ) # Convert to numeric in case a string is passed. This must be numeric, so an error here during conversion is OK.
      }, error = function(e) stop( 'Invalid [headers_on_row] argument or [headers] argument used instead of [header].' )
      )
      first_column_name = NA # if we have headers_on_row, first_column_name will be ignored since the headers row is explicitly provided. first_column_name is only used to find the headers.
    }
    if( 
      is.na(headers_on_row) && 
      is.na(first_column_name) && 
      is.na(field_name_map) &&
      header
    ) headers_on_row = 1
    
    # Trim/lower operations.
    file_type = stringr::str_trim( tolower(file_type) )
    if( do.trim.sheetname ) sheet = stringr::str_trim(sheet) # Need to keep case here so it matches the workbook.
    if( !easyr::isval(sheet) ) sheet = 1 # Default to first sheet in case a null sheet is passed into the function. This should happen very rarely.
    sheet = easyr::tonum( sheet, verbose = FALSE, ifna = 'return-unchanged' ) # if the sheet is a number formatted as a character, convert it to numeric. Thus must come AFTER stringr::str_trim since that converts to string.
    if( is.numeric(sheet) && sheet < 1 ) stop( glue::glue( 'Sheet [{sheet}] cannot be less than one.' ) )

  # If we don't already have data, read in the file with the appropriate function, and headers_on_row if applicable.
  if( is.null(x) ){
    
    # Validate that file exists.
    if( !file.exists(filename) ) stop( glue::glue(
        if( isval( folder ) ){
          'easyr::read.any > 
              File [{filename}] 
              not found in the folder [{folder}]. 
              Please double-check the file name and folder.
          '
        } else {
          'easyr::read.any > 
              File [{filename}] 
              not found in the current workspace [{getwd()}]. 
              Please double-check the file name and current workspace.
          '
        }
      ))

    if( file_type == 'rds' || grepl( '[.]rds$', filename, ignore.case = T ) ) x <- readRDS(filename)
    if( file_type == 'dbf' || grepl( '[.]dbf$', filename, ignore.case = TRUE ) ) x <- foreign::read.dbf(filename) # dbf, requires package: foreign.
    
    if( file_type == 'csv' || grepl( '[.]csv$', filename, ignore.case = T ) ){
      tryCatch({
        x <- data.table::fread( # preferred, fastest but sometimes errors out where read.csv works. We use try catch to pass errors/warnings to read.csv which is a bit slow but less error-prone.
          filename, sep = ',',
          header = FALSE, # we'll implement this later.
          stringsAsFactors = FALSE,
          encoding = encoding,
          nrows = nrows
        )
      },
      error = function(e) x <<- utils::read.csv( filename, stringsAsFactors = FALSE, check.names = FALSE, nrows = nrows, header = FALSE, encoding = encoding ),
      warning = function(w) warning( 'Warning during read of [', filename, '], from data.table::fread \n ', w )
      )
    }
    
    if( file_type == 'tsv' || grepl( '[.]tsv$', filename, ignore.case = TRUE) ) x <- data.table::fread( 
      file = filename, sep = '\t', stringsAsFactors = FALSE, nrows = nrows, header = FALSE, encoding = encoding
    )
    
    # For excel, use our custom rx function (in this file, see below). rx is not exported to prevent it's being used instead of read.any.
    isexcel = grepl( '[.]xls[xm]?$', filename, ignore.case = TRUE )
    if( isexcel ) x <- rx(
      filename = filename, sheet = sheet, first_column_name = first_column_name,nrows = nrows, verbose = verbose
    )
  
  }
  
  # Implement headers_on_row if it was passed. This does not apply to data files that always come with headers.
  if( 
    !is.na(headers_on_row) &&
    !grepl( '[.](dbf|rds)$', filename, ignore.case = TRUE ) && 
    ! file_type %in% c('rds','dbf')
  ){
    
    colnames(x) <- rany_fixColNames( 
      as.character( x[ headers_on_row, ] ), 
      fix.dup.column.names = fix.dup.column.names, 
      nastrings = na_strings 
    )
    
    if( ( headers_on_row + 1 ) > nrow(x) ){
      x <- x[ rep(FALSE, nrow(x)), , drop = FALSE ]
    } else {
      drop = which( 1:nrow(x) <= headers_on_row )
      if(length(drop) > 0) x = x[ -drop, , drop = FALSE ]      
    }
      
  }
  
  # Check if the file exists (was read), error if it doesn't.
  if( is.null(x) ){
    
    stop('File type may not be acceptable. read.any currently only reads CSV, TSV, XLS, XLSX, DBF, RDS.')
    
  } else { 
    
    x <- as.data.frame( x, stringsAsFactors = FALSE ) # this might be read in as data.table (by fread), we want output to be consistently data.frame, for now.
    
    # Enforce nrows in case it hasn't been already (DBF, RDS files).
    if( nrows > 0 ) x <- x[ 1:max(nrow(x),nrows), ] 
    
  }

  # Fix column names. This must happen BEFORE looking for first column, others they may not match as expected.
  colnames(x) = rany_fixColNames( 
    colnames(x), 
    fix.dup.column.names = fix.dup.column.names, 
    nastrings = na_strings 
  )
  
  # If we have a first column name or field name map, use it to find the row with column names.
  if( is.na(headers_on_row) && ( easyr::isval( first_column_name ) || easyr::isval( field_name_map ) ) ){
  
    # Attempt to get the headers row.
    iheaders_row =  headers_row( x, first_column_name = first_column_name, field_name_map = field_name_map ) # function is in this file, see below.

    # If the headers were found.
    if( !iheaders_row$headers_already_column_names && easyr::isval( iheaders_row$headers_on_row ) ){
        
      # Fix the new column names.
      x[ iheaders_row$headers_on_row, ]  <- rany_fixColNames( x[ iheaders_row$headers_on_row, ], fix.dup.column.names = fix.dup.column.names, nastrings = na_strings )

      # Set the names.    
      colnames(x) <-  x[ iheaders_row$headers_on_row, ]

      # Remove rows that are before or equal to the row with column names.
      if( ( iheaders_row$headers_on_row + 1 ) > nrow(x) ){
        x <- x[ rep(FALSE,nrow(x)), ]
      } else {
        x <- x[ ( iheaders_row$headers_on_row + 1 ):nrow(x), ] 
      }

    # If first_column_name was passed but we didn't find it, return an error.
    } else if( !iheaders_row$headers_already_column_names && !easyr::isval( iheaders_row$headers_on_row ) ) {
      stop('ERROR: Supplied [first_column_name] was not found in the first 1,000 or fewer rows: [',first_column_name,'] in file [',filename,'] sheet [',sheet,'].')
    }
    
  }
  
  if( make.names ) colnames(x) <- make.names( colnames(x) ) # Apply make names if the user has selected it.
  
  # Apply field name map if one was passed. This will rename the field names.
  if( easyr::isval( field_name_map ) ){ 

    # Fix the map.
    
      # str_trim drops names so we need to save them and add them back.
      field_name_map.names = names(field_name_map)
      field_name_map <- stringr::str_trim( field_name_map )
      names(field_name_map) = field_name_map.names
      rm(field_name_map.names)
      field_name_map <- field_name_map[ !is.na( field_name_map ) & !easyr::eq( field_name_map,'') ] # remove any NAs and empty strings from the field map.
    
    # apply it to each column.
    if( is.null( names( field_name_map ) ) ) stop( 'easyr::read.any [field_name_map] argument must be a named vector.')
    for( icolname in colnames(x) ) if( any( easyr::eq( names( field_name_map ), icolname ) ) ){ # is there a mapped value for this column?
    #for( icolname in colnames(x) ) if( !is.na( icolname ) & icolname %in% names( field_name_map ) ) ) ){ # is there a mapped value for this column?
      colnames(x)[ colnames(x) == icolname ] <- field_name_map[ eq( names( field_name_map ), icolname ) ][1] # apply the rename.
    } 

  }
  
  # Handle character columns (this will be all the columns).
  if( nrow(x) > 0 ) for(i in colnames(x) ) if( is.character(x[[i]]) ){
    
    # Trim white space.
    x[[i]] <- stringr::str_trim( x[[i]] )
    
    #  Implement na strings. This is done here and nowhere else.
    x[ x[[i]] %in% na_strings, i ] <- NA
    
  }
  
  # Remove NAs at ignore_rows_with_na_at.
  if( !is.na( ignore_rows_with_na_at ) ){
    
    missing.cols = ignore_rows_with_na_at[ ! ignore_rows_with_na_at %in% colnames(x) ]
    if( length( missing.cols ) > 0 ) stop(
      'ignore_rows_with_na_at columns [', cc( missing.cols, sep = ', ' ), '] not found in data.'
    )
    rm(missing.cols)
    
    for( i in ignore_rows_with_na_at ) x <- x[ !is.na(x[[i]]), ]
    
  }
  
  # Auto-convert dates and numbers.
  if( ! all_chars ){
    
    x = atype( x,
      auto_convert_dates = auto_convert_dates, 
      check_numbers = check_numbers, 
      nazero = nazero,
      check_logical = check_logical,
      allow_times = allow_times, isexcel = isexcel, stringsAsFactors = stringsAsFactors 
    )
    
  } else {
    for( i in colnames(x) ) if( ! is.character( x[[i]] ) ) x[[i]] <- as.character(x[[i]])
  }
  
  # Drop all-NA columns.
  if( drop.na.cols && nrow(x) > 0 ) x = x[ , colnames(x)[
    
    # Use sapply to find non-NA columns.
    sapply( x, function(column) any( !is.na( column ) ) )
    
    # Don't drop anything specified in the column name map.
    | colnames(x) %in% c( field_name_map, names( field_name_map ) )
    
    ], drop = FALSE]
  
  # Drop all-NA rows.
  if( drop.na.rows && nrow(x) > 0 ){
    
    row.check = matrix( ncol = ncol(x), nrow = nrow(x) )
    row.check[ !is.na(x) ] <- 1
    row.check[ is.na(x) ] <- 0
    
    x = x[ which(rowSums(row.check) > 0), , drop = FALSE ]
    
    rm(row.check)
    
  }
  
  # Set row names.
  did.row.names = FALSE
  if( !is.na( row.names.column ) ){
    
    # Validated.
    if( class(row.names.column) != 'character' || length( row.names.column ) != 1 ) stop( '[row.names] argument must be a single character.')
    if( any( duplicated( x[[ row.names.column ]] ) ) ) stop( '[row.names] column must have unique values.' )
    
    # Apply rownames and remove the column.
    rownames(x) <- x[[ row.names.column ]]
    if( row.names.remove ) x = x[, setdiff( colnames( x ), row.names.column ), drop = FALSE ]
    did.row.names = TRUE
    
  }
  
  # Check for required columns.
  if( !nanull(require_columns) ){
    
    missing.cols = setdiff( require_columns, colnames(x) )
    
    if( length( missing.cols) > 0 ) stop(
      'Columns [', paste( missing.cols, collapse = ', ' ), '] not found in result of read.any for file [', filename, '].',
      'Check the field_name_map argument (if passed) and source file to make sure the column is there and set up properly. ',
      'read.any sometimes removes columns, for example if all values in it are NA. This behaviour can be disabled by passing various arguments.'
    )
    
  }

  # Replace NA factors.
  x = factorna( x = x, na_level = na_level )

  return(x)
  
}

#' Read Excel
#' 
#' This gets a bit complex since many errors can occur when reading in excel files. We've done our best to handle common ones.
#' Requires packages: openxlsx, readxl, XML (these are required by easyr).
#' It should NOT be used directly (that's why it isn't exported), but will be called by function [read.any] as necessary, with the applicable defaults set by that function.
#'
#' @param filename File path and name for the file to be read in.
#' @param sheet The sheet to read in.
#' @param first_column_name Pass a column name to help the function find the header row.
#' @param nrows Number of rows to read in.
#' @param verbose Print helpful messages via cat().
#'
#' @return Data object
rx <- function( filename, sheet, first_column_name, nrows, verbose ){
  
  # Setup.
  x = NULL # this is here for the function to latch onto during <<-. Without it, you sometimes get a leftover "x" after running the read.

  # Handle xlsx
  if( grepl( '[.]xlsx$', filename, ignore.case = T ) ) {
      
    # Read in as text, we'll convert datatypes later. Data type conversion in a read-excel function often runs into errors.
    # We'd like more control over data conversion to enhance it.
    x <- owtreadexcel( filename = filename, sheet = sheet, nrows = nrows )
    
  # Handle non-xlsx (xls).
  # Try our preferred read function.
  } else { tryCatch({
    
    x <- suppressMessages( owtreadexcel( filename = filename, sheet = sheet, nrows = nrows ) )
    
    # If an error occurs:
  },error=function(e){
    
    # Sometimes you'll get an .xls files which is actually an HTML/XML file with file extentions xls. See test file [ESIS as of 3-1-2011 2.XLS]. Try to read this.
    if( grepl( '[.]xls$', filename, ignore.case = TRUE ) ){
      
      # This method requires sheet index number, not a sheet name, so return an error if applicable.
      if( !is.numeric(sheet) ) stop('ERROR: Not able to file [',filename,'] sheet [',sheet,']. Try using a sheet index/number instead of the name for argument [sheet], which will allow an XML-based read attempt.')
      
      # Attemp the html-format read via XML.
      if( verbose ) cat( 'Reading as XML/HTML \n' )
      x <<- XML::readHTMLTable( 
        filename, stringsAsFactors = F,
        header = FALSE # we'll find headers later.
      )[[sheet]] # this is in a different environment (error function) so we need to use.
      
    } else { stop(e) } # If we can't take steps to fix the error, just pass it through.
    
  })}

  # If for whatever reason we still don't have a file read (x), return an error. Otherwise return the data.
  if( easyr::isval(x) ){ 
    return(x)
  } else {
    stop('ERROR: Not able to read [',filename,'] sheet [',sheet,'].') # If we made it here, the file wasn't returned/read.
  }
  
}

#' Identify headers row.
#' 
#' Identify the row with headers in a data frame.
#' It should NOT be used directly (that's why it isn't exported), but will be called by function [read.any] as necessary, with the applicable defaults set by that function.
#'
#' @param x Data frame to work with.
#' @param headers_on_row The specific row with headers on it.
#' @param first_column_name A known column(s) that can be used to find the header row. This is more flexible, but only used if headers_on_row is not available. If multiple are possible, use a vector argument here.
#' @param field_name_map field_name_map from read.any.
#'
#' @return List with headers_already_column_names (TRUE/FALSE); headers_on_row (1-indexed number of the to match standard R indexing).
headers_row <- function( x, headers_on_row = NA, first_column_name = NA, field_name_map = NA ){

  # Setup.
  headers_already_column_names = FALSE
  first_column_names = c()
  
  # If we already have it, just send it back. This is to dynamically only run the function as needed.
  
    if( !is.na( headers_on_row ) )  return( list(
      headers_already_column_names = headers_already_column_names,
      headers_on_row = headers_on_row
    ))
  
  # Get list of possible column name.
    
    if( isval( first_column_name ) ){
      isplitnames = strsplit( first_column_name, ';' )[[1]]
      first_column_names = c( first_column_name, isplitnames )
    }
    
    if( isval( field_name_map ) ){
      first_column_names = c( first_column_names, names(field_name_map) )
    }
    
    first_column_names = unique( c( make.names(first_column_names), first_column_names ) ) # also check make.names results since read functions will sometimes use those names.

  # Sometimes the read function will find names on it's own. If this is the case, no need to continue this block.
  headers_already_column_names = any( first_column_names %in% colnames(x) )

  # If the headers aren't already in the column names. Search for the row with the headers.
  if( !headers_already_column_names ) for(i in 1:min(1000,nrow(x))){ # Check at most the first 1k rows.

    if( any( !is.na(x[i,] ) & ( stringr::str_trim( gsub( '\\n|\\r', '', x[i,] ) ) %in% first_column_names ) ) ){ # Check the row to see if any of its' values match the header given.

      headers_on_row = i
      break # done.

  }}

  # Return the data.
  return( list(
    headers_already_column_names = headers_already_column_names,
    headers_on_row = headers_on_row
  ))

}

#' Fix column names.
#' 
#' Code to fix column names, since this has to be done up to twice will reading in files.
#' It should NOT be used directly (that's why it isn't exported), but will be called by function [read.any] as necessary, with the applicable defaults set by that function.
#' 
#' @param col_names Vector/value of colum names/name.
#' @param fix.dup.column.names Adds 'DUPLICATE #' to duplicated column names to avoid errors with duplicate names.
#' @param nastrings Characters/strings to read as NA.
#'
#' @return Fixed names.
rany_fixColNames <- function( col_names, fix.dup.column.names, nastrings ){

    # Replace NA column names - these can cause errors downstream.
    if( any(is.na(col_names)) ) col_names[is.na(col_names)] <- 'NA'

    # Remove white space.
    col_names <- stringr::str_trim(col_names)
    
    # Remove new line characters and any other strange characters that have come throgh files in the past.
    # Replace them with a space and then resolve any double spaces.
    col_names <- gsub( ' +', ' ', gsub( '_x000D_|\\n|\\r', ' ', as.character( col_names ) ) )
    
    # Set NA column names to 'NA' as a string.
    col_names[ col_names %in% nastrings ] = 'NA'
    
    # Fix duplicate columns.
    if( fix.dup.column.names ) col_names = sapply( 1:length(col_names), function(i){
      name.count = sum( col_names[1:i] == col_names[i] )
      cc( col_names[i], ifelse( name.count == 1, '', cc( ' DUPLICATE ', (name.count - 1) ) ) )
    })

    return( enc2native(col_names) ) # Return the fixed names.
}

# special read excel function.
owtreadexcel = function( filename, sheet, nrows ) return( tryCatch({
      
      # new R versions.
      readxl::read_excel(
        filename, 
        sheet = sheet, 
        n_max = if(nrows>0){ nrows } else { Inf }, 
        col_names = FALSE, # we'll find headers later.
        col_types = 'text',
        .name_repair = 'minimal'
      )

    }, error = function(e){
      
      if( !grepl( '[.]name_repair', e ) ) stop(e)

      # old R versions.
      readxl::read_excel( 
        filename, 
        sheet = sheet, 
        n_max = if(nrows>0){ nrows } else { Inf }, 
        col_names = FALSE, # we'll find headers later.
        col_types = 'text'
      )

    })
  
)