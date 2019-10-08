# These are minor functions not called when using the package, but used within it.

filename.helper = function( filename, folder ){
  
  if( isval(filename) && isval(folder) ){
    
    # Remove // or \ to prevent double-adding it.
    folder = gsub( '(/|[\\])$', '' , folder ) 
    
    # Add to filename.
    full.path = paste0( folder, '/', filename )
  
    # If the file is not exactly found, use a search.
    # This way you can use pattern names to resist small changes in file name.
    if( !file.exists(full.path) ){
      
      guess.file = list.files( folder )
      guess.file = guess.file[ grepl( filename, guess.file ) ]
      if( length(guess.file) == 1 ) filename = paste0( folder, '/', guess.file )
      
    } else {
      
      filename = full.path
      
    }
    
  }
  
  return(filename)
  
  
}

preprocess.for.type = function( x, nastrings ){  
  
  x = as.character(x)
  x[ x %in% nastrings ] <- NA
  x = tolower( stringr::str_trim(x) )

  return(x)

}

# replace NAs in factors with level na_level (typically "NA").
# this is because the latest dplyr gives warnings if a factors is NA.
factorna = function( x, na_level ){

  if( is.null( na_level ) ) return(x)
    
  for( i in 1:ncol(x) ) if( is.factor( x[[i]] ) ){
    
    navals = which( is.na( x[[i]] ) )

    if( length(navals) > 0 ){

      if( na_level %ni% levels( x[[i]] ) ) levels( x[[i]] ) = c( levels( x[[i]] ), na_level )
      x[[i]][ navals ] <- na_level
      
    }

    rm( navals )

  }

  return(x)

}