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

edistinct = function(x) x[!duplicated(x), ]
emerge = function(x, y, by, type = c('inner', 'full', 'left')){
  
  type = match.arg(type)
  if(is.null(names(by))) names(by) = by

  # merge likes to change the order of things so let's add a sort column.
  if('xrow' %ni% names(x)){
  x$xrow = 1:nrow(x)
  } else {
    warning('easyr: xrow found in data. output will be sorted by this column.')
  }
  if('yrow' %ni% names(y)){
  y$yrow = 1:nrow(y)
  } else {
    warning('easyr: yrow found in data. output will be sorted by this column.')
  }

  # perform the merge.
  x = merge(x = x, y = y, by.x = names(by), by.y = by, all.x = type %in% c('left', 'full'), all.y = type %in% c('full'))

  # return with correct order.
  x = x[
    order(x$xrow, x$yrow), 
    setdiff(colnames(x), c('xrow', 'yrow'))
  ]
  return(x)
  
}

# determine and set table type.
settabletype = function(x, inclass){

  if( any( inclass == class(data.table::data.table())[1] ) ) return( data.table::as.data.table(x) )
  if( any( inclass == class(dplyr::tibble())[1] ) ) return( dplyr::as_tibble(x) )

  # preserve data format with stringsAsFactors = FALSE
  if( any( inclass == class(data.frame())[1] ) ) return(as.data.frame(x, stringsAsFactors = FALSE)) 

  return(x)
  
}