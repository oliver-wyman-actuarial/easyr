#' Join and Replace Values.
#' 
#' Replace a columns values with matches in a different dataset.
#' Author: Bryce Chamberlain.
#'
#' @param x Main dataset which will have new values. This data set will be returned with new values.
#' @param y Supporting dataset which has the id and new values.
#' @param by Vector of join column names. A character vector if the names match. A named character vector if they don't.
#' @param replace.cols Vector of replacement column names, similar format as by.
#' @param na.only Only replace values that are NA.
#' @param only.rows Select rows to be affected. Default checks all rows.
#' @param verbose Print via cat information about the replacement.
#'
#' @return x with new values.
#' @export
#'
#' @examples
#' 
#' df1 = utils::head( sleep )
#' group.reassign = data.frame( 
#'   id.num = factor( c( 1, 3, 4 ) ), 
#' group.replace = factor( c( 99, 99, 99 ) ) 
#' )
#' 
#' jrepl( 
#'   x = df1, 
#'   y = group.reassign, 
#'   by = c( 'ID' = 'id.num' ), 
#'   replace.cols = c( 'group' = 'group.replace' ) 
#' )
#' 
#' # doesn't affect since there are no NAs in group.
#' jrepl( 
#'   x = df1,
#'   y = group.reassign, 
#'   by = c( 'ID' = 'id.num' ), 
#'   replace.cols = c( 'group' = 'group.replace' ), 
#'   na.only = TRUE  
#' ) 
jrepl = function( x, y, by, replace.cols, na.only = FALSE, only.rows = NULL, verbose = FALSE ){
  
  # Replace y column names with x column names to make things easier.
    
    y.replace = as.character(replace.cols)
    y.join = as.character(by)
    replace.cols = if( is.null( names(replace.cols) ) ){ replace.cols } else { names(replace.cols) }
    join.cols = if( is.null( names( by ) ) ){ by } else { names( by ) }
    rm(by)
  
  # Validation.
    
    y.missing = setdiff( c( y.replace, y.join ), colnames(y) )
    if( length(y.missing) > 0 ) stop( 'jrepl error: Necessary columns were not found in y dataset: [', cc( y.missing, sep = ',' ), '].' )

    x.missing = setdiff( join.cols, colnames(x) )
    if( length(x.missing) > 0 ) stop( 'jrepl error: Necessary columns were not found in x dataset: [', cc( x.missing, sep = ',' ), '].' )

    rm( y.missing, x.missing )

  # add any columns to x that aren't there. we'll need this later to slot values into.
  for( i in 1:length(replace.cols) ) if( replace.cols[[i]] %ni% colnames(x) ){
    
      x[[ replace.cols[[i]] ]] <- NA
    
      if( class( y[[ y.replace[[i]] ]] ) == 'factor' ){
      
        x[[ replace.cols[[i]] ]] = factor(x[[ replace.cols[[i]] ]])
      
      } else {
      
        class( x[[ replace.cols[[i]] ]] ) <- class( y[[ y.replace[[i]] ]] )
      
      }
    
    }

  # Save info about the original columns.
  
  old.classes = if( ncol( x ) == 1 ){ firstClass( x ) } else { sapply( x, firstClass ) }
  
  # Ensure matching columns have the same types.
  
  xcols = c( join.cols, replace.cols )
  ycols = c( y.join, y.replace )
  
  for( i in 1:length(xcols) ){
    
    xclass = firstClass( x[[ xcols[i] ]] )
    yclass =  firstClass( y[[ ycols[i] ]] )
    
    if( xclass != yclass ){
      
      # special case that one is an int and the other is numeric, we can convert both to numeric without losing information.
      if( all( c( xclass, yclass ) %in% c( 'integer', 'numeric' ) ) ){
        
        x[[ xcols[i] ]] = as.numeric( x[[ xcols[i] ]] )
        y[[ ycols[i] ]] = as.numeric( y[[ ycols[i] ]] )
        
        # special case that one is a character and the other is a factor.
      } else if( all( c( xclass, yclass ) %in% c( 'factor', 'character' ) ) ){
        
        x[[ xcols[i] ]] = as.factor( x[[ xcols[i] ]] )
        y[[ ycols[i] ]] = as.factor( y[[ ycols[i] ]] )
        
        # otherwise, return an error since class handling can be problematic.
      } else{
        
        stop(glue::glue('
            Classes for [ x.{ xcols[i] }, y.{ ycols[i] } ] are mismatched [ {xclass}, {yclass } ]. 
            Please fix the data such that classes match before calling jrepl. 
            Error E-132 owactools::jrepl.
          '))
        
      }
      
    }
    
    rm( i, xclass, yclass )
    
  }
  
  rm( xcols, ycols )
  
  # create a copy of x. this is what we'll be returning eventually, once we add new values.
  # we'll do work on x, then instert that work back into x.copy and return it.
  x.copy = x

  # add original x row so we can re-input values back into x.
  # then filter it down to only.rows.
  x$orig.x.row = 1:nrow(x)
  if( ! is.null(only.rows) ) x = x[ only.rows, ]
  
  # Set consistent names.
    
    y = y[ , c( y.join, y.replace ) ]
    x = x[ , c( 'orig.x.row', join.cols, replace.cols ) ]
    rm( y.replace, y.join )
    
    join.cols.clean = paste0( 'join.col', 1:length(join.cols) )
    replace.cols.clean = paste0( 'replace.col', 1:length(replace.cols) )
    join.replace.cols = c( join.cols.clean, replace.cols.clean )
    names(y) = join.replace.cols
    names(x) = c( 'orig.x.row', join.replace.cols )
  
  # Now we have the same column names and both datasets limited only to our joining and replacing columns.
  # When we join we'll get .x and .y names.
    
  # Join to combine the data fast.
  
    # Perform the join. Inner join is faster, we'll use row numbers to set values back into the original x.
    x = ijoinf( x, y, by = join.cols.clean )
    
    # Check for duplication.
    urows = unique( x$orig.x.row )
    if( nrow(x) != length(urows) || any( x$orig.x.row != urows ) ) stop( 
      'jrepl error: rows were duplicated or elimitated in the join. Please fix the data such that the join does not create duplicates. Error E510 jrepl.' 
    )
    rm(urows)
    
  # Now handle each replace columns.
    
    num.replaced = c()
    
    for( i in 1:length(replace.cols.clean) ){
      
      icolname = replace.cols.clean[i]
      icolname.orig = replace.cols[i]
      xcolname = cc( icolname, '.x' )
      ycolname = cc( icolname, '.y' )
    
      # Replace values using ifelse.
      do.rows = if( na.only ){
        
        x[ which( is.na( x[[xcolname]] ) ), c( 'orig.x.row', xcolname, ycolname ) ]

      } else { x[ , c( 'orig.x.row', xcolname, ycolname ) ] }

      if( verbose ) num.replaced = c( num.replaced, sum( !is.na( do.rows[[ ycolname ]] ) ) )
      #num.changed = c( num.replaced, sum( !is.na( do.rows[[ xcolname ]] ) & !is.na( do.rows[[ ycolname ]] ) * do.rows[[ xcolname ]] != do.rows[[ ycolname ]] ) )
    
      # Set the new values into original x.
      
        # for factors, we need to add new levels.
          
          if( is.factor( x.copy[[ icolname.orig ]] ) ){
            ylevels = levels( do.rows[[ycolname]] )
            xlevels = levels( x.copy[[ icolname.orig ]] )
            levels( x.copy[[ icolname.orig ]] ) <- c( xlevels, setdiff( ylevels, xlevels ) )
          }
      
        x.copy[[ icolname.orig ]][ do.rows$orig.x.row ] <- do.rows[[ ycolname ]]
        
        if( is.factor( x.copy[[ icolname.orig ]] ) ) x.copy[[ icolname.orig ]] <- droplevels( x.copy[[ icolname.orig ]] )


    rm( i, icolname, icolname.orig, xcolname, ycolname, do.rows )
      
  }
  
  # Check for change in classes.
    
    new.classes = if( nrow( x.copy ) == 1 ){ firstClass( x.copy ) } else { sapply( x.copy, firstClass ) }
    
    if( nrow( x.copy ) > 0 ){
      diff.classes = which( new.classes != old.classes )
      if( length( diff.classes ) > 0 ) warning(
        'jrepl warning: [', cc( colnames(x.copy)[ diff.classes], sep = ',' ), '] type changed from [', cc( old.classes[diff.classes], sep = ',' ), 
        '] to [', cc( new.classes[diff.classes], sep = ',' ), ']. ',
        'To avoid this, ensure both x and y replace.cols columns are the same type.'
      )
    }
    
  # show % replaced.
  if( verbose ){
    for( i in 1:length(replace.cols.clean) ) cat( 'jrepl: [', replace.cols[i], '] values replaced:', round( num.replaced[i] / nrow(x.copy), 2 ) * 100, '% \n ' )
    cat( '\n' )
    rm(i)
  }
    
  # Return the modified data.
  return( x.copy )
  
}

# utilities.
firstClass = function(x) gsub( 'ordered', 'factor', class(x)[1] )
