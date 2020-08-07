#' Validate Equal
#' 
#' Check various properties of 2 data frames to ensure they are equivalent.
#'
#' @param df1 First data frame  to compare.
#' @param df2 Second data frame to compare.
#' @param id.column If available, a column to use as an ID. Helpful in various checks and output.
#' @param regex.remove Pattern to remove from strings. Used in gsub to remove characters we don't want to consider when comparing values. Set to NULL, NA, or "" to leave strings unchanged.
#' @param do.set.NA Remove NA strings.
#' @param nastrings Strings to consider NA.
#' @param match.round.to.digits Round numbers to these digits before checking equality.
#' @param do.all.columns.before.err Check all columns before returning an error. Takes longer but returns more detail. If FALSE, stops at first column that doesn't match and returns mismatches.
#' @param check.column.order Enforce same column order.
#' @param sort.by.id Sort by the id column before making comparisons.
#' @param acceptable.pct.rows.diff If you are OK with differences in a few rows, set this value. If fewer rows in a column don't match, the function will consider the columns equivalent. Iterpreted as a percentage (it gets divided by 100).
#' @param acceptable.pct.vals.diff If you are OK with small differences in values, set this value. If the difference in numeric values is less, the function will consider the values equivalent. Iterpreted as a percentage (it gets divided by 100) and compared to absolute value of percentage difference.
#' @param return.summary Return 2 items in a list, the row mismatches and a summary of row mismatches.
#' @param verbose Print helpful information via cat(). 
#'
#' @return May return information about mismatches. Otherwise doesn't return anything (NULL).
#' @export
#'
#' @examples
#' validate.equal( iris, iris )
#' 
validate.equal = function( 
  df1,
  df2,
  id.column = NULL,
  regex.remove = '[^A-z0-9.+\\/,-]',
  do.set.NA = TRUE,
  nastrings = easyr::nastrings,
  match.round.to.digits = 4,
  do.all.columns.before.err = FALSE,
  check.column.order = FALSE,
  sort.by.id = TRUE,
  acceptable.pct.rows.diff = 0,
  acceptable.pct.vals.diff = 0,
  return.summary = FALSE,
  verbose = TRUE
){
  
  # get the names of the data frames that were passed.
  call.parts = unlist( stringr::str_extract_all( as.character( sys.call() ), '[^,()]+') )
  call.parts = gsub( 'df[12] ?=', '', call.parts )
  call.parts = trimws( gsub( 'easyr::validate.equal|validate.equal', '', call.parts ) )
  call.parts = call.parts[ call.parts != '' ]
  
  if( length(call.parts) >= 2 ){
    df1.name = call.parts[1]
    df2.name = call.parts[2]
  } else {
    df1.name = 'df1'
    df2.name = 'df2'
  }
  
  # validate data frames.
  if( null( ncol(df1) ) || null( ncol(df2) ) ) stop( 'validate.equal only supports comparison of data frames and tibbles.' )
  if( nrow(df1) == 0 ) stop( glue::glue( '[{df1.name}] has 0 rows. Error E1127-1 easyr::validate-equal.'))
  if( nrow(df2) == 0 ) stop( glue::glue( '[{df2.name}] has 0 rows. Error E1127-2 easyr::validate-equal.'))
  
  # check column/row counts.
  
  if( nrow(df1) != nrow(df2) ) stop( glue::glue( 'Row counts not equal. {df1.name} has {nrow(df1)} rows vs. {df2.name} with {nrow(df2)}.' ) )
  
  if( ncol(df1) != ncol(df2) ){
    
    # differing columns. 
    in1out2 = setdiff( colnames(df1), colnames(df2) )
    in2out1 = setdiff( colnames(df2), colnames(df1) )
    
    stop( glue::glue( cc(
      'Column counts not equal. {df1.name} has {ncol(df1)} cols vs. {df2.name} with {ncol(df2)}. \n' ,
      if( length(in1out2) > 0 ){ 'These columns are in {df1.name} but not in {df2.name}: [ {cc(in1out2,sep=", ")} ]. \n' },
      if( length(in2out1) > 0 ){ 'These columns are in {df2.name} but not in {df1.name}: [ {cc(in2out1,sep=", ")} ]. \n' }
    )))
    
  }
  
  if( verbose ) cat( '\nRow and column counts match. \n' )
  
  # check column names.
  
  if( check.column.order ){
    
    if( any( colnames(df1) != colnames(df2) ) ) stop( 'Column order is not the same or column names are not the same.' )
    
  } else {
    
    notin1 = setdiff( colnames(df2), colnames(df1) )
    if( length(notin1) > 0 ) stop( 'colnames not in', df1.name, ': ', cc( notin1, sep = ', ' ) )
    notin2= setdiff( colnames(df2), colnames(df1) )
    if( length(notin2) > 0 ) stop( 'colnames not in ', df2.name, ': ', cc( notin2, sep = ', ' ) )
    
    rm( notin1, notin2 )
    
  }
  
  if( verbose ) cat( 'Column names match. \n' )
  
  # check column types
  
    col.types = data.frame(
      column = colnames(df1),
      df1.class = sapply( colnames(df1), function(i) as.character( class( df1[[i]] )[1] ) ),
      df2.class = sapply( colnames(df1), function(i) as.character( class( df2[[i]] )[1] ) ),
      stringsAsFactors = FALSE
    )

    # for the sake of comparision we consider ordered factor == factor
    col.types[ col.types == 'ordered' ] <- 'factor'
    
    mismatch = col.types[ col.types$df1.class != col.types$df2.class, ]
    
    if( nrow(mismatch) > 0 ){
      names( mismatch ) = c( 'column', cc( df1.name, '.class' ), cc( df2.name, '.class' ) )
      warning( 'Column types not equal. Returning unequal column types.' )
      return(mismatch)
    }
    
    rm( col.types, mismatch )
    
    if( verbose ) cat( 'Column types match. \n' )
  
  # check id column.
    
    if( !null(id.column) ){
      
      if( id.column %ni% names(df1) ) stop( glue::glue( '[{df1.name}] is missing id.column = [{id.column}]. Error E1057-1 easyr::validate-equal' ))
      if( id.column %ni% names(df2) ) stop( glue::glue( '[{df2.name}] is missing id.column = [{id.column}]. Error E1057-2 easyr::validate-equal' ))
      
      df1.ids = unique( df1[[id.column]] )
      df2.ids = unique( df2[[id.column]] )
      
      notin1 = setdiff( df2.ids, df1.ids )
      notin2 = setdiff( df1.ids, df2.ids )
      
      if( length(notin1) > 0 ) stop( id.column, ' values not in', df1.name, ' include [', cc( utils::head( notin1, 25 ), sep = ', ' ), '].' )
      if( length(notin2) > 0 ) stop( id.column, ' values not in', df2.name, ' include [', cc( utils::head( notin2, 25 ), sep = ', ' ), '].' )
      
      if( verbose ) cat( 'Unique ', id.column, ' values match. \n' )
      
      rm( df1.ids, df2.ids, notin1, notin2 )
      
    }
  
  # check columns values 
    
    # add initial row in case we re-sort.  
    df1$row = 1:nrow(df1)
    df2$row = 1:nrow(df2)
    
    # add row and then sort by id column for better matching.
    if( !null(id.column) && sort.by.id ){
      df1 = df1[order(df1[[id.column]]), ]
      df2 = df2[order(df2[[id.column]]), ]
    }
    
    # function to clean strings removing differenes we aren't concerned about.
    fixvals = function(x){
      
      if( is.character(x) ){
        
        x = trimws(x)
        
        if( !null(regex.remove) && regex.remove != '' ) x = gsub( regex.remove, '', x )
        if( do.set.NA ) x[ x %in% nastrings ] <- NA
        
      } else if( is.numeric(x) ){
        
        if( isval(match.round.to.digits) ) x = round( x, match.round.to.digits )
        
      } else if( is.ordered(x) ){

        x = factor( x, ordered = FALSE )

      }
      
      return(x)
      
    }
    
    idts = NULL
  
    # check each column.
    for( i in colnames(df1) ){
      
      if( verbose ) cat( '  Checking [', i, ']. \n' )
      
      df1vals = fixvals( df1[[i]] )
      df2vals = fixvals( df2[[i]] )
      
      # acceptable.pct.vals.diff modified?
      if( is.numeric(df1vals) && acceptable.pct.vals.diff > 0 ){

        mismatch = which(
          # use eq to catch NA = NA
          !eq( df1vals, df2vals ) &
          (
            is.na(df1vals) | is.na(df2vals) |
            # check acceptable.pct.vals.diff. round to fix floating-point issues.
            round( abs( df2vals / df1vals - 1 ), 8 ) > acceptable.pct.vals.diff
          )
        )

      # otherwise use eq.
      } else {
        mismatch = which( !eq( df1vals, df2vals ) )
      }
      
      if( length(mismatch) > 0 ){
        
        mismatch.pct = round( length(mismatch) / nrow(df1) * 100, 3 )
        
        if( mismatch.pct <= acceptable.pct.rows.diff ){
          rm( mismatch, i, df2vals, df1vals,  mismatch.pct )
          next
        }
        
        idt = data.frame(
          column = rep( i, length(mismatch) ),
          row1 = df1$row[ mismatch ],
          row2 = df2$row[ mismatch ],
          df1 = as.character(df1vals[mismatch]),
          df2 = as.character(df2vals[mismatch]),
          stringsAsFactors = FALSE
        )
        
        if( is.numeric(df1[[i]]) ){
          idt$abs.pct.diff = round( abs( df2vals[mismatch] / df1vals[mismatch] - 1 ), 1 )
        } else {
          idt$abs.pct.diff = as.numeric(NA)
        }
        
        colnames(idt) = c( 'column', glue::glue( '{df1.name}.row' ), glue::glue( '{df2.name}.row' ), df1.name, df2.name, 'abs.pct.diff' )
        
        if( !null(id.column) ){
          
          df1ids = df1[[id.column]][mismatch]
          df2ids = df2[[id.column]][mismatch]
          
          if( all( df1ids == df2ids ) ){
            
            idt[[ id.column ]] = df1ids
            idt = idt[ , c( 'column', id.column, setdiff( colnames(idt), c( 'column', id.column ) ) ) ]
            
          } else {
            
            ids = c( cc( df1.name, '.', id.column), cc( df2.name, '.', id.column) )
            idt[[ ids[1] ]] = df1ids
            idt[[ ids[2] ]] = df2ids
            idt = idt[ , c( 'column', ids, setdiff( colnames(idt),c( 'column', ids ) ) ) ]
            
          }
          
          rm( df1ids, df2ids )
          
        }
        
        if( do.all.columns.before.err ){
          idts = bindf( idts, idt )
        } else {
          warning( glue::glue( '[{mismatch.pct}%, {length(mismatch)} of {nrow(df1)}] values not equal at [{i}]. Returning first 10 (or less).' ) )
          return( utils::head( idt, 10 ) )
        }
        
        rm( idt, mismatch.pct )
        
      }
      
      rm( mismatch, i, df2vals, df1vals )
      
    }
      
  if( !null(idts) ){
    
    warning( 'Values not equal. Returning mismatches.' )
    
    if( !return.summary ){
      return(idts)
    } else {

        summ = split(idts, factor(idts$column))
        summ = data.table::rbindlist(lapply(summ, function(x){
          if(all(is.na(x$abs.pct.diff))){
            data.frame(
              column = x$column[1], num.rows = nrow(x),
              max.abs.pct.diff = as.numeric(NA), 
              mean.abs.pct.diff = as.numeric(NA)
            )
          } else {
            data.frame(
              column = x$column[1], num.rows = nrow(x),
              max.abs.pct.diff = max(x$abs.pct.diff, na.rm = TRUE), 
              mean.abs.pct.diff = mean(x$abs.pct.diff, na.rm = TRUE)
            )
          }
        }))

        summ$pct.rows = round(summ$num.rows / nrow(df1) * 100, 1)
        idts$column = factor(idts$column)
      
      return( list( rows = idts, summ = summ ) )
    }
    
  }
    
  if( verbose ) cat( 'Column values match. \n' )
  
  if( verbose ) cat( 'All checks passed! Data is equivalent. \n \n' )
  
}
