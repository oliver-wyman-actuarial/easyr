setother = function( x, maxbuckets = 6, size = NULL, otherval = 'Other' ){
    
    # summarize the size of each unique value of x.

        dt = data.frame(
            size = if( is.null(size) ){ 1 } else { size },
            x = x,
            stringsAsFactors = FALSE
        )

        buckets = dplyr::summarize( dplyr::group_by( dt, x ), size = sum( size ) )

    # if there is already fewer than maxbuckets, we are done.    
    if( nrow(buckets) <= maxbuckets ) return(x)

    # otherwise, we need to replace values to otherval. If x is a factor, we must add otherval as a level.
    if( is.factor(x) && otherval %ni% levels(x) ) levels(x) = c( levels(x), otherval )
    
    # replace anything not in the top maxbuckets - 1 with otherval, giving us maxbuckets total values.
    x[ ! x %in% utils::head( dplyr::arrange( buckets, dplyr::desc( size ) ), maxbuckets - 1 )$x ] <- otherval

    return(x)

}

otherlast = function( x, col, otherval = 'Other' ){

    # if col was passed as a column index, replace it with the column.
    if( 
        length( col ) == 1 && 
        ( is.numeric(col) || col %in% names(x) ) 
    ) col = x[[ col ]]

    others = which( !is.na(col) & col == otherval )
    if( length( others ) == 0 ) return(x)

    return( dplyr::bind_rows( 
        x[ -others, ],
        x[ others, ]
    ))

}