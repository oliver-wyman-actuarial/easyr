#' Characters to Factors
#' 
#' Convert all character columns in a data frame to factors.
#' Author: Bryce Chamberlain.
#'
#' @param x Data frame to modify.
#' @param sortlevels Choose whether to sort levels. This is the default R behavior and is therefore likely faster, but it may change the order of the data and this can be problematic so the default is FALSE.
#' @param na_level some functions don't like factors to have NAs so we replace NAs with this value for factors only. Set NULL to skip.
#'
#' @return Data frame with converted factors.
#'
#' @export
#'
#' @examples
#' char2fac( iris )
char2fac = function( x, sortlevels = FALSE, na_level = '(Missing)' ){

    for( icol in colnames(x) ){
        
        if( is.character(x[[icol]]) ){

            # Replace NAs.
            if( !is.null(na_level) ) x[[icol]][ which( is.na( x[[icol]] ) ) ] <- na_level

            # default behavior is to sort levels, so we need to give it levels to prevent sorting.
            if( ! sortlevels ) {

                x[[icol]] <- factor( 
                    x[[icol]], 
                    levels = setdiff( unique( x[[icol]] ), NA ) 
                )

            # default behavior is to sort levels.
            } else { x[[icol]] <- as.factor( x[[icol]] ) }

        }

    }

    return(x)
}

#' Factors to Characters
#' 
#' Convert all factor columns in a data frame to characters.
#' Author: Bryce Chamberlain.
#'
#' @param x Data frame to modify.
#' @return Data frame with converted characters.
#'
#' @export
#'
#' @examples
#' char2fac( iris )
fac2char = function(x){
    for( icol in colnames(x) ) if( is.factor(x[[icol]]) ) x[[icol]] <- as.character( x[[icol]] )
    return(x)
}
