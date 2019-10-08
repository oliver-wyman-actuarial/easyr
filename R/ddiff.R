#' Date difference (or difference in days).
#'
#' @param x Vector of starting dates or items that can be converted to dates by todate.
#' @param y Vector of ending dates or items that can be converted to dates by todate. 
#' @param unit Character indicating what to use as the unit of difference. Values like d, y, m or day, year, month will work. Takes just the first letter in lower-case to determine unit.
#' @param do.date.convert Convert to dates before running the difference. If you know your columns are already dates, setting to FALSE will make your code run faster.
#' @param do.numeric Convert the output to a number instead of a date difference object.
#'
#' @return Vector of differences.
#' @export
#'
#' @examples
#' ddiff( lubridate::mdy( '1/1/2018' ), lubridate::mdy( '3/4/2018' ) )
ddiff = function( x, y, unit = 'day', do.date.convert = TRUE, do.numeric = TRUE ){

    unit = gsub( '[^a-z]', '', trimws( tolower( unit ) ) )

    if( do.date.convert ){
        x = todate(x)
        y = todate(y)
    }
    
    if( substring( unit, 1, 1 ) == 'y' ){
      z = suppressMessages( lubridate::interval( x, y ) %/% lubridate::years(1) )

    } else if( substring( unit, 1, 1 ) == 'm' ){
      z = suppressMessages( lubridate::interval( x, y ) %/% months(1) )

    } else if( substring( unit, 1, 1 ) == 'q' ){
      z = suppressMessages( lubridate::interval( x, y ) %/% base::months(3) )
      
    } else if( substring( unit, 1, 1 ) == '2' ){
      z = suppressMessages( lubridate::interval( x, y ) %/% lubridate::weeks(3) )

    } else if( substring( unit, 1, 1 ) == 'd' ){
      z = y - x
    }

    if( do.numeric ) z = as.numeric(z)
    
    return(z)
    
}

#' Date Difference in Months
#'
#' @param x Vector of starting dates or items that can be converted to dates by todate.
#' @param y Vector of ending dates or items that can be converted to dates by todate.
#' @param do.date.convert Convert to dates before running the difference. If you know your columns are already dates, setting to FALSE will make your code run faster.
#' @param do.numeric Convert the output to a number instead of a date difference object.
#'
#' @return Vector of differences.
#' @export
#'
#' @examples
#' mdiff( lubridate::mdy( '1/1/2018' ), lubridate::mdy( '3/4/2018' ) )
mdiff = function( x, y, do.date.convert = TRUE, do.numeric = TRUE ){
    ddiff( x, y,  do.date.convert =  do.date.convert, do.numeric = do.numeric, unit = 'month' )
}

#' Date Difference in Years
#'
#' @param x Vector of starting dates or items that can be converted to dates by todate.
#' @param y Vector of ending dates or items that can be converted to dates by todate. 
#' @param do.date.convert Convert to dates before running the difference. If you know your columns are already dates, setting to FALSE will make your code run faster.
#' @param do.numeric Convert the output to a number instead of a date difference object.
#'
#' @return Vector of differences.
#' @export
#'
#' @examples
#' ydiff( lubridate::mdy( '1/1/2018' ), lubridate::mdy( '3/4/2018' ) )
ydiff = function( x, y, do.date.convert = TRUE, do.numeric = TRUE ){
    ddiff( x, y,  do.date.convert =  do.date.convert, do.numeric = do.numeric, unit = 'year' )
}

#' Date Difference in Quarters
#'
#' @param x Vector of starting dates or items that can be converted to dates by todate.
#' @param y Vector of ending dates or items that can be converted to dates by todate. 
#' @param do.date.convert Convert to dates before running the difference. If you know your columns are already dates, setting to FALSE will make your code run faster.
#' @param do.numeric Convert the output to a number instead of a date difference object.
#'
#' @return Vector of differences.
#' @export
#'
#' @examples
#' qdiff( lubridate::mdy( '1/1/2018' ), lubridate::mdy( '3/4/2018' ) )
qdiff = function( x, y, do.date.convert = TRUE, do.numeric = TRUE ){
    ddiff( x, y,  do.date.convert =  do.date.convert, do.numeric = do.numeric, unit = 'quarter' )
}