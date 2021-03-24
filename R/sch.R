#' Search a Data Frame.
#' 
#' Searches all columns for a term and returns all rows with at least one match.
#' Author: Bryce Chamberlain.
#'
#' @param x Data to search.
#' @param pattern Regex patter to search. Most normal search terms will work fine, too.
#' @param ignore.case Ignore case in search (uses grepl).
#' @param fixed Passed to grepl to match string as-is instead of using regex. See ?grepl.
#' @param pluscols choose columns to return in addition to those where matches are found. Can be a name, number or 'all' to bring back all columns.
#' @param exact Find exact matches intead of pattern matching.
#' @param trim Use trimws to trim columns before exact matching.
#' @param spln Sample data use easyr::spl() before searching. This will speed up searching in large datasets when you only need to identify columns, not all data that matches. See ?spl n argument for more info.
#'
#' @return Matching rows.
#' @export
#'
#' @examples
#' sch( iris, 'seto' )
#' sch( iris, 'seto', pluscols='all' )
#' sch( iris, 'seto', pluscols='Sepal.Width' )
#' sch( iris, 'seto', exact = TRUE ) # message no matches and return NULL
sch = function( x, pattern, ignore.case = FALSE, fixed = FALSE, pluscols = NULL, exact = FALSE, trim = TRUE, spln = NULL ){

  if(!is.null(pluscols) && length(pluscols)==0) stop('easyr::sch pluscols is zero-length. Please pass a vector or NULL.')

  pattern = as.character(pattern)
  if(trim) pattern = trimws(pattern)
  if(ignore.case) pattern = tolower(pattern)

  # list
  if(is.null(ncol(x)) && is.list(x)){

    # apply sampling.
    if(!is.null(spln)){
      splids = spl(1:length(x), n = spln)
      tnames = names(x)
      x = lapply(splids, function(i) x[[i]])
      if(!is.null(tnames)) names(x) = tnames[splids]
    }

    # get matches
    tnames = names(x)
    matches = which(sapply(1:length(x), function(i){
      any(grepl(pattern, unlist(x[[i]], recursive = TRUE), fixed = fixed, ignore.case = ignore.case))
    }))

    if(length(matches)==0) cat('No matches found.\n')

    toreturn = lapply(matches, function(i) x[[i]])
    if(!is.null(tnames)) names(toreturn) = tnames[matches]    

    return(toreturn)

  # vector
  } else if( is.null( nrow(x) ) ){

    stop('easyr: sch() should not be used for vectors. use grep( ..., value = TRUE) instead.')

  # otherwise we assume data.frame.
  } else {

    if(!is.null(spln)) x = spl(x, n=spln)

    # convert pluscols to numeric, it'll be easier to work with.
    if(!is.null(pluscols)){
      if(pluscols=='all'){
        pluscols = 1:ncol(x)
      } else {
        validatecolnames(x, pluscols)
        if(!is.numeric(pluscols)) pluscols = which(names(x) %in% pluscols)
      }
    }

    rows = integer()
    cols = integer()

    # search each column.
    for( i in 1:ncol(x) ){

      if(exact){

        icol = as.character(x[[i]])
        if(trim) icol = trimws(icol)
        if(ignore.case) icol = tolower(icol)

        matchrows = which(icol == pattern)
        
        rm(icol)

      } else {
        matchrows = grep(pattern, x[[i]], ignore.case = ignore.case, fixed = fixed)
      }

      if(length(matchrows)>0){
        rows = c( rows, matchrows )
        cols = c( cols, i)
      }

      rm(i, matchrows)

    }

    if(length(rows)==0){
      cat('No matches found.\n')
    } else {

      rows = sort(unique(rows))
      cols = sort(unique(c(cols, pluscols)))

      return( x[rows, cols, drop=FALSE] )

    }

  }
    
}
