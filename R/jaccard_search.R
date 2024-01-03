#' Jaccard Similarity Search
#' 
#' Search for similar strings using Jaccard Similarity. 
#'
#' @param search Single character/string to search for. 
#' @param context Vector of characters to search within. 
#' @param level Returned characters will be this similar or more similar. Higher values will return fewer/closer matches.
#' @param return_similarity Special option for diagnosing. TRUE will ignore [level] and return a named vector where the name is the context value and the value is the similarity. 
#'
#' @return Characters that meet the similarity requirement.
#' @export
#'
#' @examples
#' jaccard_search('foobar', c('foo', 'bar', 'foobars'))
#' jaccard_search('foobar', c('foo', 'bar', 'foobars'), return_similarity = TRUE)
jaccard_search = function(search, context, level = 0.5, return_similarity = FALSE){

    dt = expand.grid(input = unique(search), context = unique(context), stringsAsFactors = FALSE)
    
    jaccardSimilarity = function(string1, string2) {
        string1 = strsplit(as.character(string1), "")[[1]]
        string2 = strsplit(as.character(string2), "")[[1]]
        intersection = length(intersect(string1, string2))
        union = length(union(string1, string2))
        similarity = intersection / union
        return(similarity)
    }

    dt$similarity = sapply(split(dt, 1:nrow(dt)), function(x) jaccardSimilarity(x$input, x$context))

    if(return_similarity){
        toreturn = dt$similarity
        names(toreturn) = dt$context
    } else {
        dt = dt[dt$similarity > level, ]
        dt = dt[order(dt$input), ]
        dt = dt[order(-dt$similarity), ]
        toreturn = unique(dt$context)
    }

    return(toreturn)
    
}

