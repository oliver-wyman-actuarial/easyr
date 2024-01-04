#' Text Similarity Search
#' 
#' Search for similar strings using in a vector.
#'
#' @param search Single character/string to search for. 
#' @param context Vector of characters to search within. 
#' @param algo Algorithm to use when determining similarity. Currenly, only Jaccard Similarity is implemented. 
#' @param level Returned characters will be this similar or more similar. Higher values will return fewer/closer matches.
#' @param return_similarity Special option for diagnosing. TRUE will ignore [level] and return a named vector where the name is the context value and the value is the similarity. 
#'
#' @return Characters that meet the similarity requirement.
#' @export
#'
#' @examples
#' similar_text('foobar', c('foo', 'bar', 'foobars'))
#' similar_text('foobar', c('foo', 'bar', 'foobars'), return_similarity = TRUE)
similar_text = function(search, context, algo = 'jaccard', level = 0.5, return_similarity = FALSE){

    dt = expand.grid(input = unique(search), context = unique(context), stringsAsFactors = FALSE)
    dt$input_lcase = tolower(as.character(dt$input))
    dt$context_lcase = tolower(as.character(dt$context))
    
    if(algo == 'jaccard') simfn = function(string1, string2) {
        string1 = strsplit(as.character(string1), "")[[1]]
        string2 = strsplit(as.character(string2), "")[[1]]
        intersection = length(intersect(string1, string2))
        union = length(union(string1, string2))
        similarity = intersection / union
        return(similarity)
    }

    dt$similarity = sapply(split(dt, 1:nrow(dt)), function(x) simfn(x$input_lcase, x$context_lcase))

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

