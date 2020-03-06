#' Initialize cache.
#'
#' Set cache info so easyr can manage the cache.
#'
#'
#' @param caches List of lists with properties name, depends.on. See example.
#' @param at.path Where to save the cache. If NULL, a cache/ folder will be created in the current working directory.
#' @param verbose Print via cat() information about cache operations.
#' @param save.only Choose not to load the cache. Use this if you need to check cache validity in multiple spots but only want to load at the last check.
#' @param skip.missing Passed to hashfiles, choose if an error occurs if a depends.on file isn't found.
#'
#' @export
#'
#' @examples
#' # initialize a cache with 1 cache which depends on files in the current working directory.
#' # this will create a cache folder in your current working directory.
#' # then, you call functions to check and build the cache.
#' cache.init(
#'
#'  # Initial file read (raw except for renaming).
#'  caches = list(
#'    list( 
#'     name = 'prep-files',
#'     depends.on = c( '.' )
#'    )
#'  ),
#' 
#'  at.path = tempdir()
#'
#' )
#' 
cache.init = function( caches, at.path, verbose = TRUE, save.only = FALSE, skip.missing = TRUE ){
  
  validatecaches(caches)

  # unlock bindings so we can modify easyr resources related to cache.
  env = rlang::current_env()
  for( i in c( 'easyr.cache.info', 'cache.at', 'max.cache.loaded','cache.path' ) ){
    base::unlockBinding( i, env = parent.env( env = env ) )
    rm(i)
  }

    easyr.cache.info <<- list(
        cache.num = 0,
        cache.invalidated = FALSE,
        max.cache = 0,
        max.cache.loaded = 0,
        verbose = verbose
    )

  cache.info = caches
  easyr.cache.info$cache.info <<- cache.info

  easyr.cache.info$verbose <<- verbose
  easyr.cache.info$save.only <<- save.only
    
    # We need a directory if it doesn't already exist.
    if( !dir.exists(at.path) ) dir.create(at.path)

    # Add calculated paths.
    for( i in 1:length(cache.info) ){
        cache.info[[i]]$cache.num = i
        cache.info[[i]]$path = cc(at.path, '/', i, '-', cache.info[[i]]$name)
        cache.info[[i]]$status.path = cc(at.path, '/', i, '-', cache.info[[i]]$name, '-status')
        rm(i)
    }

    # Special code to automatically accept cache.info, for when you make a change but don't want to re-run data.
    if( re.cache <- FALSE ){
        
        for( i in cache.info ) saveRDS( 
        hashfiles( i$depends.on, skip.missing = TRUE ), 
        file = i$status.path
        )
        
    }

    # Loop through available cache.info, check the hash and delete any invalidated cache.info.
    easyr.cache.info$max.cache <<- 0
    if( easyr.cache.info$verbose ) cat( 'checking validity of cache \n' )
    
    for( i in 1:length(cache.info) ){
        
        # once the cache is invalidated, all downstream cache.info are invalid.
        if( easyr.cache.info$cache.invalidated || ! file.exists( cache.info[[i]]$path ) ){
        
          clear.cache( cache.info[[i]] )
        
        # if there is no status, delete the cache file.
        } else if( !file.exists( cache.info[[i]]$status.path ) ){
        
          if( file.exists( cache.info[[i]]$path ) ) clear.cache( cache.info[[i]] )
        
        # Otherwise check the status.
        } else if( file.exists( cache.info[[i]]$path ) ){
        
          status.valid = readRDS( cache.info[[i]]$status.path ) == hashfiles( cache.info[[i]]$depends.on, skip.missing = skip.missing )
        
        if( status.valid ){
            easyr.cache.info$max.cache <<- i
        } else {
            clear.cache( cache.info[[i]] )
        }
        
        rm( status.valid )
        
        }
        
    }
  
  easyr.cache.info$cache.info <<- cache.info
  
}


#' Check Cache Status
#' 
#' Check a cache and if necessary clear it to trigger a re-cache.
#'
#' @param cache.num The index/number for the cache we are checking in the cache.info list.
#' @param do.load Load the cache if it is found.
#'
#' @return Boolean indicating if the cache is acceptable. FALSE indicates the cache doesn't exist or is invalid so code should be run again.
#' @export
#'
#' @examples
#' # check the first cache to see if it exists and dependent files haven't changed.
#' # if this is TRUE, code in brackets will get skipped and the cache will be loaded instead.
#' # set do.load = FALSE if you have multiple files that build a cache, 
#' #    to prevent multiple cache loads.
#' # output will be printed to the console to tell you if the cache was loaded or re-built.
#' if( ! cache.ok(1) ){
#' 
#'   # do stuff
#'   
#'   # if this is the final file for this cache, end with save.cache to save passed objects as a cache.
#'   save.cache(iris)
#' }
cache.ok = function( cache.num, do.load = TRUE ){

    if( length(easyr.cache.info$cache.info) == 0 ) stop( 'easyr::cache.ok Error: Cache not set up correctly. Error E356-1 cache.' )

    # save current cache number to global so that save.cache runs correctly.
    checked.already = easyr.cache.info$cache.num == cache.num
    easyr.cache.info$cache.num <<- cache.num

    # check against the maximum valid cache. if it is less, don't load anything just return true.
    if( easyr.cache.info$max.cache > easyr.cache.info$cache.num && do.load && ! easyr.cache.info$save.only ){
        if( !checked.already && easyr.cache.info$verbose ) cat( '\t   >> skip cache [', easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path, ']. \n' )
        return( TRUE )

    # if it is the highest available cache, load it.
    } else if( easyr.cache.info$max.cache == easyr.cache.info$cache.num && do.load && ! easyr.cache.info$save.only ){
        if( !checked.already ){
            if( easyr.cache.info$verbose ) cat( '\t   >> load cache [', easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path, ']. \n' )
            load.cache(easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path)
            easyr.cache.info$max.cache.loaded <<- max.cache.loaded
        }
        return( TRUE )

    } else {
        if( !checked.already ){
            if( 
              !easyr.cache.info$save.only &&  
              file.exists(easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path) &&
              easyr.cache.info$max.cache > 0 && 
              ( is.null(easyr.cache.info$max.cache.loaded) || easyr.cache.info$max.cache.loaded < easyr.cache.info$max.cache )              
            ){
                if( easyr.cache.info$verbose ) cat( '\t   >> load cache [', easyr.cache.info$cache.info[[ easyr.cache.info$max.cache ]]$path, ']. \n' )
                load.cache(easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path)
            }
            if( easyr.cache.info$verbose ) cat( '\t   >> build cache [', easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path, ']. \n' )
        }
        return( FALSE )
    }

    # If you made it this far, load the cache and return true.
    
    return( TRUE )

}

#' Save Cache
#'  
#' Saves the arguments to a cache file, using the cache.num last checked with cache.ok.
#'
#' @param ... Objects to save.
#'
#' @export
#'
#' @examples
#' # check the first cache to see if it exists and dependent files haven't changed.
#' # if this check is TRUE, code in brackets will get skipped and the cache will be loaded instead.
#' # set do.load = FALSE if you have multiple files that build a cache, 
#' #    to prevent multiple cache loads.
#' # output will be printed to the console to tell you if the cache was loaded or re-built.
#' if( ! cache.ok(1) ){
#' 
#'   # do stuff
#'   
#'   # if this is the final file for this cache, end with save.cache to save passed objects as a cache.
#'   save.cache(iris)
#' }
#' 
#' # delete the cache folder to close out the example.
#' system( "rm -r cache" )
#' 
save.cache = function( ... ){

    if( length(easyr.cache.info$cache.info) == 0 ) stop( 'easyr::cache.ok Error: Cache not set up correctly. Error E356-2 cache.' )
        
    saveRDS(
        hashfiles( easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$depends.on, skip.missing = TRUE ),
        file = easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$status.path
    )
    
    cache.at <<- lubridate::now()
    cache.path <<- easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$path
    
    easyr.cache.info$max.cache.loaded <<- easyr.cache.info$cache.num
    if( easyr.cache.info$max.cache.loaded  > easyr.cache.info$max.cache ) easyr.cache.info$max.cache <<- easyr.cache.info$cache.num
    
    max.cache.loaded <<- easyr.cache.info$max.cache.loaded
    
    datalist = list(...)
    names(datalist) = trimws(strsplit(gsub('save.cache\\(([^)]+)\\)', '\\1', cc(deparse(sys.call()))), ",")[[1]])
    datalist$cache.at = cache.at
    datalist$max.cache.loaded = max.cache.loaded
    datalist$cache.path = cache.path
    qs::qsave(datalist, file = cache.path)

}

#' Clear Cache
#' 
#' Clears all caches or the cache related to the passed cache info list.
#'
#' @param cache The cache list to clear.
#'
#' @return FALSE if a cache info list item is passed in order to assist other functions in returning this value, otherwise NULL. 
#' @export
#'
#' @examples
#' # this will only have an effect if a current cache exists.
#' clear.cache()
#' 
clear.cache = function( cache = NULL ){

    if( length(easyr.cache.info$cache.info) == 0 ) stop( 'easyr::cache.ok Error: Cache not set up correctly. Error E356-3 cache.' )

    # determine which cache to clear
    # if nothing passed, clear them all
    if( is.null( cache ) ){
        do.caches = easyr.cache.info$cache.info
        
    # if numeric, assume an index was passed and select the relevant caches.
    } else if( is.numeric(cache) ){
        do.caches = list( easyr.cache.info$cache.info[[ cache ]] )
        
    # otherwise assume a cache list was passed.
    } else {
      do.caches = list(cache)
    }

    for( icache in do.caches ){

        if( file.exists( icache$status.path ) ) file.remove( icache$status.path )
        if( file.exists( icache$path ) ) file.remove( icache$path )
      
        if( easyr.cache.info$max.cache.loaded >= icache$cache.num ) easyr.cache.info$max.cache.loaded <<- icache$cache.num - 1
        if( easyr.cache.info$max.cache >= icache$cache.num ) easyr.cache.info$max.cache <<- icache$cache.num - 1

    }

    easyr.cache.info$cache.invalidated <<- TRUE
    if( !is.null( cache ) ) return(FALSE)

}

# utils
validatecaches = function( caches ){
  
  if( ! is.list(caches) ) stop( '[caches] must be a list of lists.' )
  
  for( i in caches ){
    
    if( ! is.list(i) ) stop( '[caches] must be a list of lists.' )
    
    missing.properties = setdiff( c( 'name', 'depends.on' ), names(i) ) 
    if( length(missing.properties) > 0 ) stop( 'Cache is missing property(s): [', cc( missing.properties, sep = ', ' ), '].' )
    
    rm(i,missing.properties)
    
  }
  
}
load.cache = function(filename) list2env(qs::qread(filename, use_alt_rep=TRUE), globalenv())

easyr.cache.info = list(
    cache.num = 0,
    cache.invalidated = FALSE,
    max.cache = 0,
    max.cache.loaded = 0,
    verbose = TRUE
)
cache.at = NULL
max.cache.loaded = -1
cache.path = 'cache'