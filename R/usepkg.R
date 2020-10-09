#' Use Package
#'
#' Installs a package if it needs to be installed, and calls require to load the package.
#' Author: Scott Sobel. Tech Review: Bryce Chamberlain.
#'
#' @param packages Character or character vector with names of the packages you want to use.
#' @param noCache When checking packages, you can choose to ignore the cached list, which will increase accuracy but decrease speed.
#' @param repos choose the URL to install from.
#'
#' @export
#'
#' @examples
#' # packages shouldn't be installed during tests or examples according to CRAN. 
#' # therefore, examples cannot be provided because CRAN now runs donttest examples.
usepkg <- function( packages, noCache = FALSE, repos = 'http://cran.us.r-project.org' ){
  
  # Install missing packages.
  notinstalled = packages[ sapply(packages, function(pkgname) length(base::find.package(pkgname, quiet = TRUE)) == 0) ]
  if( length( notinstalled ) > 0 ) utils::install.packages( notinstalled, dependencies = TRUE, repos = repos )
  
  # Require the packages.
  # require is faster than library since it doesn't re-load items.
  for( package in packages ) require( package, character.only = TRUE )
  
}
