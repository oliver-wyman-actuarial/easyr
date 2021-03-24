rm( list = ls( all = TRUE ) )

# ensure consistent Roxygen version
if(packageVersion("roxygen2") != '7.1.1') devtools::install_version('roxygen2', version = "7.1.1")

setwd( dirname( rstudioapi::getSourceEditorContext()$path ) )
devtools::document()
devtools::document()
devtools::install(upgrade = 'never')

# Check to make sure each R file has a related test file.

  rfiles = list.files( 'R' )  
  rfiles = rfiles[ !grepl( 'totype|utils|usepkg', rfiles ) ] # files that do not need tests.  
  rtests = list.files( 'tests/testthat' )
  
  rtests_expected = paste0('test_',rfiles)
  need_tests = rtests_expected[ ! rtests_expected %in% rtests ]
  
  if( length(need_tests) > 0 ) warning(
    'Not all R files have test files. Please create test files for [', 
    paste(need_tests,collapse = '], ['), 
    ']'
  )

# Run examples and tests.

  tryCatch({
    
    # this is required in newer versions.
    usethis::use_testthat()
    
  }, error = function(e) devtools::use_testthat()
  )
    
  devtools::run_examples()
  devtools::test()

# clear test files and folders.

  system( "rm -r tests/testthat/test-files-temp/cache" )
  system( "rm -r cache" )
  system( "rm -r tests/testthat/test-files-temp" )

# Optionally, build to compressed tar.gz.
# devtools::build( vignettes = FALSE )

# Optionally, run noLD check:
# https://www.r-bloggers.com/a-nold-platform-on-r-hub-package-builder/
# in R from the home directory, run:
# rhub::check(platform = "debian-gcc-devel-nold") 