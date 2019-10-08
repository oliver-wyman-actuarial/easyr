# Get test file path.
# Usage:
#   test_file("blanks.xls")
# Modified from https://github.com/tidyverse/readxl/blob/master/tests/testthat/helper.R
test_file = function( fname ) rprojroot::find_testthat_root_file( "test-files", fname )
test_file_temp = rprojroot::find_testthat_root_file( "test-files-temp" )