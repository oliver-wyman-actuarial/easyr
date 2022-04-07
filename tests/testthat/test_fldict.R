test_that("works as expected", {
  
  files = list.files( test_file( '' ), full.names = TRUE )[1:4]
  files = files[ !grepl( '~[$]', files ) ]
  t = fldict( file.list = files )
  
  expect_equal( length(t) > 0, TRUE )
  
  file_paths1 = list.files(
    'test-files',
    full.names = TRUE,
    pattern = '^[^~].+(csv|xlsx|xlsb)'
  )[1:2]
  
  
  count1a = sum(sapply(
    file_paths1, 
    function(file_path) read.any(file_path, all_chars = TRUE) %>% 
      ncol()
  ))
  
  count1b = sum(sapply(1:2, 
    function(sheetnum) read.any('test-files/html-as-xls.xls', sheet = sheetnum, all_chars = TRUE) %>% 
      ncol()
  ))
  
  count1 = count1a + count1b
    
  file_paths = list.files(
    'test-files',
    full.names = TRUE,
    pattern = '^[^~].+(csv|xlsx|xlsb|xls)'
  )[1:3]
  
  count2 = fldict(file.list = file_paths)$columns %>% nrow()
  
  expect_equal(count1, count2)
   
})