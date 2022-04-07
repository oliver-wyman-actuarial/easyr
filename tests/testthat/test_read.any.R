test_that( "NAs are replaced properly", {
  
  # do NAs read as expected?
  t = read.any( test_file( 'na-check.csv' ), drop.na.cols = FALSE )
  expect_equal( sum( is.na( t$val ) ), sum( t$isna ) )
                
})

test_that( "Null columns are dropped", {
  
  expect_equal(
    ncol( read.any( test_file( 'null-columns.xlsx' ), first_column_name = 'Period End' ) ),
    3
  )
  
})

test_that( "Mixed excel-number and string dates convert correclty", {
  
  expect_equal(
    read.any( test_file( 'mix-dateformat-xl-char.rds' ) )$Repair.Date,
    lubridate::mdy( '8/29/2017', '12/20/2016', '8/28/2017', '8/25/2017', '8/28/2017', '1/6/2014', '1/3/2014', '1/23/2014', '12/6/2013', '1/13/2014' )
  )
  
})

test_that( 'require_columns works properly', {
  
  expect_error( 
    read.any( test_file( 'mix-dateformat-xl-char.rds' ), require_columns = 'this is not a column' ),
    'this is not a column'
  )
  
})

test_that( 'file with 0 rows reads properly', {
  
  expect_equal( 
    nrow( read.any( test_file( 'zero-rows.csv' ) ) ),
    0
  )
  
})

test_that( 'Headers found using field_name_map, read column to row names', {
  
  t = read.any( test_file( 'sim-data.csv' ), field_name_map = c( 'Claim Number' = 'id' ), row.names.column = 'id' )
  
  # Find columns from field_name_map
  expect_equal( 
    colnames(t)[1],
    'Accident Date'
  )
  
  expect_equal(
    rownames(t),
    paste0( 'c', 1:48 )
  )
  
})

test_that( 'Single-column data reads properly', {
  expect_equal(
    colnames( read.any( test_file( 'one-column-test.csv' ) ) ),
    'data' 
  )
  
})
    
test_that( 'stringsAsFactors works properly', {
  
  expect_equal(
    class( read.any( test_file( 'sim-data.csv' ), first_column_name = 'Claim Number', stringsAsFactors = TRUE )$status ),
    'factor'
  )
  
  
})

test_that( 'read HTML saved as XLS', {
  
  expect_equal( 
    read.any( test_file( 'html-as-xls.xls' ), header = FALSE, verbose = FALSE )$V1[1],
    'value1'
  )
  
})

test_that( 'data types are correctly identified and converted', {
  
  t = read.any( test_file( 'sim-data.csv' ), first_column_name = 'Claim Number' )
  
  expect_equal( 
    sapply( t, class ),
    c( 'Claim Number' = 'character', 'Accident Date' = 'Date', 'Total Paid' = 'numeric', 'Final Reserve' = 'numeric', 'reserve.paid' = 'numeric', 'litigation' = 'logical', 'status' = 'character', 'Incurred' = 'numeric' )
  )
  
  expect_equal(
    t$`Accident Date`,
    lubridate::mdy( c(
      '03/20/2009', '04/30/2009', '04/09/2009', '03/17/2009', '02/09/2009', '04/19/2009', '04/03/2009', '02/16/2009', '04/17/2009', '04/25/2009', '03/29/2009', '03/20/2009', '02/28/2009', '03/20/2009', '03/22/2009', '02/10/2009', '03/29/2009', '04/10/2009', '04/18/2009', '04/02/2009', '04/14/2009', '04/26/2009', '03/15/2009', '03/04/2009', '03/01/2009', '03/08/2009', '02/13/2009', '03/26/2009', '04/15/2009', '04/21/2009', '03/03/2009', '02/28/2009', '03/08/2009', '04/15/2009', '04/29/2009', '02/06/2009', '03/19/2009', '02/14/2009', '04/01/2009', '04/07/2009', '04/24/2009', '04/02/2009', '03/31/2009', '02/22/2009', '04/03/2009', '04/22/2009', '03/16/2009', '03/09/2009' 
    ))
  )
  
})

test_that( 'times read in properly', {
  
  expect_equal(
    sapply( read.any( test_file( 'date-time.csv' ), allow_times = TRUE ), class )[1,],
    c( 'date' = 'POSIXct', 'time' = 'POSIXct' )
  )
  
})

#test_that( 'PDF read', {
#    
#    if('pdftools' %in% utils::installed.packages()){
#      
#      t = read.any( test_file( 'test.pdf' ) )
#
#      expect_equal(
#        t,
#        data.frame( line=c( 
#          'headerinfo 1',
#          'headerinfo 2',
#          'row1, row1',
#          'row2, row2'
#        ), stringsAsFactors=FALSE)
#      )
#
#  }
#  
#})

test_that( 'read xlsb', {

  expect_equal( 
    nrow(read.any( test_file( 'sample.xlsb'))),
    14
  )

  expect_equal( 
    nrow(read.any( test_file( 'sample.xlsb'), sheet = 1)),
    14
  )

  expect_equal( 
    nrow(read.any( test_file( 'sample.xlsb'), sheet = 2)),
    14
  )

})

