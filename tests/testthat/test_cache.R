context("cache")

test_that("works as expected", {
    
    if( !dir.exists( test_file_temp ) ) dir.create( test_file_temp )
    
    # these tests will run at 
  
    cache.init(
      
      at.path = test_file_temp,
      verbose = FALSE,
      
      caches = list(

          list(
              name = 'cache',
              depends.on = c( 'test-files' )
          ),
          
          list(
              name = 'cache',
              depends.on = c( 'test-files' )
          )
          
      )
    )

    expect_equal( cache.ok(1), FALSE )
    expect_equal( cache.ok(2), FALSE )
    
    expect_equal( cache.ok(1), FALSE )
    save.cache( iris )
    expect_equal( cache.ok(1), TRUE )
    expect_equal( cache.ok(2), FALSE )
    
    save.cache( iris )
    expect_equal( cache.ok(1), TRUE )
    expect_equal( cache.ok(2), TRUE )
    
    clear.cache(2)
    expect_equal( cache.ok(1), TRUE )
    clear.cache()
    
    expect_equal( cache.ok(1), FALSE )

    # clear test files and folders.
    system( "rm -r tests/testthat/test-files-temp/cache" )
    system( "rm -r cache" )
  
})