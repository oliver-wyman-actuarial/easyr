context("cache")

test_that("works as expected", {
  
    if('qs' %in% utils::installed.packages()){
        
        cache.init(
        
        at.path = tempdir(),
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

    }
  
})