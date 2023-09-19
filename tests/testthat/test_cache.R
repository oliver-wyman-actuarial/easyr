test_that("works as expected", {
  
    if('qs' %in% utils::installed.packages()){ # caching requires qs.
      
        caches = list(
          list(
            name = 'cache',
            depends.on = c( 'test-files' )
          ),
          list(
            name = 'cache',
            depends.on = c( 'test-files' )
          ),
          list(
            name = 'cache',
            depends.on = c( 'test-files' )
          )
        )

        cache.init(at.path = tempdir(), verbose = FALSE, caches = caches)
        clear.cache()

        # initially the cache should be invalid/non-existent/not-ok.
        expect_equal( cache.ok(1), FALSE )
        expect_equal( cache.ok(2), FALSE )
        
        # now if we use save.cache it should make the cache OK. 
        expect_equal( cache.ok(1), FALSE ) # sets current cache to #1
        save.cache( iris )
        expect_equal( cache.ok(1), TRUE )
        expect_equal( cache.ok(2), FALSE ) # confirm #2 is still not ok. and sets current cache to #2. 
        
        # now save again, which will make the next one ok.
        save.cache( iris )
        expect_equal( cache.ok(1), TRUE )
        expect_equal( cache.ok(2), TRUE )
        
        # now clear #2 and verify #1 is still ok.
        clear.cache(2)
        expect_equal( cache.ok(1), TRUE )

        # clear cache now should invalidate #1.
        clear.cache() 
        expect_equal( cache.ok(1), FALSE )

        # caches with warnings:
        
            cache.init(at.path = tempdir(), verbose = TRUE, caches = caches)
            clear.cache() 

            # first cache, no warning.
            # this is the typical use of caches - check if it's OK and if not then run the script. 
            if(!cache.ok(1)) withCallingHandlers({
                x = iris # base-R dataset.
                save.cache(x)
            }, warning = cache.capture_warning)

            # 2nd cache, do warning. 
            expect_warning(
                if(!cache.ok(2)) withCallingHandlers({
                    y = mtcars # base-R dataset.
                    warning('warning 1')
                    save.cache(x, y)
                }, warning = cache.capture_warning),
                regexp = 'warning 1'
            )

            # 3rd cache, do 2 warnings. sometimes a cache covers multiple scripts. 
            expect_warning(
                if(!cache.ok(3)) withCallingHandlers({
                    z = state.division # base-R dataset.
                    warning('warning 2')
                }, warning = cache.capture_warning),
                regexp = 'warning 2'
            )
            expect_warning(
                if(!cache.ok(3)) withCallingHandlers({
                  warning('warning 3')
                  save.cache(x, y, z)
                }, warning = cache.capture_warning),
                regexp = 'warning 3'
            )
            
            # now if we run the caches again, we'd like to get both warnings again.
            # however we don't because the code gets skipped. 
            expect_warning(
                if(!cache.ok(2)) withCallingHandlers({
                    y = mtcars # base-R dataset.
                    warning('warning 1')
                    save.cache(x, y)
                }, warning = cache.capture_warning),
                regexp = 'warning 1'
            )
            expect_warning({
                if(!cache.ok(3)) withCallingHandlers({
                  z = state.division # base-R dataset.
                  warning('warning 2')
                }, warning = cache.capture_warning)
                if(!cache.ok(3)) withCallingHandlers({
                  warning('warning 3')
                  save.cache(x, y, z)
                }, warning = cache.capture_warning)
              },
              regexp = 'warning 2.+warning 3' # should get 2 warnings. 
            )

    }
  
})
