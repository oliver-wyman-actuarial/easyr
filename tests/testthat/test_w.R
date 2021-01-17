test_that("w works as expected", {
    
    # Write a file.
    w( cars, paste0( tempdir(), '/cars' ) )

    # Read the file and delete it.
    t = read.csv( paste0( tempdir(), '/cars.csv' ) )
    file.remove( paste0( tempdir(), '/cars.csv' ) )

    # Verify it is as expected.
    expect_equal( t, cars )
    
})