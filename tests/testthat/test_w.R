context("w")

test_that("w works as expected", {

    if( !dir.exists( test_file_temp ) ) dir.create( test_file_temp )
    
    # Write a file.
    w( cars, paste0( test_file_temp, '/cars' ) )

    # Read the file and delete it.
    t = read.csv( paste0( test_file_temp, '/cars.csv' ) )
    file.remove( paste0( test_file_temp, '/cars.csv' ) )

    # Verify it is as expected.
    expect_equal( t, cars )
    
})