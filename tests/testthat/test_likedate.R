context("likedate")

test_that("works as expected", {
  
    expect_equal( likedate( c( '20171124', '2017/12/24', NA, '12/24/2017', '5/11/2017 1:51PM' ) ), TRUE )
    expect_equal( likedate( c("12/30/2017 10:09AM", "12/30/2017 11:10AM", "12/29/2017 2:25PM",  "12/29/2017 9:15AM",  "12/29/2017 3:00PM" ) ), TRUE )
    
    x <- 
    expect_equal( likedate( c('20171124','2017/12/24',NA,'12/24/2017','March 3rd, 2015','Mar 3, 2016') ), TRUE )
    expect_equal( likedate( c(123,456,NA)), FALSE )
    expect_equal( likedate( '3312019' ), TRUE )
    expect_equal( likedate( '2019.1.3' ), TRUE )

})