test_that("works as expected", {
    expect_equal(
        similar_text('foobar', c('foo', 'bar', 'foobars'), return_similarity = TRUE),
        c('foo' = 0.4, 'bar' = 0.6, 'foobars' = 5/6)
    )
    expect_equal(
        similar_text('foobar', c('foo', 'bar', 'foobars')),
        c('foobars', 'bar')
    )
    expect_equal(
        similar_text('foobar', c('foo', 'bar', 'foobars'), level = 0),
        c('foobars', 'bar', 'foo')
    )
    expect_equal(
        similar_text('foobar', c('foo', 'bar', 'foobars'), level = 1),
        character(0)
    )
})

