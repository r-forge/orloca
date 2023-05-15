library('orloca')
library('testthat')

test_that('data.frame to an from loca.p conversion', {
    l <- loca.p(1:3, 4:6, 7:9, label = 'as function test')
    d <- as.data.frame(l)
    ll <- as.loca.p(d)
    expect_equal(l, ll)
})

test_that('matrix to an from loca.p conversion', {
    l <- loca.p(1:3, 4:6, 7:9, label = 'as function test')
    d <- as.matrix(l)
    ll <- as.loca.p(d)
    expect_equal(l, ll)
})


