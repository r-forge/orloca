library('orloca')
library('testthat')

test_that('loca.p constructor with minimum number of parameters', {
    l <- loca.p(1:3, 4:6)
    expect_s4_class(l, 'loca.p')
    expect_equal(l@x, 1:3)
    expect_equal(l@y, 4:6)
    expect_equal(l@w, rep(1, 3))
    expect_equal(l@label, '')
    
})

test_that('loca.p constructor with label', {
    l <- loca.p(1:3, 4:6, label = 'loca.p label test')
    expect_s4_class(l, 'loca.p')
    expect_equal(l@x, 1:3)
    expect_equal(l@y, 4:6)
    expect_equal(l@w, rep(1, 3))
    expect_equal(l@label, 'loca.p label test')
})

test_that('loca.p constructor with weights', {
    l <- loca.p(1:3, 4:6, 7:9)
    expect_s4_class(l, 'loca.p')
    expect_equal(l@x, 1:3)
    expect_equal(l@y, 4:6)
    expect_equal(l@w, 7:9)
    expect_equal(l@label, '')
})

test_that('print.loca.p', {
    l <- loca.p(1:3, 4:6, 7:9)
    p <- try(print(l))
    expect_equal(l, p)
    })


test_that('summary.loca.p', {
    s <- summary(loca.p(1:3, 4:6, 7:9, label = 'summary.loca.p test'))
    p <- try(print(s))
    expect_equal(s, p)
})

