library('orloca')
library('testthat')

test_that('distsum function', {
    l <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), w = c(1, 1, 2, 1))
    expect_equal(distsum(l), 7.07106781)
    expect_equal(distsum(l, 1, 1), 6.82842712)
    expect_equal(distsum(l, lp = 1.5), 7.93700526)
})

test_that('distsumgra function', {
    l <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), w = c(1, 1, 2, 1))
    expect_equal(distsumgra(l), c(-.70710678, -.70710678))
    expect_equal(distsumgra(l, 1, 1), c(NaN, NaN))
    expect_equal(distsumgra(l, lp = 1.5), c(-.79370052, -.79370052))
})

test_that('distsummin function', {
    l <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), w = c(1, 1, 2, 1))
    expect_equal(distsummin(l), c(.57667826, .57667826))
    expect_equal(distsummin(l, 1, 1), c(.5767037, .5767037))
    expect_equal(distsummin(l, lp = 1.5), c(.92004933, .92004933))
})

