library('orloca')
library('testthat')

test_that('loca.p random generator with minimum number of parameters', {
    set.seed(1)
    l <- rloca.p(3)
    expect_s4_class(l, 'loca.p')
    expect_equal(l@x, c(.26550866, .3721239, .57285336))
    expect_equal(l@y, c(.90820779, .20168193, .89838968))
    expect_equal(l@w, rep(1, 3))
    expect_equal(l@label, '')
    
})

test_that('loca.p random generator with label', {
    set.seed(1)
    l <- rloca.p(3, label = 'rloca.p label test')
    expect_s4_class(l, 'loca.p')
    expect_equal(l@x, c(.26550866, .3721239, .57285336))
    expect_equal(l@y, c(.90820779, .20168193, .89838968))
    expect_equal(l@w, rep(1, 3))
    expect_equal(l@label, 'rloca.p label test')
})

test_that('rloca.p random generator with cero groups', {
    set.seed(1)
    l <- rloca.p(3, groups = 0)
    expect_equal(l@x, c(.26550866, .3721239, .57285336))
    expect_equal(l@y, c(.90820779, .20168193, .89838968))
    expect_equal(l@w, rep(1, 3))
    expect_s4_class(l, 'loca.p')
    expect_equal(l@label, '')
})

test_that('rloca.p random generator with one group', {
    set.seed(1)
    l <- rloca.p(3, groups = 1)
    expect_equal(l@x, c(.8383620, 1.17371645, .46719059))
    expect_equal(l@y, c(1.27051358, 1.31679916, 1.03292169))
    expect_equal(l@w, rep(1, 3))
    expect_s4_class(l, 'loca.p')
    expect_equal(l@label, '')
})

test_that('rloca.p random generator with two groups', {
    set.seed(1)
    l <- rloca.p(5, groups = 2)
    expect_equal(l@x, c(.83836202, 1.17371645, 1.57378931, 1.00646153, 1.15064984))
    expect_equal(l@y, c(.57380583, 1.27051358, .83735454, 1.34782063, 1.04490151))
    expect_equal(l@w, rep(1, 5))
    expect_s4_class(l, 'loca.p')
    expect_equal(l@label, '')
})

test_that('rloca.p random generator with a vector group', {
    set.seed(1)
    l <- rloca.p(5, groups = c(1, 2, 2))
    expect_equal(l@x, c(.83836202, 1.14635720, .86247972, .89299742, .59007829))
    expect_equal(l@y, c(1.28033169, 1.5275037, .96017595, .94639817, .674256))
    expect_equal(l@w, rep(1, 5))
    expect_s4_class(l, 'loca.p')
    expect_equal(l@label, '')
})

test_that('rloca.p random generator with a bigger set of parameters', {
    set.seed(1)
    l <- rloca.p(5, xmin=10, xmax=11, ymin=100, ymax=110, wmin=0, wmax=5, groups = c(1, 2, 2))
    expect_equal(l@x, c(20.83836202, 21.55918747, 21.52750372, 20.881802, 21.10172222))
    expect_equal(l@y, c(212.80331689, 210.06461539, 211.5064984, 217.617475, 211.49876))
    expect_equal(l@w, c(1.0084096, .88278376, 3.43511423, 3.8872261, 4.67352615))
    expect_s4_class(l, 'loca.p')
    expect_equal(l@label, '')
})

test_that('rloca.p random generator with a full set of parameters', {
    set.seed(1)
    l <- rloca.p(5, xmin=10, xmax=11, ymin=100, ymax=110, wmin=0, wmax=5, groups = c(1, 2, 2), xgmin=200, xgmax=210, ygmin=300, ygmax=310)
    expect_equal(l@x, c(213.22794, 219.64469, 219.613010893, 214.338736, 214.55865569))
    expect_equal(l@y, c(412.803316, 410.06461539, 411.5065, 417.617475, 411.498766))
    expect_equal(l@w, c(1.00840965, .88278376, 3.43511423, 3.88722610, 4.67352615))
    expect_s4_class(l, 'loca.p')
    expect_equal(l@label, '')
})
