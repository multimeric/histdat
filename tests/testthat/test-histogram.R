test_that("histogram math is the same as regular math", {
  h <- HistDat::HistDat(vals = 1:4, counts = c(1, 2, 2, 1))
  v <- as.vector(h)

  # First, check that converting to a vector works, as the rest of the tests
  # rely on this
  expect_equal(v, c(1, 2, 2, 3, 3, 4))


  # These test that statistical functions work equally well on a HistDat
  # instance and a numerical vector. This tests not only the HistDat methods
  # but also it tests that the generic versions of var, sd, and as.cdf are
  # available
  for (func in c(
    min,
    max,
    mean,
    median,
    var,
    sd,
    range,
    function(x) as.ecdf(x)(3),
    function(x) quantile(x, probs = 0.5, names = F, type = 2),
    function(x) quantile(x, probs = 0.2, names = F, type = 2)
  )) {
    expect_equal(func(h), func(v))
  }
})
