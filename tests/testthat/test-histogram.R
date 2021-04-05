# These functions will be the same when the counts are multiplied by the
# same coefficient
count_independent_funcs = c(
  # min,
  # max,
  # mean,
  # median,
  # var,
  # sd,
  # range,
  # function(x) as.ecdf(x)(3),
  function(x) quantile(x, probs = seq(0, 1, 0.1), names = F, type = 2)
)

# These functions will not be the same when the counts are multiplied by the
# same coefficient
count_multiplied_funcs = c(
  length,
  sum
)

count_dependent_funcs = c(
  function(x) x[1:5],
  function(x) x[2]
)



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

  # We can use the count dependent funcs in this test because we're comparing
  # to a vector with the same number of counts
  for (func in c(count_dependent_funcs, count_independent_funcs, count_multiplied_funcs)) {
    expect_equal(func(h), func(v))
  }
})

test_that("HistDat works with massive counts", {
  # Set a memory limit to 1 GB so the process doesn't hang, but rather crashes immediately
  # lim = unix::rlimit_as(1e9)

  # We have 6000 billion counts, which absolutely can't fit in RAM
  h_big <- HistDat::HistDat(vals = 1:4, counts = c(1, 2, 2, 1) * 1e12)
  h_small <- HistDat::HistDat(vals = 1:4, counts = c(1, 2, 2, 1))

  # These stats are independent of the actual counts, so we can test for
  # equality
  for (func in count_independent_funcs) {
    expect_equal(func(h_big), func(h_small))
  }

  # These stats are not independent of the actual counts, so we can test for
  # equality only after removing the coefficient
  for (func in count_multiplied_funcs) {
    expect_equal(func(h_big) / 1e12, func(h_small))
  }
})
