# These functions will not be affected if all when the counts are multiplied by the
# same coefficient
count_independent_funcs = c(
  min,
  max,
  mean,
  median,
  range,
  function(x) as.ecdf(x)(3),
  function(x) quantile(x, probs = seq(0, 1, 0.1), names = F, type = 2)
)

# These functions will not be the same when the counts are multiplied by the
# same coefficient, but if we divide these stats by that coefficient, we can
# recover the original statistic
count_multiplied_funcs = c(
  length,
  sum
)

# These functions are totally dependent on the count, so we can never compare
# these functions between datasets with different counts
count_dependent_funcs = c(
  function(x) x[1:5],
  function(x) x[2],
  var,
  sd
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

test_that("The c operator behaves correctly", {
  h <- HistDat::HistDat(vals = 1:4, counts = c(1, 2, 2, 1))
  h2 = c(1, 3, 6, h)
  h3 = c(h, 1, 3, 6)

  # The order of concatenation shouldn't matter
  expect_equal(h2@vals, h3@vals)
  expect_equal(h2@counts, h3@counts)

  # Concatenation should add new vals if needed, and increment counts otherwise
  expect_equal(h2@vals, c(1, 2, 3, 4, 6))
  expect_equal(h2@counts, c(2, 2, 3, 1, 1))
})

test_that("The constructor function performs the right transformations", {
  h <- HistDat::HistDat(vals = c(1, 3, 4, 2), counts = c(1, 2, 1, 2))
  expect_equal(h@vals, c(1, 2, 3, 4))
  expect_equal(h@counts, c(1, 2, 2, 1))
})
