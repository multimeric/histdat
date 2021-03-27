test_that("histogram math is the same as regular math", {
  h = histdat::hist_dat(vals = 1:4, counts = c(1, 2, 2, 1))
  v = histdat::as.vector(h)
  expect_equal(v, c(1, 2, 2, 3, 3, 4))

  cdf_at_3 = function(x) {
    as.ecdf(x)(3)
  }

  quant_05 = function(x){
    quantile(x, probs=0.5, names=F, type=2)
  }

  for (func in c(
    min,
    max,
    mean,
    median,
    histdat::var,
    histdat::sd,
    range,
    cdf_at_3,
    quant_05
  )) {
    expect_equal(func(h), func(v))
  }
})
