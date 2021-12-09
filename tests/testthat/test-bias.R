test_that("bias() estimation works", {
  set.seed(42)
  bias_vec <- bias(true_value=0, estimates=rnorm(100, mean=1))
  expect_equal(round(bias_vec[['bias']]), 1)
})

test_that("bias() handles NAs", {
  set.seed(42)
  bias_vec <- bias(true_value=0, estimates=c(rnorm(50, mean=3), NA, NA, NA))
  expect_equal(bias_vec[['bias']], NA)

  bias_vec <- bias(true_value=0, estimates=c(rnorm(50, mean=3), NA, NA, NA), na.rm=T)
  expect_equal(round(bias_vec[['bias']]), 3)
})
