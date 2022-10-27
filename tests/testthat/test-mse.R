test_that("mse() estimation works", {
  set.seed(42)
  est <- sample(c(-1, 1), size = 1e3, replace = T) + rnorm(1e3, sd = 0.01)
  mse_vec <- mse(true_value = 0, estimates = est)
  expect_equal(round(mse_vec[["mse"]], digits = 2), 1)
})

test_that("mse() handles NAs", {
  set.seed(42)

  est <- sample(c(-2, 2), size = 1e3, replace = T) + rnorm(1e3, sd = 0.01)
  est <- append(est, c(NA, NA, NA))

  mse_vec <- mse(true_value = 0, estimates = est)
  expect_equal(mse_vec[["mse"]], NA)

  mse_vec <- mse(true_value = 0, estimates = est, na.rm = T)
  expect_equal(round(mse_vec[["mse"]], digits = 2), 4)
})
