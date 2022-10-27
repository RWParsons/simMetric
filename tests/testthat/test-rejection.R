test_that("rejection() estimation works", {
  set.seed(42)
  rejection_vec <- rejection(p = runif(1000))
  expect_equal(round(rejection_vec[["rejection"]], digits = 2), 0.05)
})

test_that("rejection() handles NAs", {
  set.seed(42)
  rejection_vec <- rejection(p = c(runif(1000), NA, NA, NA))
  expect_equal(rejection_vec[["rejection"]], NA)

  rejection_vec <- rejection(p = c(runif(1000), NA, NA, NA), alpha = 0.5, na.rm = T)
  expect_equal(round(rejection_vec[["rejection"]], digits = 2), 0.5)
})
