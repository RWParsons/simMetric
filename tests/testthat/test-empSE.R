test_that("empSE() estimation works", {
  set.seed(42)
  empSE_vec <- empSE(estimates=rnorm(100, sd=1))
  expect_equal(round(empSE_vec[['empSE']]), 1)
})

test_that("empSE() handles NAs", {
  set.seed(42)
  empSE_vec <- empSE(estimates=c(rnorm(50, sd=3), NA, NA, NA))
  expect_equal(empSE_vec[['empSE']], NA)

  empSE_vec <- empSE(estimates=c(rnorm(50, sd=3), NA, NA, NA), na.rm=T)
  expect_equal(round(empSE_vec[['empSE']]), 3)
})
