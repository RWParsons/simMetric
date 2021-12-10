test_that("modSE() estimation works", {
  set.seed(42)
  modSE_vec <- modSE(se=rnorm(n=50, mean=16, sd=2))
  expect_equal(round(modSE_vec[['modSE']]), 16)
})

test_that("modSE() handles NAs", {
  set.seed(123)

  se <- rnorm(n=50, mean=9, sd=1)
  se <- append(se, c(NA, NA, NA))

  modSE_vec <- modSE(se=se)
  expect_equal(modSE_vec[['modSE']], NA)

  modSE_vec <- modSE(se=se, na.rm=T)
  expect_equal(round(modSE_vec[['modSE']]), 9)
})
