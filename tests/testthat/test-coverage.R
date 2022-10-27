test_that("coverage estimation", {
  coverage_vec <- coverage(true_value = 0, ll = c(rep(-1, 9), 1), ul = c(rep(1, 9), 2))
  expect_equal(coverage_vec[["coverage"]], 0.9)
  expect_equal(coverage_vec[["coverage_mcse"]], 0.09486833)
})

test_that("add a column to dataframe", {
  test_df <- data.frame(
    ll = rnorm(20),
    ul = rnorm(20, mean = 5),
    group_id = sample(c("a", "b"), 20, replace = T)
  )
  expect_silent({
    test_df$cov <- coverage(true_value = 0, ll = test_df$ll, ul = test_df$ul, get = c("coverage"))

    test_df_grouped <- dplyr::group_by(test_df, group_id)

    dplyr::summarize(test_df_grouped,
      coverage = coverage(true_value = 0, ll = ll, ul = ul, get = c("coverage")),
      coverage_mcse = coverage(true_value = 0, ll = ll, ul = ul, get = c("coverage_mcse"))
    )
  })
})
