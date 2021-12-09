test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("coverage estimation", {
  coverage_vec <- coverage(true_value=0, ll=c(rep(-1,9), 1), ul=c(rep(1,9), -1))
  expect_equal(coverage_vec[['coverage']], 0.9)
  expect_equal(coverage_vec[['coverage_mcse']], 0.09486833)
})

test_that("add a column to dataframe", {
  test_df <- data.frame(
    ll=rnorm(100),
    ul=rnorm(100),
    group_id=sample(c("a", "b"), 100, replace=T)
  )
  expect_silent({
    test_df$cov <- coverage(true_value=0, ll=test_df$ll, ul=test_df$ul, get=c("coverage"))
    test_df |>
      dplyr::group_by(group_id) |>
      dplyr::summarize(
        coverage=coverage(true_value=0, ll=ll, ul=ul, get=c("coverage")),
        coverage_mcse=coverage(true_value=0, ll=ll, ul=ul, get=c("coverage_mcse"))
      )
  })
})