test_that("joining metrics works", {

  simulations_df <- data.frame(
    idx=rep(1:10, 100),
    idx2=sample(c("a", "b"), replace=T, 1000),
    p_value=runif(1000),
    est=rnorm(n=1000),
    conf.ll= rnorm(n=1000, mean=-20),
    conf.ul= rnorm(n=1000, mean=20)
  )
  expect_silent({
    res <- join_metrics(
      data=simulations_df,
      id_cols=c("idx", "idx2"),
      metrics=c("rejection", "coverage", "mse"),
      true_value=0,
      ll_col="conf.ll",
      ul_col="conf.ul",
      estimates_col="est",
      p_col="p_value",
    )
  })
  expect_equal(nrow(res), 20)
})
