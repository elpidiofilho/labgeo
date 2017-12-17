context("Test pred_acc")

test_that("pred_acc r2 and RMSE are calculating correctly", {

  pred <- runif(100, min = -1, max = 1)
  obs <- runif(100, min = -1, max = 1)
  est <- pred_acc(obs,pred)
  est$Nash_Sutcliffe_efficiency
  expect_equal(est$rsquared, cor(obs, pred)^2)
  expect_equal(est$root_mean_square_error, sqrt(mean((pred - obs)^2)))

})

test_that("pred_acc NSE are calculating correctly", {
  set.seed(313)
  pred <- runif(100, min = -1, max = 1)
  obs <- runif(100, min = -1, max = 1)
  r <- -1.462764
  est <- pred_acc(obs = obs, pred = pred)
  expect_equal(round(est$Nash_Sutcliffe_efficiency,3), round(r,3))
})
