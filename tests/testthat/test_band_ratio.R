context("Test band_Ratio")

test_that("band_ratio are calculating correctly", {
  v1 <- rep(8,100)
  v2 <- rep(2,100)
  v3 <- rep(1,100)
  df <- data.frame(v1,v2,v3)
  dfb <- band_ratio(df)
  expect_equal(round(dfb$b_v1_v2[1]), round(-0.6))
  expect_equal(round(dfb$b_v1_v3[1]), round(-0.778))
  expect_equal(round(dfb$b_v2_v3[1]), round(-0.333))
})
