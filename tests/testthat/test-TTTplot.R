test_that("plot.empiricalTTT", {

  set.seed(123)
  y <- rweibull(n=50, shape=2, scale=pi)
  TTT_3 <- TTTE_Analytical(y ~ 1, scaled = FALSE)
  vdiffr::expect_doppelganger("TTT plot", plot(TTT_3, type = "s",
                                               col = 3, lwd = 3))
})
