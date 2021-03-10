test_that("plot.empiricalTTT", {

  set.seed(123)
  y <- rweibull(n=1000, shape=2, scale=pi)
  TTT_3 <- TTTE_Analytical(y ~ 1, scaled = FALSE)
  empTTT <- function() plot(TTT_3, type = "s", grid = TRUE,
                            col = 3, lwd = 3)
  # testthat::skip()
  vdiffr::expect_doppelganger("TTT-plot", empTTT)
})
