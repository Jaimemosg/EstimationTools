test_that("plot.HazardShape", {

  set.seed(456)
  y <- rweibull(n = 10000, shape = 2.5, scale = pi)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1)
  TTTplot <- function() plot(my_initial_guess)
  # testthat::skip()
  vdiffr::expect_doppelganger("TTT-for-Hazard", TTTplot)
})
