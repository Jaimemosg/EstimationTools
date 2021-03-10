test_that("plot.HazardShape", {

  set.seed(123)
  y <- rweibull(n = 1000, shape = 2.5, scale = pi)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1)
  vdiffr::expect_doppelganger("TTT for Hazard", plot(my_initial_guess,
                                        par_plot=list(mar=c(3.7,3.7,1,1.5),
                                        mgp=c(2.5,1,0))))
})
