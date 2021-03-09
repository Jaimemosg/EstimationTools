test_that("loess.options test", {

  myoptions <- loess.options(span = 0.8, degree = 1)
  y <- rweibull(n = 10000, shape = 2.5, scale = pi)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1,
                                       local_reg = myoptions)
  expect_equal( my_initial_guess$hazard_type, "Increasing" )
})
