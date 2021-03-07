test_that("running 'TTT_hazard_shape'", {

  y <- rweibull(n = 50, shape = 2.5, scale = pi)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1, silent = TRUE)
  expect_equal( is.HazardShape(my_initial_guess), TRUE )

})
