context("inherits method for HazardShape")

test_that("running mean with constant x or position", {

  y <- rweibull(n = 50, shape = 2.5, scale = pi)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1)
  expect_equal( is.HazardShape(my_initial_guess), "HazardShape")

})
