test_that("interp.options test", {

  myoptions <- interp.options(interp.fun = "splinefun", length.out = 10,
                              method = "natural")
  y <- rweibull(n = 50, shape = 2.5, scale = pi)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1,
                                       interpolation = myoptions)
  expect_equal( my_initial_guess$hazard_type, "Increasing" )
})

