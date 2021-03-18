test_that("TTT_hazard_shape EmpiricalTTT object", {

  set.seed(456)
  y <- rweibull(n = 10000, shape = 2.5, scale = pi)
  TTT_we <- TTTE_Analytical(y ~ 1)
  my_initial_guess <- TTT_hazard_shape(TTT_we)
  expect_snapshot_output( print(my_initial_guess) )
})
