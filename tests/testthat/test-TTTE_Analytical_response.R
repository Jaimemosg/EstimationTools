test_that("running 'TTTE_Analytical' (response argument)", {

  y <- rweibull(n=20, shape=1, scale=pi)
  TTT_4 <- TTTE_Analytical(response = y, scaled = FALSE)
  expect_equal( names(TTT_4$strata), "SingleGroup" )
})
