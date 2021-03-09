test_that("running 'TTTE_Analytical' (factor x)", {

  TTT_2 <- TTTE_Analytical(Surv(time, status) ~ x, method = "cens", data = aml)
  expect_equal( as.numeric(TTT_2$strata), c(10,10) )
})
