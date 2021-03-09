test_that("running 'TTTE_Analytical' (character x)", {

  data <- aml
  data$x <- as.character(data$x)
  TTT_A <- TTTE_Analytical(Surv(time, status) ~ x, method = "cens", data = data)
  TTT_B <- TTTE_Analytical(time ~ x, data = data)
  myans <- c(as.numeric(TTT_A$strata), as.numeric(TTT_B$strata))
  expect_equal( myans, c(10,10,11,12) )
})
