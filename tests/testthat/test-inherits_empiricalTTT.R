test_that("running 'maxlogLreg'", {

   TTT_1 <- TTTE_Analytical(Surv(stop, event == 'pcm') ~1, method = 'cens',
                            data = mgus1, subset=(start == 0))
  expect_equal( is.EmpiricalTTT(TTT_1), TRUE )

})
