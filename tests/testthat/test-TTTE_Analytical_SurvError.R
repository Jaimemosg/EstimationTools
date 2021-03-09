test_that("running 'TTTE_Analytical' (character x)", {

  expect_error(TTTE_Analytical(time ~ x, method = "cens", data = data))
})
