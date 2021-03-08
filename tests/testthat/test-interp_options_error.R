test_that("interp.options test (error)", {

  expect_error( interp.options(interp.fun = "splinefun", length.out = 10,
                               test = "anything") )
})
