test_that("loess.options test (error)", {

  expect_error(loess.options(span = 0.8, subset = 1))
})
