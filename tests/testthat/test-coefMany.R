test_that("running 'coefMany' maxlogLreg", {

  n <- 1000
  x <- runif(n = n, -5, 6)
  y <- rnorm(n = n, mean = -2 + 3 * x, sd = exp(1 + 0.3* x))
  norm_data <- data.frame(y = y, x = x)
  formulas <- list(sd.fo = ~ x, mean.fo = ~ x)

  norm_mod <- maxlogLreg(formulas, y_dist = y ~ dnorm, data = norm_data,
                         link = list(over = "sd", fun = "log_link"))

  a <- coefMany(norm_mod, parameter = c('mean', 'sd'))
  b <- coefMany(norm_mod)
  expect_equal( identical(a, b), TRUE )
})
