test_that("running 'coef' maxlogLreg", {

  n <- 1000
  x <- runif(n = n, -5, 6)
  y <- rnorm(n = n, mean = -2 + 3 * x, sd = exp(2 + 0.3* x))
  norm_data <- data.frame(y = y, x = x)
  formulas <- list(sd.fo = ~ x, mean.fo = ~ x)

  norm_mod <- maxlogLreg(formulas, y_dist = y ~ dnorm, data = norm_data,
                         link = list(over = "sd", fun = "log_link"))

  a <- coef(norm_mod)
  expect_length( a, 2 )
})
