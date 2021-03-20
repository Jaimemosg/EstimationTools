test_that("running 'coefMany' maxlogLreg", {

  x <- rnorm(n = 10000, mean = 160, sd = 6)
  theta_1 <- maxlogL(x = x, dist = 'dnorm',
                   link = list(over = "sd", fun = "log_link"),
                   fixed = list(mean = 160))

  a <- coefMany(theta_1, parameter = c('mean', 'sd'))
  expect_length( a, 2 )
})
