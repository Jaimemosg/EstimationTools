test_that("running 'coef' maxlogLreg", {

  x <- rnorm(n = 10000, mean = 160, sd = 6)
  theta_1 <- maxlogL(x = x, dist = 'dnorm',
                   link = list(over = "sd", fun = "log_link"),
                   fixed = list(mean = 160))
  a <- coef(theta_1)
  expect_length( a, 1 )
})
