test_that("running 'nlminb' in maxlogL", {

  mu <- 1.26;   sigma <- 0.12
  n <- 1000
  set.seed(258)
  library(gamlss.dist)
  v <- rZIP(n, mu=mu, sigma=sigma)
  fit <- maxlogL(v, dist = 'dZIP',
                 link = list(over = c('mu','sigma'),
                             fun = c('log_link', 'logit_link')))
  expect_snapshot_output( summary(fit) )
})
