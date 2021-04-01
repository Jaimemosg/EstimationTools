test_that("running 'ga' in maxlogL", {

  mu <- 1.26;   sigma <- 0.12
  n <- 1000
  set.seed(258)
  library(gamlss.dist)
  v <- rZIP(n, mu=mu, sigma=sigma)
  fit <- maxlogL(v, dist = 'dZIP', optimizer = 'ga',
                 lower = c(0,0), upper = c(4,1),
                 control = list(monitor = FALSE))
  expect_snapshot_output( summary(fit) )
})
