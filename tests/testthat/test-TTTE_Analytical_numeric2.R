test_that("running 'TTTE_Analytical' (numeric x, v2)", {

  qOW <- function(p, mu, sigma, nu, lower.tail=TRUE, log.p = FALSE){
    if (any(mu<=0 ))
      stop(paste("mu must be positive", "\n", ""))
    if (any(sigma*nu<=0))
      stop(paste("Product sigma*nu must be positive", "\n", ""))
    if (log.p == TRUE)
      p <- exp(p)
    else p <- p
    if (lower.tail == TRUE)
      p <- p
    else p <- 1 - p
    if (any(p < 0) | any(p > 1))
      stop(paste("p must be between 0 and 1", "\n", ""))
    q <- (1/mu)*(log1p( (p*(1-p)^(-1))^(1/nu) ))^(1/sigma)
    return(q)
  }
  rOW <- function(n, mu, sigma, nu){
    if(any(n<=0))
      stop(paste("n must be positive","\n",""))
    if (any(mu<=0 ))
      stop(paste("mu must be positive", "\n", ""))
    if (any(sigma*nu<=0))
      stop(paste("Product sigma*nu must be positive", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    r <- qOW(p, mu, sigma, nu)
    return(r)
  }
  x <- runif(200, 0, 10)
  nu <- 0.1 + 0.1*x
  y <- rOW(n=200, mu=0.05, sigma=2, nu=nu)

  TTT_5 <- TTTE_Analytical(y ~ x)
  expect_equal( as.numeric(TTT_5$strata), c(66,67,67) )
})
