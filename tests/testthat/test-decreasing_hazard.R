test_that("running 'TTT_hazard_shape' (decreasing hazard)", {

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

  y <- rOW(n = 1000, mu = 0.1, sigma = 0.5, nu = 0.5)
  my_initial_guess <- TTT_hazard_shape(formula = y ~ 1)
  expect_equal( my_initial_guess$hazard_type, "Decreasing" )
})
