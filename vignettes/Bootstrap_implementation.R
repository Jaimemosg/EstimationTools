## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval=FALSE------------------------------------------------------
#  if (!require('devtools')) install.packages('devtools')
#  devtools::install_github('Jaimemosg/EstimationTools', force = TRUE)

## ----HessianOptim, warning=FALSE, message=FALSE-------------------------------
library(EstimationTools)

x <- rnorm(n = 10000, mean = 160, sd = 6)
theta_1 <- maxlogL(x = x, dist = 'dnorm', control = list(trace = 1),
                   link = list(over = "sd", fun = "log_link"),
                   fixed = list(mean = 160))
summary(theta_1)

## Hessian
print(theta_1$fit$hessian)

## Standard errors
print(theta_1$fit$StdE)
print(theta_1$outputs$StdE_Method)

## ----Hessian1, echo=FALSE-----------------------------------------------------
a <- theta_1$fit$StdE

## ----HessianBootstrap---------------------------------------------------------
# Bootstrap
theta_2 <- maxlogL(x = x, dist = 'dnorm', control = list(trace = 1),
                   link = list(over = "sd", fun = "log_link"),
                   fixed = list(mean = 160))
bootstrap_maxlogL(theta_2, R = 200)
summary(theta_2)

## Hessian
print(theta_2$fit$hessian)

## Standard errors
print(theta_2$fit$StdE)
print(theta_2$outputs$StdE_Method)

## ----Hessian2, echo=FALSE-----------------------------------------------------
b <- theta_2$fit$StdE

