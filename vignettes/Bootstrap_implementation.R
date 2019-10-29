## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  if (!require('devtools')) install.packages('devtools')
#  devtools::install_github('Jaimemosg/EstimationTools', force = TRUE)

## ------------------------------------------------------------------------
norm_reg2 <- maxlogLreg(formulas, y_dist = y ~ dnorm,
                        link = list(over = "sd", fun = "log_link"))

# Bootstrap for regression model
summary(norm_reg2, Boot_Std_Err = TRUE, R = 1000)

## ------------------------------------------------------------------------
## Hessian
print(norm_reg2$fit$hessian)

## Standard errors
print(norm_reg2$outputs$StdE)
print(norm_reg2$outputs$StdE_Method)

## ----echo=FALSE----------------------------------------------------------
e <- norm_reg2$outputs$StdE

